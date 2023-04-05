module StringSet = Set.Make (String)
module Deck = Vanity_cockatrice.Deck
module T = Domainslib.Task
module Mpsc = Lockfree.Mpsc_queue
module Q = Lockfree.Ws_deque.M

module Result_channel : sig
  type t
  type signal = Count | Duplicate | Deck of Deck.t

  val create : unit -> t
  val signal : signal -> t -> unit
  val wait : t -> signal
end = struct
  type signal = Count | Duplicate | Deck of Deck.t
  type t = { queue : signal Mpsc.t; semaphore : Semaphore.Counting.t }

  let create () =
    let queue = Mpsc.create () in
    let semaphore = Semaphore.Counting.make 0 in
    { queue; semaphore }

  let signal signal { queue; semaphore } =
    Mpsc.push queue signal;
    Semaphore.Counting.release semaphore;
    ()

  let wait { queue; semaphore } =
    Semaphore.Counting.acquire semaphore;
    Option.get (Mpsc.pop queue)
end

let deck =
  Vanity_cockatrice.Deck.MTGAParser.parse
    {|
4 Kodama of the West Tree
4 Defiler of Vigor
4 Evolving Adaptive
1 Bloated Contaminator
4 Llanowar Loamspeaker
1 Boseiju, Who Endures
4 Reckoner Bankbuster
1 Ascendant Packleader
4 Brushland
3 Jewel Thief
4 Overgrown Farmland
4 Thrun, Breaker of Silence
4 Razorverge Thicket
2 Mishra's Foundry
4 Quirion Beastcaller
8 Forest
4 Valorous Stance

3 Tyvar's Stand
2 Ulvenwald Oddity
2 Gaea's Gift
3 Ossification
3 Destroy Evil
2 Silverback Elder
|}

let names =
  let lexer = Yojson.init_lexer () in
  let file = open_in Sys.argv.(2) in
  let lexbuf = Lexing.from_channel file in
  Oracle_j.read_names lexer lexbuf |> Array.of_list

let rec mine ~finished ~task_queue ~result_channel ~prefix ~semaphore =
  match Atomic.get finished with
  | true -> ()
  | false ->
      Semaphore.Counting.release semaphore;
      let deck = Q.steal task_queue in
      names
      |> Array.iter (fun card ->
             Result_channel.(signal Count result_channel);
             let deck = deck |> Deck.add_sideboard card in
             let hash = Deck.hash deck in
             if String.starts_with ~prefix hash then
               Result_channel.(signal (Deck deck) result_channel));
      mine ~finished ~task_queue ~result_channel ~prefix ~semaphore

let () =
  let prefix = Sys.argv.(1) in
  let finished = Atomic.make false in
  let result_channel = Result_channel.create () in
  let task_queue = Q.create () in
  let semaphore = Semaphore.Counting.make 10000000 in
  let enqueuer =
    Domain.spawn (fun _ ->
        deck |> Deck.popped_sideboard
        |> List.iter (fun deck ->
               deck |> Deck.popped_sideboard
               |> List.iter (fun deck ->
                      names
                      |> Array.iter (fun card ->
                             Semaphore.Counting.acquire semaphore;
                             let deck = deck |> Deck.add_sideboard card in
                             Q.push task_queue deck))))
  in
  let domains =
    List.init 16 (fun i ->
        Domain.spawn (fun _ ->
            mine ~finished ~task_queue ~result_channel ~prefix ~semaphore))
  in
  let rec run count last_duplicates duplicates = function
    | 1 -> Atomic.set finished true
    | n ->
        let open Format in
        let count, last_duplicates, duplicates, n =
          match Result_channel.wait result_channel with
          | Result_channel.Count ->
              if count mod 10000 = 0 then (
                fprintf std_formatter
                  "@[count: %d, duplicates: %d, delta: %d @." count duplicates
                  (duplicates - last_duplicates);
                (count + 1, duplicates, duplicates, n))
              else (count + 1, last_duplicates, duplicates, n)
          | Result_channel.Duplicate ->
              (count, last_duplicates, duplicates + 1, n)
          | Result_channel.Deck deck ->
              let dump = Deck.dump deck in
              fprintf std_formatter "@[<v>";
              let hash = Deck.hash deck in
              fprintf std_formatter "@[%s (%d):@]@;" hash (List.length dump);
              fprintf std_formatter "@[<v>";
              pp_print_list
                (fun ppf s -> Format.fprintf ppf "@[%s@;@]" s)
                std_formatter dump;
              fprintf std_formatter "@]@]@.";
              (count, last_duplicates, duplicates, n + 1)
        in
        run count last_duplicates duplicates n
  in
  run 0 0 0 0;
  Domain.join enqueuer;
  List.iter Domain.join domains
