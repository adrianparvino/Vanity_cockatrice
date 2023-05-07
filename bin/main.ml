module Deck = Vanity_cockatrice.Deck.CachedMain
module StringSet = Set.Make (String)
module T = Domainslib.Task
module Mpsc = Lockfree.Mpsc_queue
module Q = Lockfree.Ws_deque.M

module type Result_channel = sig
  type t
  type signal = Count | Duplicate | Deck of Deck.t

  val create : unit -> t
  val signal : signal -> t -> unit
  val wait : t -> signal
end

module Result_channel : Result_channel = struct
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
  Deck.MTGAParser.parse
    {|
4 Evolving Adaptive
4 Kumano Faces Kakkazan
2 Shivan Devastator
4 Armored Scrapgorger
3 Quirion Beastcaller
2 Arbalest Engineers
4 Bloated Contaminator
3 Kodama of the West Tree
1 Migloz, Maze Crusher
1 Halana and Alena, Partners
3 Thundering Raiju
4 Play with Fire
4 Kami's Flare
1 Lukka, Bound to Ruin
6 Forest
5 Mountain
4 Karplusan Forest
3 Copperline Gorge
1 Boseiju, Who Endures
1 Sokenzan, Crucible of Defiance

2 Strangle
3 Abrade
3 Return to Nature
1 Chandra, Dressed to Kill
4 Rending Flame
2 Jaya, Fiery Negotiator
|}

let names =
  let lexer = Yojson.init_lexer () in
  let file = open_in Sys.argv.(2) in
  let lexbuf = Lexing.from_channel file in
  Oracle_j.read_names lexer lexbuf |> List.map String.lowercase_ascii

module type Miner = sig
  type t

  val run : string -> t
  val wait : t -> unit
end

module ParallelMiner : Miner = struct
  type t = {
    enqueuer : unit Domain.t;
    finished : bool Atomic.t;
    result_channel : Result_channel.t;
    domains : unit Domain.t list;
    semaphore : Semaphore.Counting.t;
  }

  let rec mine ~finished ~task_queue ~result_channel ~prefix ~semaphore () =
    match Atomic.get finished with
    | true -> ()
    | false ->
        let deck = Q.steal task_queue in
        Semaphore.Counting.release semaphore;
        names
        |> List.iter (fun card ->
               Result_channel.(signal Count result_channel);
               let deck = deck |> Deck.add_sideboard card in
               let hash = Deck.hash deck in
               if String.starts_with ~prefix hash then
                 Result_channel.(signal (Deck deck) result_channel));
        mine ~finished ~task_queue ~result_channel ~prefix ~semaphore ()

  let rec run_enqueuer finished semaphore task_queue deck =
    let decks = Deck.popped_sideboard deck in
    let decks = List.map Deck.popped_sideboard decks |> List.flatten in
    let rec go = function
      | deck :: xs, ys -> (
          Semaphore.Counting.acquire semaphore;
          match Atomic.get finished with
          | true -> ()
          | false ->
              Q.push task_queue deck;
              go (xs, ys))
      | [], deck :: ys ->
          let xs =
            names |> List.map (fun card -> Deck.add_sideboard card deck)
          in
          go (xs, ys)
      | [], [] -> ()
    in
    go ([], decks)

  let run prefix =
    let semaphore = Semaphore.Counting.make 256 in
    let finished = Atomic.make false in
    let result_channel = Result_channel.create () in
    let task_queue = Q.create () in
    let enqueuer =
      Domain.spawn (fun _ -> run_enqueuer finished semaphore task_queue deck)
    in
    let domains =
      List.init 16 (fun _ ->
          Domain.spawn
            (mine ~finished ~task_queue ~result_channel ~prefix ~semaphore))
    in
    { finished; result_channel; enqueuer; domains; semaphore }

  let wait { finished; result_channel; enqueuer; domains; semaphore } =
    let rec run count last_duplicates duplicates = function
      | 1 ->
          Atomic.set finished true;
          Semaphore.Counting.release semaphore
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
end

module NonLoggingParallelMiner : Miner = struct
  module Tasks : sig
    type t

    val make : string array -> Deck.t -> t
    val pop : t -> Deck.t option
    val finish : t -> bool
  end = struct
    type t = {
      names : string array;
      base : Deck.t array;
      counter : int Atomic.t;
    }

    let make names base =
      let decks = Deck.popped_sideboard deck in
      let decks =
        List.map Deck.popped_sideboard decks |> List.flatten |> Array.of_list
      in
      let base = decks in
      let counter = Atomic.make 0 in
      let names = names |> Array.map String.lowercase_ascii in
      { names; base; counter }

    let pop { names; base; counter } =
      let i = Atomic.fetch_and_add counter 1 in
      let i, j = (i mod Array.length names, i / Array.length names) in
      if i < 0 || j >= Array.length base then None
      else Some (Deck.add_sideboard names.(i) base.(j))

    let finish { counter } =
      let i = Atomic.get counter in
      Atomic.compare_and_set counter i (-16)
  end

  type t = { finished : bool Atomic.t; domains : Deck.t option Domain.t list }

  let rec mine ~scratch ~tasks ~finished ~prefix () =
    match Tasks.pop tasks with
    | None -> None
    | Some deck -> (
        let result =
          names
          |> List.find_map (fun card ->
                 let deck = deck |> Deck.add_sideboard card in
                 let hash = Deck.hash_bytes deck scratch in

                 if String.starts_with ~prefix hash && Tasks.finish tasks then
                   Some deck
                 else None)
        in
        match result with
        | None -> mine ~scratch ~tasks ~finished ~prefix ()
        | x -> x)

  let run prefix =
    let finished = Atomic.make false in
    let tasks = Tasks.make (Array.of_list names) deck in
    let domains =
      (List.init [@inlined never]) 16 (fun _ ->
          Domain.spawn
            (let scratch = Bytes.create 2048 in
             mine ~scratch ~tasks ~finished ~prefix))
    in
    { finished; domains }

  let wait { finished; domains } =
    let results = List.map Domain.join domains in
    match List.find_map Fun.id results with
    | Some deck ->
        let open Format in
        let dump = Deck.dump deck in
        let hash = Deck.hash deck in
        fprintf std_formatter "@[<v>";
        fprintf std_formatter "@[%s (%d):@]@;" hash (List.length dump);
        fprintf std_formatter "@[<v>";
        pp_print_list
          (fun ppf s -> Format.fprintf ppf "@[%s@;@]" s)
          std_formatter dump;
        fprintf std_formatter "@]@]@."
    | None -> ()
end

module STMiner = struct
  type t = unit

  let rec mine ~finished ~task_queue ~result_channel ~prefix ~semaphore () =
    match Atomic.get finished with
    | true -> ()
    | false ->
        Semaphore.Counting.release semaphore;
        let deck = Q.steal task_queue in
        names
        |> List.iter (fun card ->
               Result_channel.(signal Count result_channel);
               let deck = deck |> Deck.add_sideboard card in
               let hash = Deck.hash deck in
               if String.starts_with ~prefix hash then
                 Result_channel.(signal (Deck deck) result_channel));
        mine ~finished ~task_queue ~result_channel ~prefix ~semaphore ()

  let run prefix =
    deck |> Deck.popped_sideboard
    |> Fun.flip List.fold_left 0 (fun counter deck ->
           deck |> Deck.popped_sideboard
           |> Fun.flip List.fold_left counter (fun counter deck ->
                  names
                  |> Fun.flip List.fold_left counter (fun counter card ->
                         let deck = deck |> Deck.add_sideboard card in
                         names
                         |> Fun.flip List.fold_left counter (fun counter card ->
                                let deck = deck |> Deck.add_sideboard card in
                                (if counter mod 10000 = 0 then
                                   let open Format in
                                   fprintf std_formatter "@[%d@]@." counter);
                                let hash = Deck.hash deck in
                                (if String.starts_with ~prefix hash then
                                   let open Format in
                                   fprintf std_formatter "@[%s@]@." hash);
                                counter + 1))))
    |> ignore

  let wait () = ()
end

let () =
  let prefix = Sys.argv.(1) in
  let module Miner = NonLoggingParallelMiner in
  let miner = Miner.run prefix in
  Miner.wait miner
