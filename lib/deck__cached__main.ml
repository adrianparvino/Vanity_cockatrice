module StringMap = struct
  module M = Map.Make (String)

  type 'a t = 'a M.t

  let bindings = M.bindings
  let empty = M.empty
  let fold = M.fold
  let to_seq = M.to_seq
  let add key data m = M.add (String.lowercase_ascii key) data m
  let update key f m = M.update (String.lowercase_ascii key) f m
end

type t = {
  maindeck : Int.t StringMap.t;
  sideboard : Int.t StringMap.t;
  cached_maindeck : String.t;
}

module type Parser = sig
  val parse : string -> t
end

module MTGAParser : sig
  val parse : string -> t
end = struct
  let regex =
    let open Re in
    let count = seq [ group (rep1 digit); rep1 space ] in
    let name = group (rep1 notnl) in
    compile (seq [ bol; rep space; count; name; eol ])

  let parse_line line =
    let group = Re.exec_opt regex line in
    Option.bind group (fun group ->
        match Re.Group.all group with
        | [| _; count; name |] -> Some (int_of_string count, name)
        | _ -> None)

  let parse s =
    let rec trim_empty_lines = function
      | [] -> []
      | line :: lines -> (
          match String.trim line with
          | "" -> trim_empty_lines lines
          | _ -> line :: lines)
    in
    let lines = String.split_on_char '\n' s |> trim_empty_lines in
    let rec parse_block deck = function
      | [] -> (deck, [])
      | line :: lines -> (
          match String.trim line with
          | "" -> (deck, lines)
          | line -> (
              match parse_line line with
              | None -> parse_block deck lines
              | Some (count, name) ->
                  let deck' = StringMap.add name count deck in
                  parse_block deck' lines))
    in

    let maindeck, lines = parse_block StringMap.empty lines in
    let sideboard, _ = parse_block StringMap.empty lines in
    let cached_maindeck =
      StringMap.to_seq maindeck
      |> Seq.flat_map (fun (s, n) -> Seq.init n (Fun.const s))
      |> List.of_seq |> String.concat ";"
    in
    { maindeck; sideboard; cached_maindeck }
end

let dump deck =
  let output = [] in
  let output =
    StringMap.fold
      (fun card n output ->
        List.init n (Fun.const (String.lowercase_ascii card)) @ output)
      deck.maindeck output
  in
  let output =
    StringMap.fold
      (fun card n output ->
        List.init n (Fun.const ("SB:" ^ String.lowercase_ascii card)) @ output)
      deck.sideboard output
  in
  List.sort String.compare output

let add_maindeck card ({ maindeck; _ } as deck) =
  let maindeck =
    StringMap.update card
      (function None -> Some 1 | Some x -> Some (x + 1))
      maindeck
  in
  { deck with maindeck }

let add_sideboard card ({ sideboard; _ } as deck) =
  let sideboard =
    StringMap.update card
      (function None -> Some 1 | Some x -> Some (x + 1))
      sideboard
  in
  { deck with sideboard }

let popped_sideboard ({ sideboard; _ } as deck) =
  StringMap.bindings sideboard
  |> List.map (fun (card, _) ->
         let sideboard =
           StringMap.update card
             (function None -> None | Some 1 -> None | Some n -> Some (n - 1))
             sideboard
         in
         { deck with sideboard })

module Hash : sig
  val hash : t -> string
end = struct
  module SHA1 = Digestif.SHA1

  let base32 (ctx : SHA1.ctx) =
    let digest = SHA1.get ctx in
    let alphabet = "0123456789abcdefghijklmnopqrstuvwxyz" in
    let bytes = SHA1.to_raw_string digest in
    let word = Int64.(shift_right_logical (String.get_int64_be bytes 0) 24) in
    let rec go state word = function
      | 0 -> state |> List.to_seq |> String.of_seq
      | n ->
          let i = Int64.to_int word land 0x1F in
          let state' = alphabet.[i] :: state in
          let word' = Int64.shift_right_logical word 5 in
          go state' word' (n - 1)
    in
    go [] word 8

  let rec hash_maindeck_leading (ctx : SHA1.ctx) cached_maindeck : string =
    let ctx = SHA1.feed_string ctx ";" in
    let ctx = SHA1.feed_string ctx cached_maindeck in
    base32 ctx

  let hash_maindeck (ctx : SHA1.ctx) cached_maindeck : string =
    let ctx = SHA1.feed_string ctx cached_maindeck in
    base32 ctx

  let rec hash_sideboard (ctx : SHA1.ctx) maindeck sideboard : string =
    let rec prepare_sideboard k = function
      | [] -> k []
      | (s, n) :: sb ->
          let rec go k = function
            | 0 -> k
            | n -> go (fun xs -> k (s :: xs)) (n - 1)
          in
          let k = go k n in
          prepare_sideboard k sb
    in
    let cached_sideboard =
      sideboard |> StringMap.bindings |> prepare_sideboard (String.concat ";SB:")
    in
    match cached_sideboard with
    | "" -> hash_maindeck ctx maindeck
    | sb ->
        let sb = "SB:" ^ sb in
        let ctx = SHA1.feed_string ctx sb in
        hash_maindeck_leading ctx maindeck

  let hash deck =
    let ctx = SHA1.empty in
    hash_sideboard ctx deck.cached_maindeck deck.sideboard
end

let hash = Hash.hash
