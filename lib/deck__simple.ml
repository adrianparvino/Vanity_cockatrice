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

type t = { maindeck : Int.t StringMap.t; sideboard : Int.t StringMap.t }

module type Parser = sig
  val parse : string -> t
end

module SBParser : sig
  val parse : string -> t
end = struct
  let regex =
    let open Re in
    let br = group (opt (seq [ str "SB:"; rep1 space ])) in
    let count = seq [ group (rep1 digit); rep1 space ] in
    let name = group (rep1 notnl) in
    compile (seq [ bol; rep space; br; count; name; eol ])

  let parse_line line =
    let group = Re.exec_opt regex line in
    Option.bind group (fun group ->
        match Re.Group.all group with
        | [| _; sb; count; name |] ->
            Some (String.starts_with ~prefix:"SB:" sb, int_of_string count, name)
        | _ -> None)

  let parse s =
    let lines = String.split_on_char '\n' s in
    let rec run deck = function
      | [] -> deck
      | line :: lines -> (
          match parse_line line with
          | None -> run deck lines
          | Some (false, count, name) ->
              let deck' =
                { deck with maindeck = StringMap.add name count deck.maindeck }
              in
              run deck' lines
          | Some (true, count, name) ->
              let deck' =
                {
                  deck with
                  sideboard = StringMap.add name count deck.sideboard;
                }
              in
              run deck' lines)
    in
    run { maindeck = StringMap.empty; sideboard = StringMap.empty } lines
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
    { maindeck; sideboard }
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

  let rec hash_maindeck_leading (ctx : SHA1.ctx) maindeck : string =
    match Seq.uncons maindeck with
    | None -> base32 ctx
    | Some ((card, n), maindeck) ->
        let rec go ctx = function
          | 0 -> ctx
          | n ->
              let ctx = SHA1.feed_string ctx ";" in
              let ctx = SHA1.feed_string ctx card in
              go ctx (n - 1)
        in
        let ctx = go ctx n in
        hash_maindeck_leading ctx maindeck

  let hash_maindeck (ctx : SHA1.ctx) maindeck : string =
    match Seq.uncons maindeck with
    | None -> base32 ctx
    | Some ((card, n), maindeck) ->
        let ctx = SHA1.feed_string ctx card in
        let maindeck = Seq.cons (card, n - 1) maindeck in
        hash_maindeck_leading ctx maindeck

  let rec hash_sideboard_leading (ctx : SHA1.ctx) maindeck sideboard : string =
    match Seq.uncons sideboard with
    | None -> hash_maindeck_leading ctx maindeck
    | Some ((card, n), sideboard) ->
        let rec go ctx = function
          | 0 -> ctx
          | n ->
              let ctx = SHA1.feed_string ctx ";" in
              let ctx = SHA1.feed_string ctx "SB:" in
              let ctx = SHA1.feed_string ctx card in
              go ctx (n - 1)
        in
        let ctx = go ctx n in
        hash_sideboard_leading ctx maindeck sideboard

  let hash_sideboard (ctx : SHA1.ctx) maindeck sideboard : string =
    match Seq.uncons sideboard with
    | None -> hash_maindeck ctx maindeck
    | Some ((card, n), sideboard) ->
        let ctx = SHA1.feed_string ctx "SB:" in
        let ctx = SHA1.feed_string ctx card in
        let sideboard = Seq.cons (card, n - 1) sideboard in
        hash_sideboard_leading ctx maindeck sideboard

  let hash deck =
    let ctx = SHA1.empty in
    hash_sideboard ctx
      (StringMap.to_seq deck.maindeck)
      (StringMap.to_seq deck.sideboard)
end

let hash = Hash.hash
