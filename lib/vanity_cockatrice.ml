module Deck = Deck
open Digestif

let base32 (digest : SHA1.t) =
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

(* let hash = function
   | [] -> SHA1.empty |> SHA1.get
   | card :: cards ->
       let ctx = SHA1.empty in
       let ctx = SHA1.feed_string ctx card in
       let ctx =
         List.fold_left
           (fun ctx card ->
             let ctx = SHA1.feed_string ctx ";" in
             let ctx = SHA1.feed_string ctx card in
             ctx)
           ctx cards
       in
       SHA1.get ctx *)

let hash cards = SHA1.digest_string (String.concat ";" cards)

let generate_hash deck =
  let cards = Deck.dump deck in
  let digest = hash cards in
  let base32 = base32 digest in
  base32
