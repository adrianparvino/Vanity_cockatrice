let rec base32 bytes length = 
  ignore(bytes, length)

let rec generate_hash deck =
  let x = Deck.spread deck in
  ignore(x)
