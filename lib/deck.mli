module type Deck = sig
  type t 

  module type Parser = sig
    val parse : string -> t
  end

  module SBParser : Parser
  module MTGAParser : Parser

  val dump: t -> String.t List.t

  val add_maindeck: string -> t -> t
  val add_sideboard: string -> t -> t
  val popped_sideboard: t -> t list

  val hash : t -> String.t 
end

module Simple = Deck__simple
