module type P = sig
  val symbol : string
  val name : string
end

module type S = sig
  type t = { weight : int; }
  val create : weight:int -> t
  val symbol : string
  val name : string
  val fg : Notty.A.color
  val draw : Notty.image
  val to_string : t -> string
end

module Make : functor (P : P) -> S
