module type Entity = sig
  type t
  val symbol : string
  val name : string
  val fg : t -> Notty.A.color
  val to_string : t -> string
  val random : int -> t
  val draw : t -> Notty.image
end
