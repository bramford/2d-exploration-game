(** Item creation functors and signatures **)
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

module Make(P: P) : S = struct
  type t = {
    weight: int
  }

  let create ~weight = {
    weight
  }

  let name = P.name

  let symbol = P.symbol

  let fg = Notty.A.white

  let draw =
    Notty.I.string (Notty.A.fg (fg)) symbol

  let to_string r =
    name ^ "{weight:" ^ (string_of_int(r.weight)) ^ "}"
end

