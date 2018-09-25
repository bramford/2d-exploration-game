module Rock : Item_lib.S
module Stick : Item_lib.S

type t =
  | Rock of Rock.t
  | Stick of Stick.t

val draw : t option -> Notty.image
val to_string : t option -> string
val random : int -> t option
