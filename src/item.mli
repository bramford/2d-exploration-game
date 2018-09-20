module Rock = Item_rock
type t =
  | Rock of Rock.t
val draw : t option -> Notty.image
val to_string : t option -> string
val random : int -> t option
