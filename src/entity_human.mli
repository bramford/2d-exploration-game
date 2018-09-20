type nature = Good | Neutral | Bad
type t = {
  age : int;
  nature : nature;
  inventory : Items.t;
  player : Player.t option;
}
val symbol : string
val create :
  age:int ->
  nature:nature -> player:Player.t option -> inventory:Items.t -> t
val to_string : t -> string
val fg : t -> Notty.A.color
val draw : t -> Notty.image
val nature_of_int : int -> nature
val make_player : t -> string -> (t, string) result
val is_player : t -> bool
val is_specific_player : t -> string -> bool
val random : int -> t
