type t
val to_string : t option -> string
val fg : t -> Notty.A.color
val draw : t option -> Notty.image
val is_human : t option -> bool
val make_player : string -> t option -> (t, string) result
val is_player : t option -> bool
val is_specific_player : string -> t option -> bool
val random : int -> t option
