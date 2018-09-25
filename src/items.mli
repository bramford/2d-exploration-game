type t = Item.t option list
val random : int -> Item.t option list
val draw : Item.t option list -> Notty.image
val to_string : Item.t option list -> string
