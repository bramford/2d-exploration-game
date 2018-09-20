type t = { weight : int; }
val create : weight:int -> t
val symbol : string
val fg : 'a -> Notty.A.color
val draw : 'a -> Notty.image
val to_string : t -> string
