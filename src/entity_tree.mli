type breed = Pine | Birch
type t = { age : int; breed : breed; }
val symbol : string
val create : age:int -> breed:breed -> t
val to_string : t -> string
val fg : t -> Notty.A.color
val draw : t -> Notty.image
val breed_of_int : int -> breed
val random : int -> t
