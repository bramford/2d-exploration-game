type t = { name : string; }

val to_string : t option -> string

val create : name:string -> t option
