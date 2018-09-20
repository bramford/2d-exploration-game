type coord = int * int
type node = { entity : Entity.t option; items : Items.t; }
type cell = coord * node
type t = { size : int * int; cells : cell list; }
type move_direction = Left | Right | Up | Down
val node_create : entity:Entity.t option -> items:Items.t -> node
val find_player : ('a * node) list -> string -> ('a * node, string) result
val create : int -> int -> t
val draw : t -> Notty.image
val draw_around_player : t -> string -> Notty.image
val calc_move_coord :
  int * int -> move_direction -> int -> int -> (int * int) option
val update_cell :
  ('a * 'b) * 'c -> (('a * 'b) * 'c) list -> (('a * 'b) * 'c) list
val move_player : t -> string -> move_direction -> t
val add_player : t -> string -> (t, string) result
val render : Notty_unix.Term.t -> Notty.image -> unit
val print_cell : (int * int) * node -> unit
val print : t -> unit
