type t =
  { weight : int }

let create ~weight =
  { weight }

let symbol = "·"

let fg _ = Notty.A.white

let draw r =
  Notty.I.string (Notty.A.fg (fg r)) symbol

let to_string r =
  "Rock{weight:" ^ (string_of_int(r.weight)) ^ "}"
