module Rock = Item_rock
type t =
  | Rock of Rock.t

let draw = function
  | Some Rock r -> Rock.draw r
  | None -> Notty.I.char Notty.A.empty ' ' 1 1

let to_string = function
  | Some Rock r -> Rock.to_string r
  | None -> "None"

let random i =
  if i mod 1000 < 11 then
    Some (Rock (Rock.create ~weight:((Random.int 4) + 1)))
  else None
