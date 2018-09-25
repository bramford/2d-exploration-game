module Rock = Item_lib.Make(struct
    let symbol = "."
    let name = "Rock"
  end)

type t =
  | Rock of Rock.t

let draw = function
  | Some Rock _ -> Rock.draw
  | None -> Notty.I.char Notty.A.empty ' ' 1 1

let to_string = function
  | Some Rock r -> Rock.to_string r
  | None -> "None"

let random i =
  match i mod 1000 with
  | x when x < 5 ->
    Some (Rock (Rock.create ~weight:((Random.int 4) + 1)))
  | _ -> None
