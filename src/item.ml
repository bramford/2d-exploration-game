module Rock = Item_lib.Make(struct
    let symbol = "."
    let name = "Rock"
  end)

module Stick = Item_lib.Make(struct
    let symbol = "-"
    let name = "Stick"
  end)

type t =
  | Rock of Rock.t
  | Stick of Stick.t

let draw = function
  | Some Rock _ -> Rock.draw
  | Some Stick _ -> Stick.draw
  | None -> Notty.I.char Notty.A.empty ' ' 1 1

let to_string = function
  | Some Rock r -> Rock.to_string r
  | Some Stick r -> Stick.to_string r
  | None -> "None"

let random i =
  match i mod 1000 with
  | x when x < 5 ->
    Some (Rock (Rock.create ~weight:((Random.int 4) + 1)))
  | x when (x > 5 && x < 11) ->
    Some (Stick (Stick.create ~weight:((Random.int 2) + 1)))
  | _ -> None
