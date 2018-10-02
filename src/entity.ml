module Tree = Entity_tree
module Human = Entity_human

type t =
  | Human of Human.t
  | Tree of Tree.t

let to_string = function
  | Some Human r -> Human.to_string r
  | Some Tree r -> Tree.to_string r
  | None -> "None"

let fg = function
  | Human r -> Human.fg r
  | Tree r -> Tree.fg r

let draw = function
  | Some Human r -> Human.draw r
  | Some Tree r -> Tree.draw r
  | None -> Notty.I.char Notty.A.empty ' ' 1 1

let is_human = function
  | Some Human _ -> true
  | _ -> false

let make_player n = function
  | Some Human r ->
    begin match (Human.make_player r n) with
    | Ok h -> Ok (Human h)
    | Error s -> Error s
    end
  | _ -> Error "Can't be a player"

let is_player = function
  | Some Human r -> Human.is_player r
  | _ -> false

let is_specific_player n = function
  | Some Human r -> Human.is_specific_player r n
  | _ -> false

let random n =
  let m = n mod 1000 in
  if m == 0 then
    Some (Human (Human.random n))
  else if m > 1 && m < 100 then
    Some (Tree (Tree.random n))
  else None
