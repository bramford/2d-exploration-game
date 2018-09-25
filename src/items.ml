type t = Item.t option list

let random i =
  let rec r n l =
    if n = 0 then l
    else r (n - 1) ((Item.random i) :: l)
  in
  r (Random.int 5) []

let draw l =
  match l with
  | [] | [_] -> Item.draw None
  | hd :: _ -> Item.draw hd

let to_string l =
  let rec ts acc l s se =
    match l with
    | [] | [_] -> s ^ se
    | hd :: tl ->
      let sep = if acc > 0 then "," else "" in
      ts (acc + 1) tl (s ^ sep ^ (Item.to_string hd)) se
  in
  ts 0 l "[" "]"
