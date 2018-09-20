type breed =
  | Pine
  | Birch

type t = {
  age: int;
  breed: breed;
}

let symbol = "t"

let create ~age ~breed =
  { age;
    breed;
  }

let to_string r =
  let string_of_breed breed =
    match breed with
    | Pine -> "Pine"
    | Birch -> "Birch"
  in
  "{Tree{breed:" ^ (string_of_breed r.breed) ^ ",age:" ^  (string_of_int r.age) ^ "}}"

let fg r =
  match r.breed with
  | Pine -> Notty.A.green
  | Birch -> Notty.A.cyan

let draw r =
  Notty.I.string (Notty.A.fg (fg r)) symbol

let breed_of_int i =
  let m = i mod 3 in
  match m with
  | 1 -> Birch
  | _ -> Pine

let random n =
  let breed = breed_of_int n in
  let age = n mod 100 + 1 in
  create
    ~age:age
    ~breed:breed
