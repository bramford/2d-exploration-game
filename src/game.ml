open Core

module Entity = struct
  module Human = struct
    type nature =
      | Good
      | Neutral
      | Bad

    type t = {
      age: int;
      nature: nature;
    }

    let create ~age ~nature =
      { age;
        nature;
      }

    let fg human =
      match human.nature with
        | Good -> Notty.A.white
        | Neutral -> Notty.A.yellow
        | Bad -> Notty.A.red

    let nature_of_int i =
      match i with
      | 1 -> Neutral
      | 2 -> Bad
      | _ -> Good

    let random =
      let nature =
        Random.int 100 mod 2
        |> nature_of_int
      in
      let age = Random.int 20 + 13 in
      create
        ~age:age
        ~nature:nature
  end

  module Tree = struct
    type breed =
      | Pine
      | Birch

    type t = {
      age: int;
      breed: breed;
    }

    let create ~age ~breed =
      { age;
        breed;
      }

    let fg tree =
      match tree.breed with
        | Pine -> Notty.A.green
        | Birch -> Notty.A.lightgreen

    let breed_of_int i =
      match i with
      | 1 -> Birch
      | _ -> Pine

    let random =
      let breed =
        Random.int 100 mod 1
        |> breed_of_int
      in
      let age = Random.int 1000 + 1 in
      create
        ~age:age
        ~breed:breed
  end

  type t =
    | Human of Human.t
    | Tree of Tree.t

  let fg e = function
    | Human e -> Human.fg e
    | Tree e -> Tree.fg e

  let random =
    match Random.int 10 with
    | 0 -> Human Human.random
    | _ -> Tree Tree.random


  let gen_list i =
    let rec gl i l =
      match l with
      | [] | [_] -> l
      | hd :: tl -> gl (i - 1) (random :: l)
    in
    gl i []
end

module World = struct
  type coord = (int * int)

  type t = {
    coords : coord list;
    entities : Entity.t list;
  }

  let create w h =
    let rec coords_of_bounds x x_max y y_max l =
      let l = if y <= y_max then
        coords_of_bounds x x_max (y + 1) y_max ((x, y) :: l)
      else l in
      let l = if x <= x_max then
        coords_of_bounds (x + 1) x_max y y_max ((x, y) :: l)
      else l in
      l
    in
    let coords = coords_of_bounds 0 w 0 h [] in
    let entities = Entity.gen_list (w * h) in
    { coords = coords;
      entities = entities;
    }
end

let myworld =
  World.create 64 64
