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
  end

  type entity =
    | Human of Human.t
    | Tree of Tree.t

  let entity_fg e = function
    | Human e -> Human.fg e
    | Tree e -> Tree.fg e
end

module World = struct
  type coord = (int * int)

  type t = {
    map : coord List.t
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
    { map = coords;
    }
end

let myworld =
  World.create 64 64

let myhuman =
  Entity.Human.create ~age:1 ~nature:Good

let mytree =
  Entity.Tree.create ~age:1000 ~breed:Pine
