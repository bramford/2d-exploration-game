
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



let myhuman =
  Entity.Human.create ~age:1 ~nature:Good

let mytree =
  Entity.Tree.create ~age:1000 ~breed:Pine
