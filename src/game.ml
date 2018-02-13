type human_attr_nature =
  | Good
  | Neutral
  | Bad

type human_attr = {
  age: int;
  nature: human_attr_nature;
}

type tree_attr_breed =
  | Pine
  | Birch

type tree_attr = {
  age: int;
  breed: tree_attr_breed;
}

type entity =
  | Human of human_attr
  | Tree of tree_attr

let create_human ~age ~nature =
  { age;
    nature;
  }

let create_tree ~age ~breed =
  { age;
    breed;
  }

let tree_fg tree =
    match tree.breed with
      | Pine -> Notty.A.green
      | Birch -> Notty.A.lightgreen

let human_fg human =
    match human.nature with
      | Good -> Notty.A.white
      | Neutral -> Notty.A.yellow
      | Bad -> Notty.A.red

let entity_fg e = function
  | Human e -> human_fg e
  | Tree e -> tree_fg e

let myhuman =
  create_human ~age:1 ~nature:Good

let mytree =
  create_tree ~age:1000 ~breed:Pine
