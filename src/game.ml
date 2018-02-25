(*
#require "notty";;
#require "notty.unix";;
#require "notty.top";;
*)

open Notty.Infix

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

    let symbol = "@"

    let create ~age ~nature =
      { age;
        nature;
      }

    let to_string r =
      let string_of_nature nature =
        match nature with
        | Good -> "Good"
        | Neutral -> "Neutral"
        | Bad -> "Bad"
      in
      "Human {nature: " ^ (string_of_nature r.nature) ^ ", age: " ^  (string_of_int r.age) ^ "}"
    ;;

    let fg r =
      match r.nature with
      | Good -> Notty.A.white
      | Neutral -> Notty.A.yellow
      | Bad -> Notty.A.red

    let draw r =
      Notty.I.string (Notty.A.fg (fg r)) symbol

    let nature_of_int i =
      let m = i mod 3 in
      match m with
      | 1 -> Good
      | 2 -> Bad
      | _ -> Neutral

    let burn r = false

    let random n =
      let nature = nature_of_int n in
      let age = (n mod 45) + 13 in
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
      "Tree {breed: " ^ (string_of_breed r.breed) ^ ", age: " ^  (string_of_int r.age) ^ "}"
    ;;

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

    let burn e = true

    let random n =
      let breed = breed_of_int n in
      let age = n mod 100 + 1 in
      create
        ~age:age
        ~breed:breed
  end

  type t =
    | Human of Human.t
    | Tree of Tree.t

  let to_string = function
    | Some Human r -> Human.to_string r
    | Some Tree r -> Tree.to_string r
    | None -> "{}"

  let fg = function
    | Human r -> Human.fg r
    | Tree r -> Tree.fg r

  let burn = function
    | Human r -> Human.burn r
    | Tree r -> Tree.burn r

  let draw = function
    | Some Human r -> Human.draw r
    | Some Tree r -> Tree.draw r
    | None -> Notty.I.char Notty.A.empty ' ' 1 1

  let random n =
    let m = n mod 1000 in
    if m == 0 then
      Some (Human (Human.random n))
    else if m > 1 && m < 100 then
      Some (Tree (Tree.random n))
    else None
end

module Item = struct
  module Rock = struct
    type t = {
      weight : int;
    }

    let create ~weight =
      { weight;
      }

    let symbol = "·"

    let fg r = Notty.A.white

    let draw r =
      Notty.I.string (Notty.A.fg (fg r)) symbol

    let to_string r =
      "Rock {weight: " ^ (string_of_int(r.weight)) ^ "}"
    ;;

    let burn r = false
  end

  type t =
    | Rock of Rock.t

  let symbol = function
    | Rock r -> Rock.symbol

  let draw = function
    | Some Rock r -> Rock.draw r
    | None -> Notty.I.char Notty.A.empty ' ' 1 1

  let to_string = function
    | Some Rock r -> Rock.to_string r
    | None -> "{}"

  let random n =
    let m = n mod 1000 in
    if m < 11 then
      Some (Rock (Rock.create ~weight:(Random.int 5)))
    else None
end

module World = struct
  type coord = (int * int)

  type node = {
    entity: Entity.t option;
    item: Item.t option;
  }

  type cell = (coord * node)

  type t = {
    size : (int * int);
    cells : cell list;
  }

  let node_create ~entity ~item =
    { entity;
      item;
    }

  let create w h =
    let rec create_cells x x_max y y_max l =
      let l =
        if x > x_max then
          l
        else
          if y > y_max then
            let node = node_create
              ~entity:(Entity.random (Random.int 2000))
              ~item:(Item.random (Random.int 2000))
            in
            create_cells (x + 1) x_max 0 y_max (((x,y), node) :: l)
          else
            let node = node_create
              ~entity:(Entity.random (Random.int 2000))
              ~item:(Item.random (Random.int 2000))
            in
            create_cells x x_max (y + 1) y_max (((x,y), node) :: l)
      in
      l
    in
    let cells = List.rev (create_cells 0 w 0 h []) in
    { size = (w, h);
      cells = cells;
    }

  let draw w =
    let (x,y) = w.size in
    Notty.I.tabulate x y (fun n m ->
        let node = List.assoc (n,m) w.cells in
        match node.entity with
        | Some e -> Entity.draw node.entity
        | None -> Item.draw node.item
      )

  let print w =
    List.iter
      (fun cell ->
        let (coord,cell) = cell in
        let (x,y) = coord in
        Printf.printf "(%d,%d) = { %s,%s }\n" x y (Entity.to_string cell.entity) (Item.to_string cell.item);
      )
      w.cells

  let render d =
    Notty_unix.output_image d
end
;;

Random.self_init ();;
let terminal_size = Notty_unix.Term.size (Notty_unix.Term.create ());;
let world =
  let (x,y) = terminal_size in
  World.create x y
;;
let draw = World.draw world;;
World.render draw
