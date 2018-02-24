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

  let fg = function
    | Human r -> Human.fg r
    | Tree r -> Tree.fg r

  let burn = function
    | Human r -> Human.burn r
    | Tree r -> Tree.burn r

  let draw = function
    | Human r -> Human.draw r
    | Tree r -> Tree.draw r

  let random n =
    let m = n mod 100 in
    match m with
    | 0 -> Human (Human.random n)
    | _ -> Tree (Tree.random n)
end

module World = struct
  type coord = (int * int)

  type t = {
    size : (int * int);
    entities : (coord * Entity.t) list;
  }

  let create w h =
    let rec create_coord_entities x x_max y y_max l =
      let l =
        if x > x_max then
          l
        else
          if y > y_max then
            create_coord_entities (x + 1) x_max 0 y_max (((x, y), Entity.random (Random.int 200)) :: l)
          else
            create_coord_entities x x_max (y + 1) y_max (((x, y), Entity.random (Random.int 200)) :: l)
      in
      l
    in
    let entities = List.rev (create_coord_entities 0 w 0 h []) in
    { size = (w, h);
      entities = entities;
    }

  let draw w =
    let (x,y) = w.size in
    Notty.I.tabulate x y (fun n m ->
        let entity = List.assoc (n,m) w.entities in
        Entity.draw entity
      )

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
