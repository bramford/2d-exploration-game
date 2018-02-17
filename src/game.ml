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
      match i with
      | 1 -> Neutral
      | 2 -> Bad
      | _ -> Good

    let burn r = false

    let random =
      let nature =
        Random.int 3
        |> nature_of_int
      in
      let age = (Random.int 20) + 13 in
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
        | Birch -> Notty.A.lightgreen

    let draw r =
      Notty.I.string (Notty.A.fg (fg r)) symbol

    let breed_of_int i =
      match i with
      | 1 -> Birch
      | _ -> Pine

    let burn e = true

    let random =
      let breed =
        Random.int 1
        |> breed_of_int
      in
      let age = (Random.int 100) + 1 in
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

  let random =
    match Random.int 10 with
    | 0 -> Human Human.random
    | _ -> Tree Tree.random
end

module World = struct
  type coord = (int * int)

  type t = {
    coords : coord list;
    entities : Entity.t list;
  }

  let create w h =
    let rec coords_of_bounds x x_max y y_max l =
      let l =
        if x > x_max then
          l
        else
          if y > y_max then
            coords_of_bounds (x + 1) x_max 0 y_max ((x, y) :: l)
          else
            coords_of_bounds x x_max (y + 1) y_max ((x, y) :: l)
      in
      l
    in
    let gen_entities c =
      let rec gen_entities_list n n_max l =
        if n <= n_max then
          gen_entities_list (n + 1) n_max (Entity.random :: l)
        else l
      in
      gen_entities_list 0 c []
    in
    let coords = List.rev (coords_of_bounds 0 w 0 h []) in
    let entities = gen_entities (w * h) in
    { coords = coords;
      entities = entities;
    }

  let draw w =
    let draw_entities el =
      let rec d el i =
        match el with
        | [] | [_] -> i
        | hd :: tl ->
          let hd_image = Entity.draw hd in
          d tl (i <|> hd_image)
      in
      d el Notty.I.empty
    in
    draw_entities w.entities

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
