(*
#require "notty";;
#require "notty.unix";;
#require "notty.top";;
*)

open Notty.Infix

module Player = struct
  type t =
    { name: string }

  let to_string r =
    match r with
    | Some x -> x.name
    | None -> "None"

  let create ~name =
    Some ({ name })

end

module Item = struct
  module Rock = struct
    type t =
      { weight : int }

    let create ~weight =
      { weight }

    let symbol = "·"

    let fg r = Notty.A.white

    let draw r =
      Notty.I.string (Notty.A.fg (fg r)) symbol

    let to_string r =
      "Rock{weight:" ^ (string_of_int(r.weight)) ^ "}"
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
    | None -> "None"

  let random i =
    if i mod 1000 < 11 then
      Some (Rock (Rock.create ~weight:((Random.int 4) + 1)))
    else None
end

module Items = struct
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
    | hd :: tl -> Item.draw hd

  let to_string l =
    let rec ts acc l s se =
      match l with
      | [] | [_] -> s ^ se
      | hd :: tl ->
        let sep = if acc > 0 then "," else "" in
        ts (acc + 1) tl (s ^ sep ^ (Item.to_string hd)) se
    in
    ts 0 l "[" "]"
end

module Entity = struct
  module Human = struct
    type nature =
      | Good
      | Neutral
      | Bad

    type t = {
      age: int;
      nature: nature;
      inventory: Items.t;
      player: Player.t option;
    }

    let symbol = "@"

    let create ~age ~nature ~player ~inventory =
      { age;
        nature;
        player;
        inventory;
      }

    let to_string r =
      let string_of_nature nature =
        match nature with
        | Good -> "Good"
        | Neutral -> "Neutral"
        | Bad -> "Bad"
      in
      "{Human{nature:" ^ (string_of_nature r.nature) ^ ",age:" ^  (string_of_int r.age) ^ ",player:" ^ (Player.to_string r.player) ^ ",inventory:" ^ (Items.to_string r.inventory) ^ "}}"

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


    let is_player r =
      match r.player with
      | Some _ -> true
      | _ -> false

    let is_specific_player r n =
      match r.player with
      | Some x ->
        if (String.equal x.name n) then
          true
        else false
      | _ -> false

    let random n =
      let nature = nature_of_int n in
      let age = (n mod 45) + 13 in
      create
        ~age:age
        ~nature:nature
        ~player:(Player.create ~name:"player")
        ~inventory:(Items.random n)
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
  end

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
end

module World = struct
  type coord = (int * int)

  type node = {
    entity: Entity.t option;
    items: Items.t;
  }

  type cell = (coord * node)

  type t = {
    size : (int * int);
    cells : cell list;
  }

  type move_direction = Left | Right | Up | Down

  let node_create ~entity ~items =
    { entity;
      items;
    }

  let rec find_player cells name =
    match cells with
    | [] | [_] -> Error "Player cell not found"
    | hd :: tl ->
      let (coord,node) = hd in
      if (Entity.is_specific_player name node.entity) then
        Ok hd
      else
        find_player tl name

  let create w h =
    let rec create_cells x x_max y y_max l =
      if x > x_max then
        l
      else
        if y > y_max then
          create_cells (x + 1) x_max 0 y_max l
        else
          let node = node_create
            ~entity:(Entity.random (Random.int 2000))
            ~items:(Items.random (Random.int 2000))
          in
          create_cells x x_max (y + 1) y_max (((x,y), node) :: l)
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
        | None -> Items.draw node.items
      )

  let draw_around_player w n =
    let (max_x,max_y) = w.size in
    let player_cell = find_player w.cells n in
    match player_cell with
    | Error s ->
      Printf.eprintf "No player found: %s\n" s;
      exit 1
    | Ok r ->
      let (coord,node) = r in
      let (c_x,c_y) = coord in
      let diff_x = c_x - 20 in
      let diff_y = c_y - 10 in
      let draw_x = if diff_x > 0 then diff_x else 1 in
      let draw_y = if diff_y > 0 then diff_y else 1 in
      let rec calc_draw_size axis axis_max default =
        if axis + default > axis_max then calc_draw_size axis axis_max (default - 1)
        else default
      in
      Notty.I.tabulate (calc_draw_size draw_x max_x 40) (calc_draw_size draw_y max_y 20) (fun n m ->
          let node = List.assoc ((n + draw_x),(m + draw_y)) w.cells in
          match node.entity with
          | Some e -> Entity.draw node.entity
          | None -> Items.draw node.items
        )

  let calc_move_coord c d max_x max_y =
    let (cx,cy) = c in
    match d with
    | Right -> if (cx + 1) < max_x then None else Some ((cx + 1),cy)
    | Left -> if (cx - 1) < 0 then None else Some ((cx - 1),cy)
    | Up -> if (cy - 1) < 0 then None else Some (cx,(cy - 1))
    | Down -> if (cy + 1) < max_y then None else Some (cx,(cy + 1))

  let update_cell cell cells w_size =
    let rec cells_after cell cells =
      match cells with
      | [] -> []
      | hd :: tl ->
        let ((hdcx,hdcy),_) = hd in
        let ((c_x,c_y),_) = cell in
        if hdcx == c_x && hdcy == c_y then
          tl
        else cells_after cell tl
    in
    List.append
      (cells_after cell (List.rev cells))
      (cell :: (cells_after cell cells))

  let move_player w p d =
    let player_cell = find_player w.cells p in
    match player_cell with
    | Ok r ->
      let (pc,pn) = r in
      let upn = {entity = None;items = pn.items} in
      let (max_x,max_y) = pc in
      let dc = calc_move_coord pc d max_x max_y in
      begin match dc with
      | None -> w
      | Some c ->
        let dn = (List.assoc c w.cells) in
        begin match dn.entity with
        | Some _ -> w
        | None ->
          let udn = {entity = pn.entity;items = dn.items} in
          let cells = (update_cell (c,udn)  w.cells w.size) in
          let cells = (update_cell (pc,upn) cells w.size) in
          {size = w.size;cells = cells}
        end
      end
    | Error s ->
      Printf.eprintf "No player found: %s\n" s;
      w

  let render d =
    Notty_unix.output_image d

  let print_cell c =
    let (coord,cell) = c in
    let (x,y) = coord in
    Printf.eprintf "(%d,%d) = {%s,%s}\n" x y (Entity.to_string cell.entity) (Items.to_string cell.items)

  let print w =
    Printf.eprintf "Printing world...\n";
    List.iter print_cell w.cells
end

let clear_screen () =
  let open Notty_unix in
  let (x,y) = (Term.size (Term.create ())) in
  output_image (Notty.I.void x y)

let game_loop world =
  World.draw_around_player world "player" |> World.render

let run_game mode =
  let open Core.Command in
  Random.self_init ();
  let world =
    World.create 128 128
  in
  match mode with
  | Some "print_world" ->
    World.print world
  | Some _ | None ->
    clear_screen ();
    game_loop world;
;;

let command =
  let open Core.Command in
  let spec = Spec.(empty +> anon (maybe ("mode" %: string))) in
  basic_spec
    ~summary:"2D Exploration Game"
    ~readme:(fun () -> "A simple 2D exploration game that runs exclusively in the terminal")
    spec
    (fun mode () -> run_game mode)

let () =
  Core.Command.run command
