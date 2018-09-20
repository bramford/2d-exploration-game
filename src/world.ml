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
    let (_,node) = hd in
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
      | Some _ -> Entity.draw node.entity
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
    let (coord,_) = r in
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
        | Some _ -> Entity.draw node.entity
        | None -> Items.draw node.items
      )

let calc_move_coord c d max_x max_y =
  let (cx,cy) = c in
  match d with
  | Right -> if (cx + 1) < max_x then None else Some ((cx + 1),cy)
  | Left -> if (cx - 1) < 0 then None else Some ((cx - 1),cy)
  | Up -> if (cy - 1) < 0 then None else Some (cx,(cy - 1))
  | Down -> if (cy + 1) < max_y then None else Some (cx,(cy + 1))

let update_cell cell cells =
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
        let cells = (update_cell (c,udn)  w.cells) in
        let cells = (update_cell (pc,upn) cells) in
        {size = w.size;cells = cells}
      end
    end
  | Error s ->
    Printf.eprintf "No player found: %s\n" s;
    w

let add_player w p =
  let rec find_humans cells humans =
    match cells with
    | [] -> humans
    | hd :: tl ->
      let (_,node) = hd in
      if (Entity.is_human node.entity) && (not (Entity.is_player node.entity)) then
        find_humans tl (hd :: humans)
      else
        find_humans tl humans
  in
  let humans = find_humans w.cells [] in
  if List.length humans < 1 then
    Error "No non-player humans found"
  else
    let (c,n) = List.nth humans (Random.int (List.length humans)) in
    match Entity.make_player p n.entity with
    | Ok ne ->
      let nn = {entity = Some ne; items = n.items;} in
      let cells = (update_cell (c,nn) w.cells) in
      Ok {size = w.size;cells = cells}
    | Error s -> Error s

let render t wi =
  let (_,fdo) = Notty_unix.Term.fds t in
  let oc = Unix.out_channel_of_descr fdo in
  let (term_x,term_y) = Notty_unix.Term.size t in
  let image_x = Notty.I.width wi in
  let image_y = Notty.I.height wi in
  let pad_x_each_side = (term_x - image_x) / 2 in
  let pad_y_each_side = (term_y - image_y) / 2 in
  let image = Notty.I.pad ~l:pad_x_each_side ~r:pad_x_each_side ~t:pad_y_each_side ~b:pad_y_each_side wi in
  Notty_unix.output_image ~fd:oc image;
  flush oc

let print_cell c =
  let (coord,cell) = c in
  let (x,y) = coord in
  Printf.eprintf "(%d,%d) = {%s,%s}\n" x y (Entity.to_string cell.entity) (Items.to_string cell.items)

let print w =
  Printf.eprintf "Printing world...\n";
  List.iter print_cell w.cells
