(*
#require "notty";;
#require "notty.unix";;
#require "notty.top";;
*)

let game_loop world =
  let term = (Notty_unix.Term.create ~mouse:false ~bpaste:false ()) in
  let player = "player" in
  World.draw_around_player world player |> World.render term;
  let rec event_loop term world player =
    match (Notty_unix.Term.event term) with
    | `Key (`ASCII 'h',_) ->
      let world = World.move_player world player World.Left in
      World.draw_around_player world player |> World.render term;
      event_loop term world player
    | `Key (`ASCII 'j',_) ->
      let world = World.move_player world player World.Down in
      World.draw_around_player world player |> World.render term;
      event_loop term world player
    | `Key (`ASCII 'k',_) ->
      let world = World.move_player world player World.Up in
      World.draw_around_player world player |> World.render term;
      event_loop term world player
    | `Key (`ASCII 'l',_) ->
      let world = World.move_player world player World.Right in
      World.draw_around_player world player |> World.render term;
      event_loop term world player
    | `Key (`Escape,_) | `Key (`ASCII 'q',_) | `Key (`ASCII 'C',[`Ctrl]) -> exit 0
    | _ -> event_loop term world player
  in
  event_loop term world player

let run_game mode =
  Random.self_init ();
  let world = World.create 128 128 in
  let world =
    match World.add_player world "player" with
    | Ok w -> w
    | Error s ->
      Printf.eprintf "ERROR: %s\n" s;
      exit 1
  in
  match mode with
  | Some "print_world" ->
    World.print world
  | Some _ | None ->
    game_loop world

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
