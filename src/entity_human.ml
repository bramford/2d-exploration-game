module type S = sig
  include Entity_lib.Entity
  val make_player : t -> string -> (t, string) result
  val is_player : t -> bool
  val is_specific_player : t -> string -> bool
end

module Human : S = struct
  type nature =
    | Good
    | Neutral
    | Bad

  type t = {
    age : int;
    nature : nature;
    inventory : Items.t;
    player : Player.t option;
  }

  let name = "Human"
  let symbol = "@"

  let fg r =
    match r.nature with
    | Good -> Notty.A.white
    | Neutral -> Notty.A.yellow
    | Bad -> Notty.A.red

  let nature_of_int i =
    let m = i mod 3 in
    match m with
    | 1 -> Good
    | 2 -> Bad
    | _ -> Neutral

  let create ~age ~nature ~inventory ~player =
    { age;
      nature;
      inventory;
      player;
    }

  let random n =
    let age = (n mod 45) + 13 in
    let nature = nature_of_int n in
    let player = None in
    let inventory = (Items.random n) in
    create
      ~age
      ~nature
      ~player
      ~inventory

  let to_string r =
    let string_of_nature nature =
      match nature with
      | Good -> "Good"
      | Neutral -> "Neutral"
      | Bad -> "Bad"
    in
    "{Human{nature:" ^ (string_of_nature r.nature) ^ ",age:" ^  (string_of_int r.age) ^ ",player:" ^ (Player.to_string r.player) ^ ",inventory:" ^ (Items.to_string r.inventory) ^ "}}"

  let make_player r n =
    match r.player with
    | Some _ -> Error "Already a player"
    | _ -> Ok (create ~age:r.age ~nature:r.nature ~player:(Player.create ~name:n) ~inventory:r.inventory)

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

  let draw r =
    Notty.I.string (Notty.A.fg (fg r)) symbol
end

include Human
