type t = { name: string }

let to_string t =
  match t with
  | Some t -> t.name
  | None -> "None"

let create ~name =
  Some ({ name })
