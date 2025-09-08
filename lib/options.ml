type t = (string, bool) Hashtbl.t

let opts : t = Hashtbl.create 4
let set_flag name value = Hashtbl.replace opts name value
let is_active name = Hashtbl.find_opt opts name |> Option.value ~default:false

let parse_flags flags =
  flags
  |> List.iter (fun flag ->
         let name = String.sub flag 2 (String.length flag - 2) in
         set_flag name true
     )
