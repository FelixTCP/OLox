module Options = struct
  type t = (string, bool) Hashtbl.t

  let global_opts : t = Hashtbl.create 4
  let set_flag name value = Hashtbl.replace global_opts name value
  let is_active name = try Hashtbl.find global_opts name with Not_found -> false

  let parse_flags flags =
    flags
    |> List.filter (String.starts_with ~prefix:"--")
    |> List.iter (fun flag ->
           let name = String.sub flag 2 (String.length flag - 2) in
           set_flag name true
       )
end
