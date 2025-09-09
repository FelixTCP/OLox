type t = (string, Value.lox_value) Hashtbl.t list

let push_scope env = Hashtbl.create 16 :: env

let define env name value =
  match env with
  | [] -> failwith "Empty environment"
  | current :: _rest -> Hashtbl.replace current name value

let rec get env name =
  match env with
  | [] -> None
  | scope :: rest -> (
      match Hashtbl.find_opt scope name with
      | Some value -> Some value
      | None -> get rest name
    )

let assign env name value =
  let rec aux = function
    | [] -> None
    | scope :: rest ->
        if Hashtbl.mem scope name then (
          Hashtbl.replace scope name value ;
          Some value
        ) else
          aux rest
  in
  aux env

let create () =
  let env = [ Hashtbl.create 16 ] in
  Value.Callable.get_native_bindings ()
  |> List.iter (fun (name, value) -> define env name value) ;
  env
