type t = (string, Value.lox_value) Hashtbl.t list

let push_scope env = Hashtbl.create 16 :: env

let define env name value =
  match env with
  | [] -> failwith "Empty environment"
  | current :: _ ->
      print_endline
        ("[env] Defining variable: " ^ name ^ " at depth "
        ^ string_of_int (List.length env - 1)
        ) ;
      Hashtbl.replace current name value

let get env depth name =
  let rec aux remaining_env =
    let current_depth = List.length remaining_env - 1 in
    match remaining_env with
    | [] -> None
    | scope :: rest ->
        print_endline ("[env] Checking scope at depth " ^ string_of_int current_depth) ;
        Hashtbl.iter
          (fun k v -> print_endline ("  " ^ k ^ ": " ^ Value.stringify_result v))
          scope ;
        if current_depth = depth then
          Hashtbl.find_opt scope name
        else
          aux rest
  in
  print_endline
    ("[env] Getting variable: " ^ name ^ " at depth " ^ string_of_int depth) ;
  List.iteri
    (fun i e ->
      print_endline ("Environment depth: " ^ string_of_int (List.length env - 1 - i)) ;
      Hashtbl.iter
        (fun k v -> print_endline ("  " ^ k ^ ": " ^ Value.stringify_result v))
        e
    )
    env ;
  aux env

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
