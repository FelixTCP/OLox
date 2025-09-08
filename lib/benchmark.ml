(* benchmark.ml - Add this to your lib/ directory *)
open Value

(* Copy-based environment (book's approach) *)
module CopyEnv = struct
  type t = (string, lox_value) Hashtbl.t

  let create () = Hashtbl.create 16

  let create_enclosed (enclosing : t) : t =
    let new_env = create () in
    Hashtbl.iter (fun k v -> Hashtbl.add new_env k v) enclosing ;
    new_env

  let define env name value = Hashtbl.replace env name value
  let get env name = Hashtbl.find_opt env name
end

(* Chain-based environment (your approach) *)
module ChainEnv = struct
  type t = (string, lox_value) Hashtbl.t list

  let create () = [ Hashtbl.create 16 ]
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
end

(* Timing utilities *)
let time_it f x =
  let start = Sys.time () in
  let result = f x in
  let elapsed = Sys.time () -. start in
  result, elapsed

let printf = Printf.printf

let benchmark_scope_creation num_vars num_scopes =
  printf "=== Scope Creation Benchmark ===\n" ;
  printf "Variables: %d, Scopes: %d\n\n" num_vars num_scopes ;

  (* Setup: create base environment with variables *)
  let copy_env = CopyEnv.create () in
  let chain_env = ChainEnv.create () in

  for i = 1 to num_vars do
    let var_name = "var" ^ string_of_int i in
    let var_val = LOX_NUM (float_of_int i) in
    CopyEnv.define copy_env var_name var_val ;
    ChainEnv.define chain_env var_name var_val
  done ;

  (* Benchmark copy-based approach *)
  let copy_test () =
    let rec create_nested_scopes env depth =
      if depth <= 0 then
        env
      else
        let new_env = CopyEnv.create_enclosed env in
        create_nested_scopes new_env (depth - 1)
    in
    create_nested_scopes copy_env num_scopes
  in

  (* Benchmark chain-based approach *)
  let chain_test () =
    let rec create_nested_scopes env depth =
      if depth <= 0 then
        env
      else
        let new_env = ChainEnv.push_scope env in
        create_nested_scopes new_env (depth - 1)
    in
    create_nested_scopes chain_env num_scopes
  in

  let _, copy_time = time_it copy_test () in
  let _, chain_time = time_it chain_test () in

  printf "Copy approach:  %.6f seconds\n" copy_time ;
  printf "Chain approach: %.6f seconds\n" chain_time ;
  printf "Speedup:        %.2fx\n\n" (copy_time /. chain_time)

let benchmark_variable_access num_vars num_scopes num_lookups =
  printf "=== Variable Access Benchmark ===\n" ;
  printf "Variables: %d, Scopes: %d, Lookups: %d\n\n" num_vars num_scopes num_lookups ;

  (* Create nested environments *)
  let copy_env = ref (CopyEnv.create ()) in
  let chain_env = ref (ChainEnv.create ()) in

  (* Add variables to each scope level *)
  for scope = 1 to num_scopes do
    copy_env := CopyEnv.create_enclosed !copy_env ;
    chain_env := ChainEnv.push_scope !chain_env ;

    for var = 1 to num_vars do
      let var_name = Printf.sprintf "s%d_var%d" scope var in
      let var_val = LOX_NUM (float_of_int ((scope * 1000) + var)) in
      CopyEnv.define !copy_env var_name var_val ;
      ChainEnv.define !chain_env var_name var_val
    done
  done ;

  (* Generate random variable names to lookup *)
  Random.init 42 ;
  (* Reproducible results *)
  let random_vars =
    Array.init num_lookups (fun _ ->
        let scope = Random.int num_scopes + 1 in
        let var = Random.int num_vars + 1 in
        Printf.sprintf "s%d_var%d" scope var
    )
  in

  (* Benchmark copy-based lookups *)
  let copy_test () =
    Array.fold_left
      (fun acc var_name ->
        match CopyEnv.get !copy_env var_name with
        | Some _ -> acc + 1
        | None -> acc
      )
      0 random_vars
  in

  (* Benchmark chain-based lookups *)
  let chain_test () =
    Array.fold_left
      (fun acc var_name ->
        match ChainEnv.get !chain_env var_name with
        | Some _ -> acc + 1
        | None -> acc
      )
      0 random_vars
  in

  let copy_found, copy_time = time_it copy_test () in
  let chain_found, chain_time = time_it chain_test () in

  printf "Copy approach:  %.6f seconds (%d found)\n" copy_time copy_found ;
  printf "Chain approach: %.6f seconds (%d found)\n" chain_time chain_found ;
  printf "Access ratio:   %.2fx %s\n\n"
    (abs_float (copy_time /. chain_time))
    (if copy_time > chain_time then "slower" else "faster")

let run_benchmarks () =
  printf "OCaml Environment Performance Comparison\n" ;
  printf "========================================\n\n" ;

  (* Test different scenarios *)
  benchmark_scope_creation 10 5 ;
  benchmark_scope_creation 100 5 ;
  benchmark_scope_creation 1000 5 ;
  benchmark_scope_creation 100 10 ;

  benchmark_variable_access 10 3 1000 ;
  benchmark_variable_access 50 5 1000 ;
  benchmark_variable_access 100 10 10000 ;

  printf "Benchmark complete!\n"
