open Value

type lox_callable = {
  name : string;
  arity : int;
  call : lox_value list -> lox_value;
}

let clock_function =
  { name = "clock"; arity = 0; call = (fun _ -> LOX_NUM (Sys.time ())) }

(* let make_user_function name params body closure = *)
(*   { *)
(*     name; *)
(*     arity = List.length params; *)
(*     call = *)
(*       (fun args -> *)
(*         (* Implementation will go here *) *)
(*         LOX_NIL *)
(*       ); *)
(*   } *)
