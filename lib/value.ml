open Types

let stringify_type = function
  | LOX_BOOL _ -> "bool"
  | LOX_STR _ -> "string"
  | LOX_NUM _ -> "number"
  | LOX_NIL -> "nil"
  | LOX_CALLABLE _ -> "callable"
  | LOX_VOID -> "void"
  | LOX_CLASS _ -> "class"
  | LOX_INSTANCE _ -> "instance"

let stringify_result = function
  | LOX_BOOL b -> string_of_bool b
  | LOX_STR s -> s
  | LOX_NUM n ->
      if Float.is_integer n then
        string_of_int (Float.to_int n)
      else
        string_of_float n
  | LOX_NIL -> "nil"
  | LOX_CALLABLE c -> "<fn " ^ c.name ^ ">"
  | LOX_VOID -> "void"
  | LOX_CLASS c -> "<class " ^ c.name ^ ">"
  | LOX_INSTANCE i -> "<instance of " ^ i.class_ref.name ^ ">"

let is_truthy expr =
  match expr with
  | LOX_NIL -> false
  | LOX_BOOL b -> b
  | _ -> true

let is_equal left right =
  match left, right with
  | LOX_NUM l, LOX_NUM r -> l = r
  | LOX_STR l, LOX_STR r -> String.equal l r
  | LOX_BOOL l, LOX_BOOL r -> Bool.equal l r
  | LOX_NIL, LOX_NIL -> true
  | LOX_VOID, LOX_VOID -> true
  (* TODO: Implement other equalities if needed *)
  (* | LOX_CALLABLE l, LOX_CALLABLE r -> l = r *)
  (* | LOX_CLASS l, LOX_CLASS r -> l = r *)
  | LOX_INSTANCE l, LOX_INSTANCE r -> l.id = r.id
  | _, _ -> false

module Callable = struct
  let clock_function : lox_callable =
    {
      name = "clock";
      arity = 0;
      call = (fun _ _ -> Ok (LOX_NUM (Sys.time ())));
      closure = [];
      is_initializer = false;
    }

  let native_functions : lox_callable list = [ clock_function ]

  let get_native_bindings () : (string * lox_value) list =
    native_functions |> List.map (fun (f : lox_callable) -> f.name, LOX_CALLABLE f)
end
