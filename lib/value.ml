type lox_value =
  | LOX_BOOL of bool
  | LOX_STR of string
  | LOX_NUM of float
  | LOX_NIL
  | LOX_CALLABLE of lox_callable
  | LOX_VOID (* Return type for callables that do not return anything *)
  | LOX_CLASS of lox_callable
  | LOX_INSTANCE of lox_value * (string, lox_value) Hashtbl.t

and lox_callable = {
  name : string;
  arity : int;
  call : lox_value list -> (lox_value, Error.t list) result;
}

let stringify_type = function
  | LOX_BOOL _ -> "bool"
  | LOX_STR _ -> "string"
  | LOX_NUM _ -> "number"
  | LOX_NIL -> "nil"
  | LOX_CALLABLE _ -> "callable"
  | LOX_VOID -> "void"
  | LOX_CLASS _ -> "class"
  | LOX_INSTANCE _ -> "instance"

let rec stringify_result = function
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
  | LOX_INSTANCE (cls, _) -> "<instance of " ^ stringify_result cls ^ ">"

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
  | _, _ -> false

module Callable = struct
  let clock_function =
    { name = "clock"; arity = 0; call = (fun _ -> Ok (LOX_NUM (Sys.time ()))) }

  let native_functions = [ clock_function ]

  (* Return list of (name, value) pairs instead of calling Environment.define *)
  let get_native_bindings () =
    native_functions |> List.map (fun f -> f.name, LOX_CALLABLE f)
end
