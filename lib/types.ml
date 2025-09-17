(* This file just contains the same definitions as the .mli *)
type lox_value =
  | LOX_BOOL of bool
  | LOX_STR of string
  | LOX_NUM of float
  | LOX_NIL
  | LOX_CALLABLE of lox_callable
  | LOX_VOID
  | LOX_CLASS of lox_class
  | LOX_INSTANCE of lox_instance

and environment = (string, lox_value) Hashtbl.t list

and lox_callable = {
  name : string;
  arity : int;
  call : environment -> lox_value list -> (lox_value, Error.t list) result;
  closure : environment;
  is_initializer : bool;
}

and lox_class = {
  name : string;
  superclass : lox_class option;
  methods : (string, lox_callable) Hashtbl.t;
}

and lox_instance = {
  class_ref : lox_class;
  id : int;
  fields : (string, lox_value) Hashtbl.t;
}
