module Value = struct
  type lox_value =
    | LOX_BOOL of bool
    | LOX_STR of string
    | LOX_NUM of float
    | LOX_NIL

  let stringify_result = function
    | LOX_BOOL b -> string_of_bool b
    | LOX_STR s -> s
    | LOX_NUM n -> string_of_float n
    | LOX_NIL -> "nil"

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
    | _, _ -> false
end
