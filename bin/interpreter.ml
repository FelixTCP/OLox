module Interpreter = struct
  open Parser
  open Lexer

  type lox_value =
    | LOX_BOOL of bool
    | LOX_STR of string
    | LOX_NUM of float
    | LOX_NIL

  let ( >>= ) result f =
    match result with
    | Ok x -> f x
    | Error e -> Error e

  let runtime_error (token : Token.token) msg = Error.RuntimeError (token.line, msg)

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

  let eval_unary_expr (op : Token.token) value : (lox_value, Error.t list) result =
    match op.ttype, value with
    | MINUS, LOX_NUM n -> Ok (LOX_NUM (-.n))
    | MINUS, _ -> Error [ runtime_error op "Operand must be a number" ]
    | BANG, value -> Ok (LOX_BOOL (not (is_truthy value)))
    | _ -> Error [ runtime_error op "Unknown unary operator" ]

  let eval_binary_expr (left_val : lox_value) (op : Token.token)
      (right_val : lox_value) : (lox_value, Error.t list) result =
    match left_val, op.ttype, right_val with
    | LOX_NUM l, PLUS, LOX_NUM r -> Ok (LOX_NUM (l +. r))
    | LOX_NUM l, MINUS, LOX_NUM r -> Ok (LOX_NUM (l -. r))
    | LOX_NUM l, STAR, LOX_NUM r -> Ok (LOX_NUM (l *. r))
    | LOX_NUM l, SLASH, LOX_NUM r ->
        if r = 0.0 then
          Error [ runtime_error op "Division by zero" ]
        else
          Ok (LOX_NUM (l /. r))
    | LOX_NUM _, (PLUS | MINUS | STAR | SLASH), _ ->
        Error [ runtime_error op "Right operand must be a number" ]
    | _, (MINUS | STAR | SLASH), _ ->
        Error [ runtime_error op "Left operand must be a number" ]
    | LOX_STR l, PLUS, LOX_STR r -> Ok (LOX_STR (String.cat l r))
    | LOX_STR _, PLUS, _ ->
        Error [ runtime_error op "Right operand must be a string" ]
    | _, PLUS, _ -> Error [ runtime_error op "Left operand must be a string" ]
    | LOX_NUM l, GREATER, LOX_NUM r -> Ok (LOX_BOOL (l > r))
    | LOX_NUM l, GREATER_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l >= r))
    | LOX_NUM l, LESS, LOX_NUM r -> Ok (LOX_BOOL (l < r))
    | LOX_NUM l, LESS_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l <= r))
    | LOX_NUM _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
        Error [ runtime_error op "Right operand must be a number" ]
    | _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
        Error [ runtime_error op "Left operand must be a number" ]
    | _, EQUAL_EQUAL, _ -> Ok (LOX_BOOL (is_equal left_val right_val))
    | _ -> Error [ runtime_error op "Unknown binary operator" ]

  let rec eval_expr (expr : Expression.expr) : (lox_value, Error.t list) result =
    match expr with
    | Expression.LITERAL lit -> (
        match lit with
        | L_BOOL b -> Ok (LOX_BOOL b)
        | L_STRING s -> Ok (LOX_STR s)
        | L_NUM f -> Ok (LOX_NUM f)
        | L_NIL -> Ok LOX_NIL
      )
    | Expression.GROUPING e -> eval_expr e
    | Expression.UNARY (op, e) ->
        eval_expr e >>= fun value -> eval_unary_expr op value
    | Expression.BINARY (l, op, r) ->
        eval_expr l >>= fun l_value ->
        eval_expr r >>= fun r_value -> eval_binary_expr l_value op r_value

  let interpret_ast (ast : AST.ast) : (lox_value, Error.t list) result =
    match ast with
    | NODE (expr, _, _) -> eval_expr expr
    | _ ->
        Error
          [ runtime_error (Token.make_eof_token ()) "Expected valid syntax-tree" ]

  let stringify_result = function
    | LOX_BOOL b -> string_of_bool b
    | LOX_STR s -> s
    | LOX_NUM n -> string_of_float n
    | LOX_NIL -> "nil"
end
