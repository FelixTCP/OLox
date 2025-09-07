module Interpreter = struct
  open Parser
  open Lexer
  open Value
  open Environment

  let ( >>= ) result f =
    match result with
    | Ok x -> f x
    | Error e -> Error e

  let runtime_error (token : Token.token) msg = Error.RuntimeError (token.line, msg)

  let eval_unary_expr (op : Token.token) value :
      (Value.lox_value, Error.t list) result =
    match op.ttype, value with
    | MINUS, Value.LOX_NUM n -> Ok (LOX_NUM (-.n))
    | MINUS, _ -> Error [ runtime_error op "Operand must be a number" ]
    | BANG, value -> Ok (LOX_BOOL (not (Value.is_truthy value)))
    | _ -> Error [ runtime_error op "Unknown unary operator" ]

  let eval_binary_expr (left_val : Value.lox_value) (op : Token.token)
      (right_val : Value.lox_value) : (Value.lox_value, Error.t list) result =
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
    | _, EQUAL_EQUAL, _ -> Ok (LOX_BOOL (Value.is_equal left_val right_val))
    | _ -> Error [ runtime_error op "Unknown binary operator" ]

  let rec eval_expr env (expr : Expression.expr) :
      (Value.lox_value, Error.t list) result =
    match expr with
    | Expression.LITERAL lit -> (
        match lit with
        | L_BOOL b -> Ok (LOX_BOOL b)
        | L_STRING s -> Ok (LOX_STR s)
        | L_NUM f -> Ok (LOX_NUM f)
        | L_NIL -> Ok LOX_NIL
      )
    | Expression.GROUPING e -> eval_expr env e
    | Expression.UNARY (op, e) ->
        eval_expr env e >>= fun value -> eval_unary_expr op value
    | Expression.BINARY (l, op, r) ->
        eval_expr env l >>= fun l_value ->
        eval_expr env r >>= fun r_value -> eval_binary_expr l_value op r_value
    | Expression.VARIABLE v -> (
        let name = v.lexeme in
        match Environment.get env name with
        | None -> Error [ runtime_error v ("Could not find variable named " ^ name) ]
        | Some value -> Ok value
      )

  let eval env stmt =
    match stmt with
    | Statement.PRNT expr ->
        eval_expr env expr >>= fun value ->
        print_endline (Value.stringify_result value) ;
        Ok Value.LOX_NIL
    | Statement.EXPR expr -> eval_expr env expr
    | Statement.VAR_DEF (v, None) ->
        let name = v.lexeme in
        Environment.define env name Value.LOX_NIL ;
        Ok LOX_NIL
    | Statement.VAR_DEF (v, Some expr) ->
        let name = v.lexeme in
        eval_expr env expr >>= fun value ->
        Environment.define env name value ;
        Ok Value.LOX_NIL

  let interpret_ast (ast : AST.ast) : (Value.lox_value, Error.t list) result =
    let env = Environment.create () in
    match ast with
    | AST.PROGRAM statements ->
        let rec aux = function
          | [] -> Ok Value.LOX_NIL
          | [ stmt ] -> eval env stmt
          | stmt :: rest -> eval env stmt >>= fun _ -> aux rest
        in
        aux statements
end
