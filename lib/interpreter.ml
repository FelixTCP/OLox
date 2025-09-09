module Interpreter = struct
  open Parser
  open Lexer

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
        Error [ runtime_error op "Right operand on concat must be a string" ]
    | l, PLUS, _ ->
        Error
          [
            runtime_error op
              (Printf.sprintf
                 "Left operand on '+' must be a Value of type number or string but \
                  was `%s` of type %s"
                 (Value.stringify_result l) (Value.stringify_type l)
              );
          ]
    | LOX_NUM l, GREATER, LOX_NUM r -> Ok (LOX_BOOL (l > r))
    | LOX_NUM l, GREATER_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l >= r))
    | LOX_NUM l, LESS, LOX_NUM r -> Ok (LOX_BOOL (l < r))
    | LOX_NUM l, LESS_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l <= r))
    | LOX_NUM _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
        Error [ runtime_error op "Right operand on comparison must be a number" ]
    | _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
        Error [ runtime_error op "Left operand on comparison must be a number" ]
    | _, EQUAL_EQUAL, _ -> Ok (LOX_BOOL (Value.is_equal left_val right_val))
    | _, BANG_EQUAL, _ -> Ok (LOX_BOOL (not (Value.is_equal left_val right_val)))
    | _ -> Error [ runtime_error op "Unknown binary operator" ]

  let eval_locial_expr (left_val : Value.lox_value) (op : Token.token)
      (right_val : Value.lox_value) : (Value.lox_value, Error.t list) result =
    match op.ttype with
    | AND -> Ok (if not (Value.is_truthy left_val) then left_val else right_val)
    | OR -> Ok (if Value.is_truthy left_val then left_val else right_val)
    | _ -> Error [ runtime_error op "Unknowlogical operator" ]

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
    | Expression.ASSIGN (v, e) -> (
        let name = v.lexeme in
        eval_expr env e >>= fun value ->
        match Environment.assign env name value with
        | None ->
            Error
              [
                runtime_error v ("Could not assign to unknown variable named " ^ name);
              ]
        | Some value -> Ok value
      )
    | Expression.LOGICAL (l, op, r) ->
        eval_expr env l >>= fun l_value ->
        eval_expr env r >>= fun r_value -> eval_locial_expr l_value op r_value
    | Expression.CALL _ ->
        Error [ Error.RuntimeError (0, "Function calls not supported") ]

  type interpreter_result = VALUE of Value.lox_value | NO_VALUE

  let rec eval env stmt =
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
    | Statement.BLOCK stmts ->
        let block_env = Environment.push_scope env in
        let rec exec_block = function
          | [] -> Ok Value.LOX_NIL
          | s :: rest -> eval block_env s >>= fun _ -> exec_block rest
        in
        exec_block stmts
    | Statement.IF (cond, then_branch, else_branch) -> (
        eval_expr env cond >>= fun condition ->
        if Value.is_truthy condition then
          eval env then_branch
        else
          match else_branch with
          | None -> Ok Value.LOX_NIL
          | Some else_stmt -> eval env else_stmt
      )
    | Statement.WHILE (cond, body) ->
        let rec loop () =
          eval_expr env cond >>= fun condition ->
          if Value.is_truthy condition then
            eval env body >>= fun _ -> loop ()
          else
            Ok Value.LOX_NIL
        in
        loop ()
    | Statement.FOR (init, cond, incr, body) ->
        let for_env = Environment.push_scope env in
        eval for_env init >>= fun _ ->
        let condition =
          match cond with
          | None -> Expression.LITERAL (L_BOOL true)
          | Some c -> c
        in
        let increment =
          match incr with
          | None -> Statement.EXPR (Expression.LITERAL (L_BOOL true))
          | Some i -> Statement.EXPR i
        in
        eval for_env
          (Statement.WHILE (condition, Statement.BLOCK [ body; increment ]))

  let interpret_ast (env : Environment.t) (ast : AST.ast) :
      (interpreter_result, Error.t list) result =
    let rec execute_statements = function
      | [] -> Ok NO_VALUE
      | [ Statement.EXPR expr ] -> eval_expr env expr >>= fun v -> Ok (VALUE v)
      | stmt :: rest -> eval env stmt >>= fun _ -> execute_statements rest
    in
    match ast with
    | AST.PROGRAM statements -> execute_statements statements
end
