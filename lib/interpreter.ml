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

  let eval_logical_expr (left_val : Value.lox_value) (op : Token.token)
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
        let depth =
          match Resolution.lookup expr with
          | None -> 0
          | Some d -> d
        in
        match Environment.get env depth name with
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
        eval_expr env r >>= fun r_value -> eval_logical_expr l_value op r_value
    | Expression.CALL (c, a) -> (
        eval_expr env c >>= fun func ->
        match func with
        | Value.LOX_CALLABLE callable ->
            if callable.arity <> List.length a then
              Error
                [
                  runtime_error (Token.make_eof_token ())
                    (Printf.sprintf "Expected %d arguments but got %d" callable.arity
                       (List.length a)
                    );
                ]
            else
              let rec eval_args args =
                match args with
                | [] -> Ok []
                | expr :: rest ->
                    eval_expr env expr >>= fun value ->
                    eval_args rest >>= fun values -> Ok (value :: values)
              in
              eval_args a >>= fun arg_values -> callable.call (List.rev arg_values)
        | o ->
            Error
              [
                Error.RuntimeError
                  ( 0,
                    Printf.sprintf "Function calls not supported for `%s` of type %s"
                      (Value.stringify_result o) (Value.stringify_type o)
                  );
              ]
      )

  type control_flow = RETURN of Value.lox_value | NEXT

  let ( >>? ) result f =
    result >>= function
    | NEXT -> f ()
    | RETURN v -> Ok (RETURN v)

  let rec eval env (res : Resolution.resolution_info) stmt :
      (control_flow, Error.t list) result =
    match stmt with
    | Statement.PRNT expr ->
        eval_expr env expr >>= fun value ->
        print_endline (Value.stringify_result value) ;
        Ok NEXT
    | Statement.EXPR expr -> eval_expr env expr >>= fun _ -> Ok NEXT
    | Statement.VAR_DEF (v, None) ->
        let name = v.lexeme in
        Environment.define env name Value.LOX_NIL ;
        Ok NEXT
    | Statement.VAR_DEF (v, Some expr) ->
        let name = v.lexeme in
        eval_expr env expr >>= fun value ->
        Environment.define env name value ;
        Ok NEXT
    | Statement.BLOCK stmts ->
        let block_env = Environment.push_scope env in
        eval_block block_env res stmts
    | Statement.IF (cond, then_branch, else_branch) -> (
        eval_expr env cond >>= fun condition ->
        if Value.is_truthy condition then
          eval env res then_branch >>? fun () -> Ok NEXT
        else
          match else_branch with
          | None -> Ok NEXT
          | Some else_stmt -> eval env res else_stmt >>? fun () -> Ok NEXT
      )
    | Statement.WHILE (cond, body) ->
        let rec loop () =
          eval_expr env cond >>= fun condition ->
          if Value.is_truthy condition then
            eval env res body >>? fun () -> loop ()
          else
            Ok NEXT
        in
        loop ()
    | Statement.FOR (init, cond, incr, body) ->
        let for_env = Environment.push_scope env in
        eval for_env res init >>= fun _ ->
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
        eval for_env res
          (Statement.WHILE (condition, Statement.BLOCK [ body; increment ]))
    | Statement.FUN_DEF (name, params, body) ->
        let func : Value.lox_callable =
          {
            name = name.lexeme;
            arity = List.length params;
            call =
              (fun args ->
                let func_env = Environment.push_scope env in
                List.iter2
                  (fun (param : Token.token) arg ->
                    Environment.define func_env param.lexeme arg
                  )
                  params args ;
                eval func_env res (Statement.BLOCK body) >>= function
                | NEXT -> Ok Value.LOX_VOID
                | RETURN v -> Ok v
              );
          }
        in
        let fn = Value.LOX_CALLABLE func in
        Environment.define env name.lexeme fn ;
        Ok NEXT
    | Statement.RETURN expr -> eval_expr env expr >>= fun v -> Ok (RETURN v)

  and eval_block env res = function
    | [] -> Ok NEXT
    | [ stmt ] -> eval env res stmt
    | stmt :: rest -> eval env res stmt >>? fun () -> eval_block env res rest

  let interpret_ast (env : Environment.t) (ast : AST.ast) :
      (Value.lox_value, Error.t list) result =
    let rec execute_statements res = function
      | [] -> Ok Value.LOX_VOID
      | [ Statement.EXPR expr ] -> eval_expr env expr
      | stmt :: rest -> eval env res stmt >>= fun _ -> execute_statements res rest
    in
    match ast with
    | AST.PROGRAM (stmts, res) -> execute_statements res stmts
end
