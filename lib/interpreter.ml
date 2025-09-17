open Lexer

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let err (token : Token.t) msg = Error [ Error.RuntimeError (token.line, msg) ]

let eval_unary_expr (op : Token.t) value : (Value.lox_value, Error.t list) result =
  match op.ttype, value with
  | MINUS, Value.LOX_NUM n -> Ok (LOX_NUM (-.n))
  | MINUS, _ -> err op "Operand must be a number"
  | BANG, value -> Ok (LOX_BOOL (not (Value.is_truthy value)))
  | _ -> err op "Unknown unary operator"

let eval_binary_expr (left_val : Value.lox_value) (op : Token.t)
    (right_val : Value.lox_value) : (Value.lox_value, Error.t list) result =
  match left_val, op.ttype, right_val with
  | LOX_NUM l, PLUS, LOX_NUM r -> Ok (LOX_NUM (l +. r))
  | LOX_NUM l, MINUS, LOX_NUM r -> Ok (LOX_NUM (l -. r))
  | LOX_NUM l, STAR, LOX_NUM r -> Ok (LOX_NUM (l *. r))
  | LOX_NUM l, SLASH, LOX_NUM r ->
      if r = 0.0 then
        err op "Division by zero"
      else
        Ok (LOX_NUM (l /. r))
  | LOX_NUM _, (PLUS | MINUS | STAR | SLASH), _ ->
      err op "Right operand must be a number"
  | _, (MINUS | STAR | SLASH), _ -> err op "Left operand must be a number"
  | LOX_STR l, PLUS, LOX_STR r -> Ok (LOX_STR (String.cat l r))
  | LOX_STR _, PLUS, _ -> err op "Right operand on concat must be a string"
  | l, PLUS, _ ->
      err op
        (Printf.sprintf
           "Left operand on '+' must be a Value of type number or string but was \
            `%s` of type %s"
           (Value.stringify_result l) (Value.stringify_type l)
        )
  | LOX_NUM l, GREATER, LOX_NUM r -> Ok (LOX_BOOL (l > r))
  | LOX_NUM l, GREATER_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l >= r))
  | LOX_NUM l, LESS, LOX_NUM r -> Ok (LOX_BOOL (l < r))
  | LOX_NUM l, LESS_EQUAL, LOX_NUM r -> Ok (LOX_BOOL (l <= r))
  | LOX_NUM _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
      err op "Right operand on comparison must be a number"
  | _, (GREATER | GREATER_EQUAL | LESS | LESS_EQUAL), _ ->
      err op "Left operand on comparison must be a number"
  | _, EQUAL_EQUAL, _ -> Ok (LOX_BOOL (Value.is_equal left_val right_val))
  | _, BANG_EQUAL, _ -> Ok (LOX_BOOL (not (Value.is_equal left_val right_val)))
  | _ -> err op "Unknown binary operator"

let eval_logical_expr (left_val : Value.lox_value) (op : Token.t)
    (right_val : Value.lox_value) : (Value.lox_value, Error.t list) result =
  match op.ttype with
  | AND -> Ok (if not (Value.is_truthy left_val) then left_val else right_val)
  | OR -> Ok (if Value.is_truthy left_val then left_val else right_val)
  | _ -> err op "Unknowlogical operator"

let rec eval_expr env res (expr : Expression.t) :
    (Value.lox_value, Error.t list) result =
  match expr with
  | Expression.LITERAL lit -> (
      match lit with
      | L_BOOL b -> Ok (LOX_BOOL b)
      | L_STRING s -> Ok (LOX_STR s)
      | L_NUM f -> Ok (LOX_NUM f)
      | L_NIL -> Ok LOX_NIL
    )
  | Expression.GROUPING e -> eval_expr env res e
  | Expression.UNARY (op, e) ->
      eval_expr env res e >>= fun value -> eval_unary_expr op value
  | Expression.BINARY (l, op, r) ->
      eval_expr env res l >>= fun l_value ->
      eval_expr env res r >>= fun r_value -> eval_binary_expr l_value op r_value
  | Expression.VARIABLE v -> (
      let name = v.lexeme in
      let depth =
        match Hashtbl.find_opt res expr with
        | None ->
            Printf.sprintf "Variable %s not resolved" name |> print_endline ;
            0
        | Some d -> d
      in
      match Environment.get env depth name with
      | None ->
          err v
            (Printf.sprintf "Could not find variable named %s at depth %d" name depth)
      | Some value -> Ok value
    )
  | Expression.ASSIGN (v, e) -> (
      let name = v.lexeme in
      eval_expr env res e >>= fun value ->
      match Environment.assign env name value with
      | None -> err v ("Could not assign to unknown variable named " ^ name)
      | Some value -> Ok value
    )
  | Expression.LOGICAL (l, op, r) ->
      eval_expr env res l >>= fun l_value ->
      eval_expr env res r >>= fun r_value -> eval_logical_expr l_value op r_value
  | Expression.CALL (c, a) -> (
      eval_expr env res c >>= fun func ->
      match func with
      | Value.LOX_CLASS c | Value.LOX_CALLABLE c ->
          if c.arity <> List.length a then
            let t = Expression.hd_token expr in
            err t
              (Printf.sprintf "In call `%s`: Expected %d arguments but got %d" c.name
                 c.arity (List.length a)
              )
          else
            let rec eval_args args =
              match args with
              | [] -> Ok []
              | expr :: rest ->
                  eval_expr env res expr >>= fun value ->
                  eval_args rest >>= fun values -> Ok (value :: values)
            in
            eval_args a >>= fun arg_values -> c.call arg_values
      | o ->
          err (Expression.hd_token expr)
            (Printf.sprintf "Function calls not supported for `%s` of type %s"
               (Value.stringify_result o) (Value.stringify_type o)
            )
    )
  | Expression.THIS keyword -> (
      let depth =
        match Hashtbl.find_opt res expr with
        | None ->
            Printf.sprintf "'this' not resolved" |> print_endline ;
            0
        | Some d -> d
      in
      match Environment.get env depth "this" with
      | None -> err keyword (Printf.sprintf "Could not find 'this' at depth %d" depth)
      | Some value -> Ok value
    )
  | Expression.GET (obj_expr, name) -> (
      eval_expr env res obj_expr >>= fun obj ->
      match obj with
      | Value.LOX_INSTANCE (_, fields) -> (
          match Hashtbl.find_opt fields name.lexeme with
          | Some value -> (
              match value with
              | Value.METHOD m -> Ok (Value.LOX_CALLABLE m)
              | Value.FIELD (Some v) -> Ok v
              | Value.FIELD None -> err name "Field is uninitialized"
            )
          | None ->
              (* TODO: propose other property based on levenshtein distance *)
              err name (Printf.sprintf "Undefined property '%s'" name.lexeme)
        )
      | _ ->
          err name
            (Printf.sprintf "Only instances have properties, got `%s` of type %s"
               (Value.stringify_result obj) (Value.stringify_type obj)
            )
    )
  | Expression.SET (obj, name, value) -> (
      eval_expr env res obj >>= fun obj_val ->
      match obj_val with
      | Value.LOX_INSTANCE (_, fields) ->
          eval_expr env res value >>= fun value_val ->
          Hashtbl.replace fields name.lexeme (Value.FIELD (Some value_val)) ;
          Ok value_val
      | _ ->
          err name
            (Printf.sprintf "Only instances have fields, got `%s` of type %s"
               (Value.stringify_result obj_val)
               (Value.stringify_type obj_val)
            )
    )

type control_flow = RETURN of Value.lox_value | NEXT

(* bubble up return values *)
let ( >>? ) result f =
  result >>= function
  | NEXT -> f ()
  | RETURN v -> Ok (RETURN v)

let rec eval env (res : Resolver.resolution) stmt :
    (control_flow, Error.t list) result =
  match stmt with
  | Statement.PRNT expr ->
      eval_expr env res expr >>= fun value ->
      print_endline (Value.stringify_result value) ;
      Ok NEXT
  | Statement.EXPR expr -> eval_expr env res expr >>= fun _ -> Ok NEXT
  | Statement.VAR_DEF (v, None) ->
      let name = v.lexeme in
      Environment.define env name Value.LOX_NIL ;
      Ok NEXT
  | Statement.VAR_DEF (v, Some expr) ->
      let name = v.lexeme in
      eval_expr env res expr >>= fun value ->
      Environment.define env name value ;
      Ok NEXT
  | Statement.BLOCK stmts ->
      let block_env = Environment.push_scope env in
      eval_block block_env res stmts
  | Statement.IF (cond, then_branch, else_branch) -> (
      eval_expr env res cond >>= fun condition ->
      if Value.is_truthy condition then
        eval env res then_branch >>? fun () -> Ok NEXT
      else
        match else_branch with
        | None -> Ok NEXT
        | Some else_stmt -> eval env res else_stmt >>? fun () -> Ok NEXT
    )
  | Statement.WHILE (cond, body) ->
      let rec loop () =
        eval_expr env res cond >>= fun condition ->
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
                (fun (param : Token.t) arg ->
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
  | Statement.RETURN expr -> eval_expr env res expr >>= fun v -> Ok (RETURN v)
  | Statement.CLASS_DEC (name, body) ->
      let class_env = Environment.push_scope env in
      let method_templates = Hashtbl.create 10 in
      let fields_defaults = Hashtbl.create 10 in

      List.iter
        (fun stmt ->
          match stmt with
          | Statement.FUN_DEF (m_name, m_params, m_body) ->
              Hashtbl.add method_templates m_name.lexeme (m_params, m_body)
          | Statement.VAR_DEF (var_name, None) ->
              Hashtbl.add fields_defaults var_name.lexeme None
          | Statement.VAR_DEF (var_name, Some expr) -> (
              match eval_expr env res expr with
              | Error _ -> Printf.eprintf "Failed to evaluate field initializer\n"
              | Ok result -> Hashtbl.add fields_defaults var_name.lexeme (Some result)
            )
          | _ ->
              Printf.eprintf
                "Only methods or fields are allowed in class body of %s\n"
                name.lexeme
        )
        body ;

      let make_class class_getter =
        let class_constructor : Value.lox_callable =
          {
            name = name.lexeme;
            arity =
              ( match Hashtbl.find_opt method_templates "init" with
              | Some (params, _) -> List.length params
              | None -> 0
              );
            call =
              (fun args ->
                let instance_fields = Hashtbl.create 10 in

                fields_defaults
                |> Hashtbl.iter (fun field_name default_value ->
                       Hashtbl.add instance_fields field_name
                         (Value.FIELD default_value)
                   ) ;

                (* create empty fields that are bound as methods after the instace is
                   created *)
                method_templates
                |> Hashtbl.iter (fun method_name _ ->
                       Hashtbl.add instance_fields method_name (Value.FIELD None)
                   ) ;

                (* create a unique id for the instance *)
                let id = Value.FIELD (Some (Value.LOX_NUM (Random.float 1.0))) in
                Hashtbl.replace instance_fields "<id>" id ;

                let instance =
                  Value.LOX_INSTANCE (class_getter (), instance_fields)
                in

                method_templates
                |> Hashtbl.iter (fun method_name (m_params, m_body) ->
                       let bound_method : Value.lox_callable =
                         {
                           name = method_name;
                           arity = List.length m_params;
                           call =
                             (fun method_args ->
                               let method_env = Environment.push_scope class_env in
                               Environment.define method_env "this" instance ;

                               List.iter2
                                 (fun (param : Token.t) arg ->
                                   Environment.define method_env param.lexeme arg
                                 )
                                 m_params method_args ;

                               eval method_env res (Statement.BLOCK m_body)
                               >>= function
                               | NEXT -> Ok Value.LOX_VOID
                               | RETURN v -> Ok v
                             );
                         }
                       in
                       Hashtbl.replace instance_fields method_name
                         (Value.METHOD bound_method)
                   ) ;

                match Hashtbl.find_opt method_templates "init" with
                | None -> Ok instance
                | Some (init_params, init_body) -> (
                    let init_env = Environment.push_scope class_env in
                    Environment.define init_env "this" instance ;
                    List.iter2
                      (fun (param : Token.t) arg ->
                        Environment.define init_env param.lexeme arg
                      )
                      init_params args ;
                    eval init_env res (Statement.BLOCK init_body) >>= function
                    | NEXT -> Ok instance
                    | RETURN _ -> Ok instance
                  )
              );
          }
        in
        Value.LOX_CLASS class_constructor
      in

      let rec class_value = lazy (make_class (fun () -> Lazy.force class_value)) in
      let final_class = Lazy.force class_value in

      Environment.define env name.lexeme final_class ;
      Ok NEXT

and eval_block env res = function
  | [] -> Ok NEXT
  | [ stmt ] -> eval env res stmt
  | stmt :: rest -> eval env res stmt >>? fun () -> eval_block env res rest

let interpret_ast (env : Environment.t) (ast : Ast.t) :
    (Value.lox_value, Error.t list) result =
  let rec execute_statements res = function
    | [] -> Ok Value.LOX_VOID
    | [ Statement.EXPR expr ] -> eval_expr env res expr
    | stmt :: rest -> eval env res stmt >>= fun _ -> execute_statements res rest
  in
  match ast with
  | Ast.PROGRAM (stmts, res) -> execute_statements res stmts
