open Types
open Lexer

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let err (token : Token.t) msg = Error [ Error.RuntimeError (token.line, msg) ]

let rec find_method (klass : lox_class) name =
  match Hashtbl.find_opt klass.methods name with
  | Some meth -> Some meth
  | None -> (
      match klass.superclass with
      | Some super -> find_method super name
      | None -> None
    )

let bind (meth : lox_callable) (instance : lox_instance) : lox_callable =
  let environment = Environment.push_scope meth.closure in
  Environment.define environment "this" (LOX_INSTANCE instance) ;
  { meth with closure = environment }

let eval_unary_expr (op : Token.t) value : (lox_value, Error.t list) result =
  match op.ttype, value with
  | MINUS, LOX_NUM n -> Ok (LOX_NUM (-.n))
  | MINUS, _ -> err op "Operand must be a number"
  | BANG, value -> Ok (LOX_BOOL (not (Value.is_truthy value)))
  | _ -> err op "Unknown unary operator"

let eval_binary_expr (left_val : lox_value) (op : Token.t) (right_val : lox_value) :
    (lox_value, Error.t list) result =
  match left_val, op.ttype, right_val with
  | LOX_NUM l, PLUS, LOX_NUM r -> Ok (LOX_NUM (l +. r))
  | LOX_NUM l, MINUS, LOX_NUM r -> Ok (LOX_NUM (l -. r))
  | LOX_NUM l, STAR, LOX_NUM r -> Ok (LOX_NUM (l *. r))
  | LOX_NUM l, SLASH, LOX_NUM r ->
      if r = 0.0 then err op "Division by zero" else Ok (LOX_NUM (l /. r))
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

let eval_logical_expr (left_val : lox_value) (op : Token.t) (right_val : lox_value) :
    (lox_value, Error.t list) result =
  match op.ttype with
  | AND -> Ok (if not (Value.is_truthy left_val) then left_val else right_val)
  | OR -> Ok (if Value.is_truthy left_val then left_val else right_val)
  | _ -> err op "Unknown logical operator"

let rec eval_expr (env : environment) (res : Resolver.resolution)
    (expr : Expression.t) : (lox_value, Error.t list) result =
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
        | Some d -> d
        | None -> 0 (* Global variable *)
      in
      (* INFO FOR GEMINI: eval_expr this lands here because this is seen as a
         variable name *)
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
  | Expression.LOGICAL (l, op, r) -> (
      eval_expr env res l >>= fun l_value ->
      match op.ttype with
      | AND ->
          if not (Value.is_truthy l_value) then Ok l_value else eval_expr env res r
      | OR -> if Value.is_truthy l_value then Ok l_value else eval_expr env res r
      | _ -> err op "Unknown logical operator"
    )
  | Expression.CALL (c, a) -> (
      eval_expr env res c >>= fun callee ->
      let rec eval_args args =
        match args with
        | [] -> Ok []
        | expr :: rest ->
            eval_expr env res expr >>= fun value ->
            eval_args rest >>= fun values -> Ok (value :: values)
      in
      eval_args a >>= fun arg_values ->
      match callee with
      | LOX_CALLABLE c ->
          let argc = List.length arg_values in
          if c.arity <> argc then
            err (Expression.hd_token expr)
              (Printf.sprintf "Expected %d arguments but got %d" c.arity argc)
          else
            c.call c.closure arg_values
      | LOX_CLASS cl -> (
          let instance =
            { class_ref = cl; id = Random.bits (); fields = Hashtbl.create 16 }
          in
          let argc = List.length arg_values in
          match find_method cl "init" with
          | None ->
              if argc <> 0 then
                err (Expression.hd_token expr)
                  (Printf.sprintf
                     "Expected 0 arguments for default constructor but got %d" argc
                  )
              else
                Ok (LOX_INSTANCE instance)
          | Some init ->
              if argc <> init.arity then
                err (Expression.hd_token expr)
                  (Printf.sprintf "Expected %d arguments for constructor but got %d"
                     init.arity argc
                  )
              else
                let bound_init = bind init instance in
                bound_init.call bound_init.closure arg_values >>= fun _ ->
                Ok (LOX_INSTANCE instance)
        )
      | o ->
          err (Expression.hd_token expr)
            (Printf.sprintf "Can only call functions and classes. Got %s"
               (Value.stringify_type o)
            )
    )
  | Expression.THIS keyword -> (
      let depth =
        match Hashtbl.find_opt res expr with
        | Some d -> d
        | None -> 0
      in
      match Environment.get env depth "this" with
      | None -> err keyword "Could not find 'this' in the current scope"
      | Some value -> Ok value
    )
  | Expression.SUPER (keyword, method_name) ->
      err keyword "super not implemented yet"
  | Expression.GET (obj_expr, name) -> (
      eval_expr env res obj_expr >>= fun obj ->
      match obj with
      | LOX_INSTANCE instance -> (
          match Hashtbl.find_opt instance.fields name.lexeme with
          | Some field -> Ok field
          | None -> (
              match find_method instance.class_ref name.lexeme with
              | Some meth -> Ok (LOX_CALLABLE (bind meth instance))
              | None ->
                  err name (Printf.sprintf "Undefined property '%s'" name.lexeme)
            )
        )
      | _ ->
          err name
            (Printf.sprintf "Only instances have properties, got %s"
               (Value.stringify_type obj)
            )
    )
  | Expression.SET (obj, name, value) -> (
      eval_expr env res obj >>= fun obj_val ->
      match obj_val with
      | LOX_INSTANCE instance ->
          eval_expr env res value >>= fun value_val ->
          Hashtbl.replace instance.fields name.lexeme value_val ;
          Ok value_val
      | _ ->
          err name
            (Printf.sprintf "Only instances have fields, got %s"
               (Value.stringify_type obj_val)
            )
    )

type control_flow = RETURN of lox_value | NEXT

let ( >>? ) result f =
  result >>= function
  | NEXT -> f ()
  | RETURN v -> Ok (RETURN v)

let rec eval (env : environment) (res : Resolver.resolution) stmt :
    (control_flow, Error.t list) result =
  match stmt with
  | Statement.PRNT expr ->
      eval_expr env res expr >>= fun value ->
      print_endline (Value.stringify_result value) ;
      Ok NEXT
  | Statement.EXPR expr -> eval_expr env res expr >>= fun _ -> Ok NEXT
  | Statement.VAR_DEF (v, None) ->
      Environment.define env v.lexeme LOX_NIL ;
      Ok NEXT
  | Statement.VAR_DEF (v, Some expr) ->
      eval_expr env res expr >>= fun value ->
      Environment.define env v.lexeme value ;
      Ok NEXT
  | Statement.BLOCK stmts ->
      let block_env = Environment.push_scope env in
      eval_block block_env res stmts
  | Statement.IF (cond, then_branch, else_branch) -> (
      eval_expr env res cond >>= fun condition ->
      if Value.is_truthy condition then
        eval env res then_branch
      else
        match else_branch with
        | None -> Ok NEXT
        | Some else_stmt -> eval env res else_stmt
    )
  | Statement.WHILE (cond, body) ->
      let rec loop () =
        eval_expr env res cond >>= fun condition ->
        if Value.is_truthy condition then
          eval env res body >>? loop
        else
          Ok NEXT
      in
      loop ()
  (* TODO: desugar FOR statement in Parser not in Interpreter *)
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
      let func : lox_callable =
        {
          name = name.lexeme;
          arity = List.length params;
          closure = env;
          is_initializer = false;
          call =
            (fun exec_env args ->
              let func_env = Environment.push_scope exec_env in
              List.iter2
                (fun (p : Token.t) a -> Environment.define func_env p.lexeme a)
                params args ;
              eval_block func_env res body >>= function
              | NEXT -> Ok LOX_VOID
              | RETURN v -> Ok v
            );
        }
      in
      let fn = LOX_CALLABLE func in
      Environment.define env name.lexeme fn ;
      Ok NEXT
  | Statement.RETURN expr -> eval_expr env res expr >>= fun v -> Ok (RETURN v)
  | Statement.CLASS_DEC (name, super, method_stmts) ->
      let superclass =
        match super with
        | None -> Ok None
        | Some expr -> (
            eval_expr env res expr >>= function
            | LOX_CLASS s -> Ok (Some s)
            | _ -> err (Expression.hd_token expr) "Superclass must be a class"
          )
      in
      superclass >>= fun sc_opt ->
      Environment.define env name.lexeme LOX_NIL ;
      (* Allow class to refer to itself *)
      let class_env =
        match sc_opt with
        | Some sc ->
            let env' = Environment.push_scope env in
            Environment.define env' "super" (LOX_CLASS sc) ;
            env'
        | None -> env
      in
      let methods = Hashtbl.create 10 in
      method_stmts
      |> List.iter (function
           | Statement.FUN_DEF (m_name, m_params, m_body) ->
               let is_init = m_name.lexeme = "init" in
               let meth : lox_callable =
                 {
                   name = m_name.lexeme;
                   arity = List.length m_params;
                   closure = class_env;
                   is_initializer = is_init;
                   call =
                     (fun method_env args ->
                       List.iter2
                         (fun (p : Token.t) a ->
                           Environment.define method_env p.lexeme a
                         )
                         m_params args ;
                       eval_block method_env res m_body >>= function
                       (* TODO: is wrong right now *)
                       | RETURN v ->
                           if is_init then
                             let d = List.length method_env - 1 in
                             Environment.get method_env d "this"
                             |> Option.get |> Result.ok
                           else
                             Ok v
                       | NEXT ->
                           if is_init then
                             let d = List.length method_env - 1 in
                             Environment.get method_env d "this"
                             |> Option.get |> Result.ok
                           else
                             Ok LOX_VOID
                     );
                 }
               in
               Hashtbl.add methods m_name.lexeme meth
           | _ -> ()
           ) ;

      let klass = { name = name.lexeme; superclass = sc_opt; methods } in
      Environment.assign env name.lexeme (LOX_CLASS klass) |> ignore ;
      Ok NEXT

and eval_block env res = function
  | [] -> Ok NEXT
  | stmt :: rest -> eval env res stmt >>? fun () -> eval_block env res rest

let interpret_ast (env : environment) (ast : Ast.t) :
    (lox_value, Error.t list) result =
  let rec execute_statements res = function
    | [] -> Ok LOX_VOID
    | [ Statement.EXPR expr ] -> eval_expr env res expr
    | stmt :: rest -> (
        eval env res stmt >>= function
        | NEXT -> execute_statements res rest
        | RETURN _ ->
            err (Statement.hd_token stmt) "Cannot return from top-level code."
      )
  in
  match ast with
  | Ast.PROGRAM (stmts, res) -> execute_statements res stmts
