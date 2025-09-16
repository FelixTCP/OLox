type function_type = NONE | FUNCTION | METHOD | INITIALIZER
type class_type = NO_CLASS | CLASS | SUBCLASS

type resolver_state = {
  scopes : (string, bool) Hashtbl.t Stack.t;
  locals : (Expression.expr, int) Hashtbl.t;
  current_function : function_type ref;
  current_class : class_type ref;
}

(* Resolver Helper functions *)
let parse_error (token : Lexer.Token.token) msg = Error.ParseError (token.line, msg)

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let ( >>! ) result f =
  match result with
  | Ok _ -> f
  | Error e -> Error e

let create () =
  {
    scopes = Stack.create ();
    locals = Hashtbl.create 32;
    current_function = ref NONE;
    current_class = ref NO_CLASS;
  }

let begin_scope resolver = Stack.push (Hashtbl.create 32) resolver.scopes
let end_scope resolver = Stack.drop resolver.scopes

let declare resolver name =
  if not (Stack.is_empty resolver.scopes) then
    let current_scope = Stack.top resolver.scopes in
    if Hashtbl.mem current_scope name then
      Printf.eprintf
        "Warning: Variable `%s` already declared in this scope. Overwriting.\n" name
    else
      Hashtbl.replace current_scope name false

let define resolver name =
  if not (Stack.is_empty resolver.scopes) then
    let current_scope = Stack.top resolver.scopes in
    Hashtbl.replace current_scope name true

let resolve_local resolver expr (name : string) (token : Lexer.Token.token) :
    (unit, Error.t list) result =
  if name = "this" && !(resolver.current_class) = NO_CLASS then
    Error [ parse_error token "Cannot use 'this' outside of a class" ]
  else (* scopes from innermost to outermost *)
    let scopes_list =
      Stack.fold (fun acc scope -> scope :: acc) [] resolver.scopes
    in
    let rec find_in_scopes scopes depth : unit =
      match scopes with
      | [] -> ()
      | scope :: rest -> (
          Hashtbl.mem scope name |> function
          | true -> Hashtbl.replace resolver.locals expr depth
          | false -> find_in_scopes rest (depth + 1)
        )
    in
    find_in_scopes scopes_list 0 ;
    Ok ()

open Statement

let rec resolve_stmt resolver = function
  | BLOCK stmts -> resolve_block resolver stmts
  | VAR_DEF (name, init) -> resolve_var_declaration resolver name init
  | FUN_DEF (name, params, body) ->
      resolve_function resolver name params body FUNCTION
  | CLASS_DEC (name, methods) -> resolve_class resolver name methods
  | EXPR expr -> resolve_expr resolver expr
  | PRNT expr -> resolve_expr resolver expr
  | IF (condition, then_branch, else_branch) -> (
      resolve_expr resolver condition
      >>! resolve_stmt resolver then_branch
      >>!
      match else_branch with
      | Some stmt -> resolve_stmt resolver stmt
      | None -> Ok ()
    )
  | WHILE (condition, body) ->
      resolve_expr resolver condition >>! resolve_stmt resolver body
  | FOR (init, condition, increment, body) ->
      resolve_stmt resolver init
      >>! ( match condition with
          | Some expr -> resolve_expr resolver expr
          | None -> Ok ()
          )
      >>! ( match increment with
          | Some expr -> resolve_expr resolver expr
          | None -> Ok ()
          )
      >>! resolve_stmt resolver body
  | RETURN expr -> (
      match !(resolver.current_function) with
      | NONE ->
          Error
            [
              parse_error
                (Lexer.Token.make_eof_token ())
                "Cannot return from top-level code";
            ]
      | INITIALIZER ->
          Error
            [
              parse_error
                (Lexer.Token.make_eof_token ())
                "Cannot return from top-level code";
            ]
      | _ -> resolve_expr resolver expr
    )

and resolve_block resolver stmts : (unit, Error.t list) result =
  begin_scope resolver ;
  let rec resolve_stmts = function
    | [] -> Ok ()
    | stmt :: rest -> resolve_stmt resolver stmt >>! resolve_stmts rest
  in
  let result = resolve_stmts stmts in
  end_scope resolver ;
  result

and resolve_var_declaration resolver name init : (unit, Error.t list) result =
  declare resolver name.Lexer.Token.lexeme ;
  ( match init with
  | None -> Ok ()
  | Some expr -> resolve_expr resolver expr
  )
  >>= fun () ->
  define resolver name.lexeme ;
  Ok ()

and resolve_function resolver name params body func_type :
    (unit, Error.t list) result =
  let enclosing_function = !(resolver.current_function) in

  declare resolver name.Lexer.Token.lexeme ;
  define resolver name.lexeme ;

  resolver.current_function := func_type ;

  begin_scope resolver ;

  ( match func_type with
  | METHOD | INITIALIZER ->
      let this_scope = Stack.top resolver.scopes in
      Hashtbl.replace this_scope "this" true
  | _ -> ()
  ) ;

  List.iter
    (fun param ->
      declare resolver param.Lexer.Token.lexeme ;
      define resolver param.lexeme
    )
    params ;

  let rec resolve_body_stmts = function
    | [] -> Ok ()
    | stmt :: rest ->
        resolve_stmt resolver stmt >>= fun () -> resolve_body_stmts rest
  in

  let result = resolve_body_stmts body in
  end_scope resolver ;
  resolver.current_function := enclosing_function ;
  result

and resolve_class resolver name methods : (unit, Error.t list) result =
  let enclosing_class = !(resolver.current_class) in
  resolver.current_class := CLASS ;

  declare resolver name.Lexer.Token.lexeme ;
  define resolver name.lexeme ;

  begin_scope resolver ;

  let rec resolve_methods = function
    | [] -> Ok ()
    | method_stmt :: rest -> (
        match method_stmt with
        | FUN_DEF (method_name, params, body) ->
            let func_type =
              if method_name.lexeme = "init" then INITIALIZER else METHOD
            in
            resolve_function resolver method_name params body func_type
            >>! resolve_methods rest
        | VAR_DEF (name, None) ->
            resolve_var_declaration resolver name None >>! resolve_methods rest
        | VAR_DEF (name, Some init) ->
            resolve_var_declaration resolver name (Some init)
            >>! resolve_methods rest
        | _ ->
            Error
              [
                parse_error
                  (Lexer.Token.make_eof_token ())
                  "Only field and method declarations allowed in class body";
              ]
      )
  in

  let result = resolve_methods methods in
  end_scope resolver ;
  resolver.current_class := enclosing_class ;
  result

and resolve_expr resolver : Expression.expr -> (unit, Error.t list) result = function
  | VARIABLE name ->
      ( if not (Stack.is_empty resolver.scopes) then
          let scope = Stack.top resolver.scopes in
          match Hashtbl.find_opt scope name.lexeme with
          | Some false ->
              Error
                [
                  parse_error name
                    (Printf.sprintf
                       "Cannot read local variable `%s` in its own initializer"
                       name.lexeme
                    );
                ]
          | _ -> Ok ()
        else
          Ok ()
      )
      >>! resolve_local resolver (VARIABLE name) name.lexeme name
  | ASSIGN (name, value) ->
      resolve_expr resolver value
      >>! resolve_local resolver (ASSIGN (name, value)) name.lexeme name
  | BINARY (left, _, right) ->
      resolve_expr resolver left >>! resolve_expr resolver right
  | LOGICAL (left, _, right) ->
      resolve_expr resolver left >>! resolve_expr resolver right
  | UNARY (_, expr) -> resolve_expr resolver expr
  | GROUPING expr -> resolve_expr resolver expr
  | CALL (callee, args) ->
      resolve_expr resolver callee
      >>!
      let rec resolve_args = function
        | [] -> Ok ()
        | arg :: rest -> resolve_expr resolver arg >>! resolve_args rest
      in
      resolve_args args
  | GET (object_expr, _) -> resolve_expr resolver object_expr
  | SET (object_expr, _, value) ->
      resolve_expr resolver object_expr >>! resolve_expr resolver value
  | LITERAL _ -> Ok ()
  | THIS keyword ->
      if !(resolver.current_class) = NO_CLASS then
        Error [ parse_error keyword "Cannot use 'this' outside of a class" ]
      else
        resolve_local resolver (THIS keyword) keyword.lexeme keyword

type resolution = (Expression.expr, int) Hashtbl.t

let resolve statements : (resolution, Error.t list) result =
  let resolver = create () in
  resolver.scopes |> Stack.push (Hashtbl.create 32) ;
  let rec resolve_all_stmts = function
    | [] -> Ok ()
    | stmt :: rest -> resolve_stmt resolver stmt >>= fun () -> resolve_all_stmts rest
  in
  resolve_all_stmts statements >>= fun () -> Ok resolver.locals
