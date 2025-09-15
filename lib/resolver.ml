type function_type = NONE | FUNCTION | METHOD | INITIALIZER
type class_type = NO_CLASS | CLASS | SUBCLASS

type resolver_state = {
  scopes : (string, bool) Hashtbl.t Stack.t;
  locals : (Expression.expr, int) Hashtbl.t;
  current_function : function_type ref;
  current_class : class_type ref;
}

let create () =
  {
    scopes = Stack.create ();
    locals = Hashtbl.create 32;
    current_function = ref NONE;
    current_class = ref NO_CLASS;
  }

(* Resolver Helper Functions *)
(* type scope = { variables : (string, unit) Hashtbl.t; level : int } *)
(* let scopes = ref (Stack.create ()) *)

let begin_scope resolver = Stack.push (Hashtbl.create 32) resolver.scopes
let end_scope resolver = Stack.drop resolver.scopes

let declare resolver name =
  if not (Stack.is_empty resolver.scopes) then (
    let current_scope = Stack.top resolver.scopes in
    if Hashtbl.mem current_scope name then
      Printf.eprintf
        "Warning: Variable `%s` already declared in this scope. Overwriting.\n" name
    else
      print_endline ("[res] Declaring variable: " ^ name) ;
    Hashtbl.replace current_scope name false
  )

let define resolver name =
  if not (Stack.is_empty resolver.scopes) then (
    let current_scope = Stack.top resolver.scopes in
    Hashtbl.replace current_scope name true ;
    print_endline
      ("[res] Defining variable: " ^ name ^ " at depth "
      ^ string_of_int (Stack.length resolver.scopes - 1)
      )
  )

let resolve_local resolver expr (name : string) =
  if name = "this" && !(resolver.current_class) = NO_CLASS then
    failwith "Cannot use 'this' outside of a class" ;
  (* scopes from innermost to outermost *)
  let scopes_list = Stack.fold (fun acc scope -> scope :: acc) [] resolver.scopes in
  let rec find_in_scopes scopes depth : unit =
    match scopes with
    | [] -> ()
    | scope :: rest -> (
        Hashtbl.mem scope name |> function
        | true ->
            Printf.printf "[resolver] Resolving %s at depth %d\n" name depth ;
            Hashtbl.replace resolver.locals expr depth
        | false -> find_in_scopes rest (depth + 1)
      )
  in
  find_in_scopes scopes_list 0

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
      resolve_expr resolver condition ;
      resolve_stmt resolver then_branch ;
      match else_branch with
      | Some stmt -> resolve_stmt resolver stmt
      | None -> ()
    )
  | WHILE (condition, body) ->
      resolve_expr resolver condition ;
      resolve_stmt resolver body
  | FOR (init, condition, increment, body) ->
      resolve_stmt resolver init ;
      ( match condition with
      | Some expr -> resolve_expr resolver expr
      | None -> ()
      ) ;
      ( match increment with
      | Some expr -> resolve_expr resolver expr
      | None -> ()
      ) ;
      resolve_stmt resolver body
  | RETURN expr ->
      if !(resolver.current_function) = NONE then
        failwith "Cannot return from top-level code" ;
      if !(resolver.current_function) = INITIALIZER then
        failwith "Cannot return a value from an initializer" ;
      resolve_expr resolver expr

and resolve_block resolver stmts =
  begin_scope resolver ;
  List.iter (resolve_stmt resolver) stmts ;
  end_scope resolver

and resolve_var_declaration resolver name init =
  declare resolver name.Lexer.Token.lexeme ;
  ( match init with
  | None -> ()
  | Some expr -> resolve_expr resolver expr
  ) ;
  define resolver name.lexeme

and resolve_function resolver name params body func_type =
  let enclosing_function = !(resolver.current_function) in

  declare resolver name.Lexer.Token.lexeme ;
  define resolver name.lexeme ;

  resolver.current_function := func_type ;

  begin_scope resolver ;

  (* For methods, declare 'this' in the function scope *)
  ( match func_type with
  | METHOD | INITIALIZER ->
      let this_scope = Stack.top resolver.scopes in
      Hashtbl.replace this_scope "this" true ;
      print_endline ("[res] Declared 'this' in method: " ^ name.Lexer.Token.lexeme)
  | _ -> ()
  ) ;

  (* Declare parameters *)
  List.iter
    (fun param ->
      declare resolver param.Lexer.Token.lexeme ;
      define resolver param.lexeme
    )
    params ;

  (* Resolve function body *)
  List.iter (resolve_stmt resolver) body ;

  print_endline ("[res] statements are: " ^ string_of_int (List.length body)) ;
  end_scope resolver ;
  resolver.current_function := enclosing_function

and resolve_class resolver name methods =
  let enclosing_class = !(resolver.current_class) in
  resolver.current_class := CLASS ;

  declare resolver name.Lexer.Token.lexeme ;
  define resolver name.lexeme ;

  begin_scope resolver ;

  List.iter
    (fun method_stmt ->
      match method_stmt with
      | FUN_DEF (method_name, params, body) ->
          let func_type =
            if method_name.lexeme = "init" then INITIALIZER else METHOD
          in
          print_endline ("[res] Resolving method: " ^ method_name.Lexer.Token.lexeme) ;
          resolve_function resolver method_name params body func_type
      | VAR_DEF (name, None) -> resolve_var_declaration resolver name None
      | VAR_DEF (name, Some init) -> resolve_var_declaration resolver name (Some init)
      | _ -> failwith "Only method declarations allowed in class body"
    )
    methods ;

  end_scope resolver ;
  resolver.current_class := enclosing_class

and resolve_expr resolver = function
  | VARIABLE name ->
      ( if not (Stack.is_empty resolver.scopes) then
          let scope = Stack.top resolver.scopes in
          match Hashtbl.find_opt scope name.lexeme with
          | Some false ->
              failwith
                ("Cannot read local variable in its own initializer: " ^ name.lexeme)
          | _ -> ()
      ) ;
      print_endline ("[res] Resolving variable in resolve_expr: " ^ name.lexeme) ;
      resolve_local resolver (VARIABLE name) name.lexeme
  | ASSIGN (name, value) ->
      resolve_expr resolver value ;
      resolve_local resolver (ASSIGN (name, value)) name.lexeme
  | BINARY (left, _, right) ->
      resolve_expr resolver left ;
      resolve_expr resolver right
  | LOGICAL (left, _, right) ->
      resolve_expr resolver left ;
      resolve_expr resolver right
  | UNARY (_, expr) -> resolve_expr resolver expr
  | GROUPING expr -> resolve_expr resolver expr
  | CALL (callee, args) ->
      resolve_expr resolver callee ;
      List.iter (resolve_expr resolver) args
  | GET (object_expr, _) -> resolve_expr resolver object_expr
  | SET (object_expr, _, value) ->
      resolve_expr resolver object_expr ;
      resolve_expr resolver value
  | LITERAL _ -> ()
  | THIS keyword ->
      if !(resolver.current_class) = NO_CLASS then
        failwith "Cannot use 'this' outside of a class" ;
      resolve_local resolver (THIS keyword) keyword.lexeme

type resolution = (Expression.expr, int) Hashtbl.t

let resolve statements : (resolution, Error.t list) result =
  let resolver = create () in
  resolver.scopes |> Stack.push (Hashtbl.create 32) ;
  List.iter (resolve_stmt resolver) statements ;
  Ok resolver.locals
