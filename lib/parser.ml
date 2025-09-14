module Expression = struct
  type expr =
    | LITERAL of Lexer.Token.literal_type
    | UNARY of Lexer.Token.token * expr
    | BINARY of expr * Lexer.Token.token * expr
    | LOGICAL of expr * Lexer.Token.token * expr
    | GROUPING of expr
    | VARIABLE of Lexer.Token.token
    | ASSIGN of Lexer.Token.token * expr
    | CALL of expr * expr list
end

module Resolution = struct
  type resolution_info = { locals : (Expression.expr, int) Hashtbl.t }

  let current_resolution = ref None
  let create () = { locals = Hashtbl.create 32 }

  let init () =
    let info = create () in
    current_resolution := Some info ;
    info

  let get_current () =
    match !current_resolution with
    | Some info -> info
    | None -> failwith "Resolution not initialized"

  let resolve expr depth =
    let info = get_current () in
    Hashtbl.replace info.locals expr depth

  let lookup expr =
    let info = get_current () in
    Hashtbl.find_opt info.locals expr

  let get_resolution_info () = get_current ()
end

module Statement = struct
  type stmt =
    | EXPR of Expression.expr
    | PRNT of Expression.expr
    | VAR_DEF of Lexer.Token.token * Expression.expr option
    | BLOCK of stmt list
    | IF of Expression.expr * stmt * stmt option
    | WHILE of Expression.expr * stmt
    | FOR of stmt * Expression.expr option * Expression.expr option * stmt
    | FUN_DEF of Lexer.Token.token * Lexer.Token.token list * stmt list
    | RETURN of Expression.expr
end

module Parser = struct
  open Lexer
  open Expression

  (* Resolver Helper Functions *)
  type scope = { variables : (string, unit) Hashtbl.t; level : int }

  let scopes = ref (Stack.create ())

  let begin_scope () =
    let new_level = (Stack.top !scopes).level + 1 in
    let new_scope = { variables = Hashtbl.create 32; level = new_level } in
    Stack.push new_scope !scopes

  let end_scope () = Stack.drop !scopes

  let resolve_var name expr =
    let scope_list = Stack.fold (fun acc s -> s :: acc) [] !scopes |> List.rev in
    let rec find_var scopes depth =
      match scopes with
      | [] -> ()
      | scope :: rest ->
          if Hashtbl.mem scope.variables name then
            Resolution.resolve expr depth
          else
            find_var rest (depth - 1)
    in
    find_var scope_list (List.length scope_list - 1)

  let new_var name =
    let current_scope = Stack.top !scopes in
    (* TODO: decide, whether redeclaration inside the same scope should be allowed *)
    (* if Hashtbl.mem current_scope.variables name then *)
    (*   Printf.eprintf *)
    (*     "Warning: Variable `%s` already declared in this scope. Overwriting.\n" name *)
    Hashtbl.replace current_scope.variables name ()

  (* Parser Helper functions *)
  let parse_error (token : Token.token) msg = Error.ParseError (token.line, msg)

  let ( >>= ) result f =
    match result with
    | Ok x -> f x
    | Error e -> Error e

  let expect_token token_type tokens msg =
    match (tokens : Token.token list) with
    | t :: rest when t.ttype = token_type -> Ok (t, rest)
    | t :: _ -> Error [ parse_error t msg ]
    | [] -> Error [ parse_error (Token.make_eof_token ()) msg ]

  let expect_statement tokens =
    expect_token SEMICOLON tokens
      (Printf.sprintf "Expected ';' after `%s` to end the statement"
         (List.hd tokens).lexeme
      )

  let parse_binary_left_assoc_expression operand_parser operator_types
      ?(logical = false) tokens : (expr * Token.token list, Error.t list) result =
    operand_parser tokens >>= fun (left, rest) ->
    let rec loop l tkns : (expr * Token.token list, Error.t list) result =
      match (tkns : Token.token list) with
      | ({ ttype; _ } as t) :: rest' when List.mem ttype operator_types ->
          operand_parser rest' >>= fun (right, rest'') ->
          if logical then
            loop (LOGICAL (l, t, right)) rest''
          else
            loop (BINARY (l, t, right)) rest''
      | _ -> Ok (l, tkns)
    in
    loop left rest

  (* Main parsing functions *)
  let rec program (tokens : Token.token list) :
      (Statement.stmt list, Error.t list) result =
    let rec parse_stmt acc (tkns : Token.token list) =
      if tkns = [] then
        Error [ parse_error (Token.make_eof_token ()) "Unexpected end of input" ]
      else if (List.hd tkns).ttype = EOF then
        Ok (List.rev acc)
      else
        declaration tkns >>= fun (stmt, rest) -> parse_stmt (stmt :: acc) rest
    in
    parse_stmt [] tokens

  and declaration = function
    | { ttype = VAR; _ } :: rest -> var_declaration rest
    | { ttype = FUN; _ } :: rest -> fun_declaration rest
    | _ as tokens -> statement tokens

  and var_declaration = function
    | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = EQUAL; _ } :: rest ->
        expression rest >>= fun (expr, rest') ->
        expect_statement rest' >>= fun (_, rest'') ->
        new_var id.lexeme ;
        Ok (Statement.VAR_DEF (id, Some expr), rest'')
    | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = SEMICOLON; _ } :: rest ->
        new_var id.lexeme ;
        Ok (VAR_DEF (id, None), rest)
    | { ttype = IDENTIFIER; _ } :: t :: _rest ->
        Error
          [
            parse_error t
              ("Expected '=' in variable declaration but found " ^ t.lexeme);
          ]
    | t :: _ ->
        Error
          [
            parse_error t
              ("Expected identifier in variable declaration but found " ^ t.lexeme);
          ]
    | [] ->
        Error
          [
            parse_error (Token.make_eof_token ())
              "Expected identifier in variable declaration but found nothing";
          ]

  and fun_declaration (tokens : Token.token list) =
    func tokens >>= fun (function_stmt, rest) -> Ok (function_stmt, rest)

  and func = function
    | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = LEFT_PAR; _ } :: rest ->
        parameters rest >>= fun (params, rest') ->
        expect_token LEFT_BRA rest' "Expected '{' after parameter list"
        >>= fun (_, rest'') ->
        block rest'' >>= fun (body, rest''') ->
        new_var id.lexeme ;
        Ok (Statement.FUN_DEF (id, params, body), rest''')
    | { ttype = IDENTIFIER; _ } :: other :: _ ->
        Error
          [
            parse_error other
              "Expected '(' to start parameter list afer identifier in function \
               declaration";
          ]
    | t :: _ ->
        Error
          [ parse_error t "Expected identifier after 'fun' in function declaration" ]
    | [] ->
        Error
          [
            parse_error (Token.make_eof_token ())
              "Expected identifier after 'fun' in function declaration but found EOF";
          ]

  and parameters tokens : (Token.token list * Token.token list, Error.t list) result
      =
    let rec parse_params acc (tkns : Token.token list) =
      if List.length acc > 255 then
        Error
          [
            parse_error (List.hd tkns)
              "Cannot have more than 255 parameters in a function declaration";
          ]
      else
        match tkns with
        | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev acc, rest)
        | ({ ttype = IDENTIFIER; _ } as t) :: { ttype = RIGHT_PAR; _ } :: rest ->
            Ok (List.rev (t :: acc), rest)
        | ({ ttype = IDENTIFIER; _ } as t) :: rest ->
            expect_token COMMA rest "Expected ',' between parameters"
            >>= fun (_, rest') -> parse_params (t :: acc) rest'
        | t :: _ ->
            Error
              [
                parse_error t
                  (Printf.sprintf
                     "Expected Unterminated parameter list. Expected ')' but found \
                      ` `."
                  ^ t.lexeme
                  );
              ]
        | [] ->
            Error
              [
                parse_error (Token.make_eof_token ())
                  "Unterminated parameter list. Expected ')' but found EOF";
              ]
    in
    parse_params [] tokens

  and statement = function
    | { ttype = PRINT; _ } :: rest -> print_statement rest
    | { ttype = LEFT_BRA; _ } :: rest ->
        block rest >>= fun (stmts, rest') -> Ok (Statement.BLOCK stmts, rest')
    | { ttype = IF; _ } :: rest -> if_statement rest
    | { ttype = WHILE; _ } :: rest -> while_statement rest
    | { ttype = FOR; _ } :: rest -> for_statement rest
    | { ttype = RETURN; _ } :: rest -> return_statement rest
    | _ as tokens -> expression_statement tokens

  and print_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    expression tokens >>= fun (expr, rest) ->
    expect_statement rest >>= fun (_, rest') -> Ok (Statement.PRNT expr, rest')

  and expression_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    expression tokens >>= fun (expr, rest) ->
    expect_statement rest >>= fun (_, rest') -> Ok (Statement.EXPR expr, rest')

  and block (tokens : Token.token list) :
      (Statement.stmt list * Token.token list, Error.t list) result =
    let rec parse_block acc (tkns : Token.token list) =
      match tkns with
      | [] ->
          Error
            [
              parse_error (Token.make_eof_token ())
                "Unterminated block. Expected '}' before end of input.";
            ]
      | { ttype = RIGHT_BRA; _ } :: rest ->
          end_scope () ;
          Ok (List.rev acc, rest)
      | _ -> declaration tkns >>= fun (stmt, rest) -> parse_block (stmt :: acc) rest
    in
    begin_scope () ;
    parse_block [] tokens

  and return_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    expression tokens >>= fun (expr, rest) ->
    expect_statement rest >>= fun (_, rest') -> Ok (Statement.RETURN expr, rest')

  and if_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    match tokens with
    | { ttype = LEFT_PAR; _ } :: rest -> (
        expression rest >>= fun (condition, rest') ->
        expect_token RIGHT_PAR rest' "Expected ')' after if condition"
        >>= fun (_, rest'') ->
        statement rest'' >>= fun (then_branch, rest''') ->
        match rest''' with
        | { ttype = ELSE; _ } :: rest4 ->
            statement rest4 >>= fun (else_branch, rest5) ->
            Ok (Statement.IF (condition, then_branch, Some else_branch), rest5)
        | _ -> Ok (Statement.IF (condition, then_branch, None), rest''')
      )
    | t :: _ -> Error [ parse_error t "Expected '(' after if-statement" ]
    | [] ->
        Error
          [
            parse_error (Token.make_eof_token ())
              "Expected '(' after if-statement but reached EOF";
          ]

  and while_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    match tokens with
    | { ttype = LEFT_PAR; _ } :: rest ->
        expression rest >>= fun (condition, rest') ->
        expect_token RIGHT_PAR rest' "Expected ')' after while condition"
        >>= fun (_, rest'') ->
        statement rest'' >>= fun (stmt, rest''') ->
        Ok (Statement.WHILE (condition, stmt), rest''')
    | t :: _ -> Error [ parse_error t "Expected '(' after while-statement" ]
    | [] ->
        Error
          [
            parse_error (Token.make_eof_token ())
              "Expected '(' after while-statement but reached EOF";
          ]

  and for_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    let parse_statement init cond incr (tokens : Token.token list) =
      expect_token RIGHT_PAR tokens "Expected ')' after for condition"
      >>= fun (_, rest) ->
      statement rest >>= fun (stmt, rest') ->
      Ok (Statement.FOR (init, cond, incr, stmt), rest')
    in
    let parse_increment init cond (tokens : Token.token list) =
      match tokens with
      | { ttype = SEMICOLON; _ } :: rest ->
          let incr = None in
          parse_statement init cond incr rest
      | _ ->
          expression tokens >>= fun (incr_expr, rest) ->
          let incr = Some incr_expr in
          parse_statement init cond incr rest
    in
    let parse_condition init (tokens : Token.token list) =
      match tokens with
      | { ttype = SEMICOLON; _ } :: rest ->
          let cond = None in
          parse_increment init cond rest
      | _ ->
          expression tokens >>= fun (cond_expr, rest) ->
          expect_statement rest >>= fun (_, rest') ->
          let cond = Some cond_expr in
          parse_increment init cond rest'
    in
    match tokens with
    | { ttype = LEFT_PAR; _ } :: rest -> (
        match rest with
        | { ttype = SEMICOLON; _ } :: rest' ->
            let init = Statement.EXPR (LITERAL L_NIL) in
            parse_condition init rest'
        | { ttype = VAR; _ } :: rest' ->
            var_declaration rest' >>= fun (init, rest'') ->
            parse_condition init rest''
        | _ ->
            expression_statement rest >>= fun (init, rest') ->
            parse_condition init rest'
      )
    | t :: _ -> Error [ parse_error t "Expected '(' after for-statement" ]
    | [] ->
        Error
          [
            parse_error (Token.make_eof_token ())
              "Expected '(' after for-statement but reached EOF";
          ]

  and arguments tokens : (expr list * Token.token list, Error.t list) result =
    let rec parse_args acc (tkns : Token.token list) =
      match tkns with
      | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev acc, rest)
      | [] ->
          Error
            [
              parse_error (Token.make_eof_token ())
                "Unterminated argument list. Expected ')' before end of input";
            ]
      | _ -> (
          expression tkns >>= fun (arg, rest) ->
          match rest with
          | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev (arg :: acc), rest)
          | _ ->
              expect_token Token.COMMA rest "Expected ',' between arguments"
              >>= fun (_, rest') ->
              if List.length acc > 255 then
                Error
                  [
                    parse_error (List.hd tkns)
                      "Cannot have more than 255 arguments in a function call";
                  ]
              else
                parse_args (arg :: acc) rest'
        )
    in
    parse_args [] tokens

  and expression tokens : (expr * Token.token list, Error.t list) result =
    assignment tokens

  and assignment tokens : (expr * Token.token list, Error.t list) result =
    match tokens with
    | { ttype = IDENTIFIER; _ } :: { ttype = EQUAL; _ } :: rest ->
        let var_token = List.hd tokens in
        assignment rest >>= fun (value, rest') ->
        print_endline
          (Printf.sprintf "Resolving assignment to variable `%s`" var_token.lexeme) ;
        resolve_var var_token.lexeme (VARIABLE var_token) ;
        Ok (ASSIGN (var_token, value), rest')
    | _ -> logic_or tokens

  and logic_or tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression logic_and [ OR ] tokens ~logical:true

  and logic_and tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression equality [ AND ] tokens ~logical:true

  and equality tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression comparison [ BANG_EQUAL; EQUAL_EQUAL ] tokens

  and comparison tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression term
      [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ]
      tokens

  and term tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression factor [ PLUS; MINUS ] tokens

  and factor tokens : (expr * Token.token list, Error.t list) result =
    parse_binary_left_assoc_expression unary [ STAR; SLASH ] tokens

  and unary tokens : (expr * Token.token list, Error.t list) result =
    match tokens with
    | ({ ttype = BANG | MINUS; _ } as t) :: rest ->
        unary rest >>= fun (expr, rest') -> Ok (UNARY (t, expr), rest')
    | _ -> call tokens

  and call tokens : (expr * Token.token list, Error.t list) result =
    primary tokens >>= fun (expr, rest) ->
    match rest with
    | { ttype = LEFT_PAR; _ } :: rest' ->
        arguments rest' >>= fun (args, rest'') -> Ok (CALL (expr, args), rest'')
    | _ -> Ok (expr, rest)

  and primary tokens : (expr * Token.token list, Error.t list) result =
    match tokens with
    | { ttype = NIL; _ } :: rest -> Ok (LITERAL L_NIL, rest)
    | ({ ttype = STRING; _ } as t) :: rest -> Ok (LITERAL (L_STRING t.lexeme), rest)
    | ({ ttype = NUMBER; _ } as t) :: rest -> (
        try
          let num = Float.of_string t.lexeme in
          Ok (LITERAL (L_NUM num), rest)
        with Failure _ ->
          Error [ parse_error t ("Invalid number format: " ^ t.lexeme) ]
      )
    | ({ ttype = TRUE | FALSE; _ } as t) :: rest ->
        let lit = if t.ttype = TRUE then true else false in
        Ok (LITERAL (L_BOOL lit), rest)
    | ({ ttype = IDENTIFIER; _ } as t) :: rest ->
        resolve_var t.lexeme (VARIABLE t) ;
        Ok (VARIABLE t, rest)
    | { ttype = LEFT_PAR; _ } :: rest ->
        expression rest >>= fun (expr, rest') ->
        expect_token Token.RIGHT_PAR rest' "Expect ')' after expression"
        >>= fun (_, rest'') -> Ok (GROUPING expr, rest'')
    | t :: rest ->
        let found =
          (* <= 1 because of EOF token at the end *)
          if List.length rest <= 1 then "none" else (List.hd rest).lexeme
        in
        Error
          [
            parse_error t
              ("Expected expression after " ^ t.lexeme ^ " but found " ^ found);
          ]
    | [] -> Error [ parse_error (Token.make_eof_token ()) "Expect expression" ]

  let parse tokens :
      (Statement.stmt list * Resolution.resolution_info, Error.t list) result =
    let _ = Resolution.init () in
    scopes := Stack.create () ;
    Stack.push { variables = Hashtbl.create 32; level = 0 } !scopes ;
    program tokens >>= fun stmts -> Ok (stmts, Resolution.get_resolution_info ())
end
