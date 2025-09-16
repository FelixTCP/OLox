open Lexer

(* Parser Helper functions *)
let err (token : Token.t) msg = Error [ Error.ParseError (token.line, msg) ]

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let expect_token token_type tokens msg =
  match (tokens : Token.t list) with
  | t :: rest when t.ttype = token_type -> Ok (t, rest)
  | t :: _ -> err t msg
  | [] -> err (Token.make_eof ()) msg

let expect_statement tokens =
  expect_token SEMICOLON tokens
    (Printf.sprintf "Expected ';' after `%s` to end the statement"
       (List.hd tokens).lexeme
    )

let parse_binary_left_assoc_expression operand_parser operator_types
    ?(logical = false) tokens : (Expression.t * Token.t list, Error.t list) result =
  operand_parser tokens >>= fun (left, rest) ->
  let rec loop l tkns : (Expression.t * Token.t list, Error.t list) result =
    match (tkns : Token.t list) with
    | ({ ttype; _ } as t) :: rest' when List.mem ttype operator_types ->
        operand_parser rest' >>= fun (right, rest'') ->
        if logical then
          loop (Expression.LOGICAL (l, t, right)) rest''
        else
          loop (BINARY (l, t, right)) rest''
    | _ -> Ok (l, tkns)
  in
  loop left rest

(* Main parsing functions *)
let rec program (tokens : Token.t list) : (Statement.t list, Error.t list) result =
  let rec parse_stmt acc (tkns : Token.t list) =
    if tkns = [] then
      err (Token.make_eof ()) "Unexpected end of input"
    else if (List.hd tkns).ttype = EOF then
      Ok (List.rev acc)
    else
      declaration tkns >>= fun (stmt, rest) -> parse_stmt (stmt :: acc) rest
  in
  parse_stmt [] tokens

and declaration = function
  | { ttype = VAR; _ } :: rest -> var_declaration rest
  | { ttype = FUN; _ } :: rest -> fun_declaration rest
  | { ttype = CLASS; _ } :: rest -> class_declaration rest
  | _ as tokens -> statement tokens

and var_declaration = function
  | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = EQUAL; _ } :: rest ->
      expression rest >>= fun (expr, rest') ->
      expect_statement rest' >>= fun (_, rest'') ->
      Ok (Statement.VAR_DEF (id, Some expr), rest'')
  | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = SEMICOLON; _ } :: rest ->
      Ok (VAR_DEF (id, None), rest)
  | { ttype = IDENTIFIER; _ } :: t :: _rest ->
      err t ("Expected '=' in variable declaration but found " ^ t.lexeme)
  | t :: _ ->
      err t ("Expected identifier in variable declaration but found " ^ t.lexeme)
  | [] ->
      err (Token.make_eof ())
        "Expected identifier in variable declaration but found nothing"

and class_declaration (tokens : Token.t list) =
  let rec parse_body acc (tkns : Token.t list) =
    match tkns with
    | { ttype = RIGHT_BRA; _ } :: rest -> Ok (List.rev acc, rest)
    | [] ->
        err (Token.make_eof ())
          "Unterminated class body. Expected '}' before end of input."
    | _ ->
        declaration tkns >>= fun (func_stmt, rest) ->
        parse_body (func_stmt :: acc) rest
  in
  match tokens with
  | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = LEFT_BRA; _ } :: rest ->
      parse_body [] rest >>= fun (body, rest') ->
      Ok (Statement.CLASS_DEC (id, body), rest')
  | { ttype = IDENTIFIER; _ } :: other :: _ ->
      err other
        "Expected '{' to start a class body afer identifier in class declaration"
  | t :: _ -> err t "Expected identifier after 'class' in class declaration"
  | [] ->
      err (Token.make_eof ())
        "Expected identifier after 'class' in class declaration but found EOF"

and fun_declaration (tokens : Token.t list) =
  func tokens >>= fun (function_stmt, rest) -> Ok (function_stmt, rest)

and func = function
  | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = LEFT_PAR; _ } :: rest ->
      parameters rest >>= fun (params, rest') ->
      expect_token LEFT_BRA rest' "Expected '{' after parameter list"
      >>= fun (_, rest'') ->
      block rest'' >>= fun (body, rest''') ->
      Ok (Statement.FUN_DEF (id, params, body), rest''')
  | { ttype = IDENTIFIER; _ } :: other :: _ ->
      err other
        "Expected '(' to start parameter list afer identifier in function \
         declaration"
  | t :: _ -> err t "Expected identifier after 'fun' in function declaration"
  | [] ->
      err (Token.make_eof ())
        "Expected identifier after 'fun' in function declaration but found EOF"

and parameters tokens : (Token.t list * Token.t list, Error.t list) result =
  let rec parse_params acc (tkns : Token.t list) =
    if List.length acc > 255 then
      err (List.hd tkns)
        "Cannot have more than 255 parameters in a function declaration"
    else
      match tkns with
      | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev acc, rest)
      | ({ ttype = IDENTIFIER; _ } as t) :: { ttype = RIGHT_PAR; _ } :: rest ->
          Ok (List.rev (t :: acc), rest)
      | ({ ttype = IDENTIFIER; _ } as t) :: rest ->
          expect_token COMMA rest "Expected ',' between parameters"
          >>= fun (_, rest') -> parse_params (t :: acc) rest'
      | t :: _ ->
          err t
            (Printf.sprintf
               "Expected Unterminated parameter list. Expected ')' but found ` `."
            ^ t.lexeme
            )
      | [] ->
          err (Token.make_eof ())
            "Unterminated parameter list. Expected ')' but found EOF"
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

and print_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
  expression tokens >>= fun (expr, rest) ->
  expect_statement rest >>= fun (_, rest') -> Ok (Statement.PRNT expr, rest')

and expression_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
  expression tokens >>= fun (expr, rest) ->
  expect_statement rest >>= fun (_, rest') -> Ok (Statement.EXPR expr, rest')

and block (tokens : Token.t list) :
    (Statement.t list * Token.t list, Error.t list) result =
  let rec parse_block acc (tkns : Token.t list) =
    match tkns with
    | [] ->
        err (Token.make_eof ())
          "Unterminated block. Expected '}' before end of input."
    | { ttype = RIGHT_BRA; _ } :: rest -> Ok (List.rev acc, rest)
    | _ -> declaration tkns >>= fun (stmt, rest) -> parse_block (stmt :: acc) rest
  in
  parse_block [] tokens

and return_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
  expression tokens >>= fun (expr, rest) ->
  expect_statement rest >>= fun (_, rest') -> Ok (Statement.RETURN expr, rest')

and if_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
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
  | t :: _ -> err t "Expected '(' after if-statement"
  | [] -> err (Token.make_eof ()) "Expected '(' after if-statement but reached EOF"

and while_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
  match tokens with
  | { ttype = LEFT_PAR; _ } :: rest ->
      expression rest >>= fun (condition, rest') ->
      expect_token RIGHT_PAR rest' "Expected ')' after while condition"
      >>= fun (_, rest'') ->
      statement rest'' >>= fun (stmt, rest''') ->
      Ok (Statement.WHILE (condition, stmt), rest''')
  | t :: _ -> err t "Expected '(' after while-statement"
  | [] ->
      err (Token.make_eof ()) "Expected '(' after while-statement but reached EOF"

and for_statement (tokens : Token.t list) :
    (Statement.t * Token.t list, Error.t list) result =
  let parse_statement init cond incr (tokens : Token.t list) =
    expect_token RIGHT_PAR tokens "Expected ')' after for condition"
    >>= fun (_, rest) ->
    statement rest >>= fun (stmt, rest') ->
    Ok (Statement.FOR (init, cond, incr, stmt), rest')
  in
  let parse_increment init cond (tokens : Token.t list) =
    match tokens with
    | { ttype = SEMICOLON; _ } :: rest ->
        let incr = None in
        parse_statement init cond incr rest
    | _ ->
        expression tokens >>= fun (incr_expr, rest) ->
        let incr = Some incr_expr in
        parse_statement init cond incr rest
  in
  let parse_condition init (tokens : Token.t list) =
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
          var_declaration rest' >>= fun (init, rest'') -> parse_condition init rest''
      | _ ->
          expression_statement rest >>= fun (init, rest') ->
          parse_condition init rest'
    )
  | t :: _ -> err t "Expected '(' after for-statement"
  | [] -> err (Token.make_eof ()) "Expected '(' after for-statement but reached EOF"

and arguments tokens : (Expression.t list * Token.t list, Error.t list) result =
  let rec parse_args acc (tkns : Token.t list) =
    match tkns with
    | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev acc, rest)
    | [] ->
        err (Token.make_eof ())
          "Unterminated argument list. Expected ')' before end of input"
    | _ -> (
        expression tkns >>= fun (arg, rest) ->
        match rest with
        | { ttype = RIGHT_PAR; _ } :: rest -> Ok (List.rev (arg :: acc), rest)
        | _ ->
            expect_token Token.COMMA rest "Expected ',' between arguments"
            >>= fun (_, rest') ->
            if List.length acc > 255 then
              err (List.hd tkns)
                "Cannot have more than 255 arguments in a function call"
            else
              parse_args (arg :: acc) rest'
      )
  in
  parse_args [] tokens

and expression tokens : (Expression.t * Token.t list, Error.t list) result =
  assignment tokens

and assignment tokens : (Expression.t * Token.t list, Error.t list) result =
  call tokens >>= fun (expr, rest) ->
  match rest with
  | { ttype = EQUAL; _ } :: rest' -> (
      match expr with
      | GET (obj, name) ->
          assignment rest' >>= fun (value, rest'') ->
          Ok (Expression.SET (obj, name, value), rest'')
      | VARIABLE var_token ->
          assignment rest' >>= fun (value, rest'') ->
          Ok (Expression.ASSIGN (var_token, value), rest'')
      | _ -> err (List.hd rest) "Invalid assignment target"
    )
  | _ -> logic_or tokens

and logic_or tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression logic_and [ OR ] tokens ~logical:true

and logic_and tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression equality [ AND ] tokens ~logical:true

and equality tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression comparison [ BANG_EQUAL; EQUAL_EQUAL ] tokens

and comparison tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression term
    [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ]
    tokens

and term tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression factor [ PLUS; MINUS ] tokens

and factor tokens : (Expression.t * Token.t list, Error.t list) result =
  parse_binary_left_assoc_expression unary [ STAR; SLASH ] tokens

and unary tokens : (Expression.t * Token.t list, Error.t list) result =
  match tokens with
  | ({ ttype = BANG | MINUS; _ } as t) :: rest ->
      unary rest >>= fun (expr, rest') -> Ok (Expression.UNARY (t, expr), rest')
  | _ -> call tokens

and call tokens : (Expression.t * Token.t list, Error.t list) result =
  let rec parse_call (expr : Expression.t) (tokens : Token.t list) =
    match tokens with
    | { ttype = LEFT_PAR; _ } :: rest ->
        arguments rest >>= fun (args, rest') -> parse_call (CALL (expr, args)) rest'
    | { ttype = DOT; _ } :: rest -> (
        match rest with
        | ({ ttype = IDENTIFIER; _ } as id) :: rest' ->
            parse_call (GET (expr, id)) rest'
        | _ ->
            err (List.hd rest) "Expected property name after '.' in property access"
      )
    | _ -> Ok (expr, tokens)
  in
  primary tokens >>= fun (expr, rest) -> parse_call expr rest

and primary tokens : (Expression.t * Token.t list, Error.t list) result =
  match tokens with
  | { ttype = NIL; _ } :: rest -> Ok (LITERAL L_NIL, rest)
  | ({ ttype = STRING; _ } as t) :: rest -> Ok (LITERAL (L_STRING t.lexeme), rest)
  | ({ ttype = NUMBER; _ } as t) :: rest -> (
      try
        let num = Float.of_string t.lexeme in
        Ok (LITERAL (L_NUM num), rest)
      with Failure _ -> err t ("Invalid number format: " ^ t.lexeme)
    )
  | ({ ttype = TRUE | FALSE; _ } as t) :: rest ->
      let lit = if t.ttype = TRUE then true else false in
      Ok (LITERAL (L_BOOL lit), rest)
  | ({ ttype = IDENTIFIER; _ } as t) :: rest -> Ok (VARIABLE t, rest)
  | { ttype = LEFT_PAR; _ } :: rest ->
      expression rest >>= fun (expr, rest') ->
      expect_token Token.RIGHT_PAR rest' "Expect ')' after expression"
      >>= fun (_, rest'') -> Ok (Expression.GROUPING expr, rest'')
  | ({ ttype = THIS; _ } as t) :: rest -> Ok (VARIABLE t, rest)
  | t :: rest ->
      let found =
        (* <= 1 because of EOF token at the end *)
        if List.length rest <= 1 then "none" else (List.hd rest).lexeme
      in
      err t ("Expected expression after " ^ t.lexeme ^ " but found " ^ found)
  | [] -> err (Token.make_eof ()) "Expect expression"

let parse tokens : (Statement.t list * Resolver.resolution, Error.t list) result =
  program tokens >>= fun stmts ->
  Resolver.resolve stmts >>= fun res -> Ok (stmts, res)
