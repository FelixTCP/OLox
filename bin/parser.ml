module Expression = struct
  type expr =
    | LITERAL of Lexer.Token.literal_type
    | UNARY of Lexer.Token.token * expr
    | BINARY of expr * Lexer.Token.token * expr
    | GROUPING of expr
    | VARIABLE of Lexer.Token.token
end

module Statement = struct
  type stmt =
    | EXPR of Expression.expr
    | PRNT of Expression.expr
    | VAR_DEF of Lexer.Token.token * Expression.expr option
end

module AST = struct
  open Expression

  type ast = PROGRAM of Statement.stmt list

  let to_string (ast : ast) : (string, Error.t list) result =
    let rec expr_to_string indent expr =
      let fmt = Printf.sprintf in
      let indent_str = String.make (indent * 2) ' ' in
      match expr with
      | LITERAL lit -> (
          match lit with
          | Lexer.Token.L_BOOL b -> fmt "%sLiteral: Bool(%b)\n" indent_str b
          | Lexer.Token.L_STRING s -> fmt "%sLiteral: String(\"%s\")\n" indent_str s
          | Lexer.Token.L_NUM f -> fmt "%sLiteral: Num(%f)\n" indent_str f
          | Lexer.Token.L_NIL -> fmt "%sLiteral: Nil\n" indent_str
        )
      | UNARY (op, sub_expr) ->
          let sub_expr_str = expr_to_string (indent + 1) sub_expr in
          fmt "%sUnary: %s\n%s" indent_str op.lexeme sub_expr_str
      | BINARY (l_expr, op, r_expr) ->
          let left_str = expr_to_string (indent + 1) l_expr in
          let right_str = expr_to_string (indent + 1) r_expr in
          fmt "%sBinary: %s\n%s%s" indent_str op.lexeme left_str right_str
      | GROUPING sub_expr ->
          let sub_expr_str = expr_to_string (indent + 1) sub_expr in
          fmt "%sGrouping:\n%s" indent_str sub_expr_str
      | VARIABLE name -> fmt "%sVariable:\n%s" indent_str name.lexeme
    in
    let rec aux indent (ast : Statement.stmt list) =
      ast
      |> List.fold_left
           (fun acc stmt ->
             let indent_str = String.make (indent * 2) ' ' in
             let stmt_str =
               match stmt with
               | Statement.PRNT expr ->
                   let expr_str = aux (indent + 1) [ Statement.EXPR expr ] in
                   Printf.sprintf "%sPrint Statement:\n%s" indent_str expr_str
               | Statement.EXPR expr -> expr_to_string indent expr
               | Statement.VAR_DEF (id, None) ->
                   Printf.sprintf "%sVar Declaration: %s\n" indent_str id.lexeme
               | Statement.VAR_DEF (id, Some expr) ->
                   let expr_str = expr_to_string (indent + 1) expr in
                   Printf.sprintf "%sVar Initilazation: %s\n%s" indent_str id.lexeme
                     expr_str
             in
             acc ^ stmt_str
           )
           ""
    in
    match ast with
    | PROGRAM p -> Ok (aux 0 p)

  let build_ast stmts : (ast, Error.t list) result = Ok (PROGRAM stmts)
end

module Parser = struct
  open Lexer
  open Expression

  (* Helper functions *)
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
    expect_token SEMICOLON tokens "Expected ';' at the end of a statement"

  let parse_binary_left_assoc_expression operand_parser operator_types tokens :
      (expr * Token.token list, Error.t list) result =
    operand_parser tokens >>= fun (left, rest) ->
    let rec loop l tkns : (expr * Token.token list, Error.t list) result =
      match (tkns : Token.token list) with
      | ({ ttype; _ } as t) :: rest' when List.mem ttype operator_types ->
          operand_parser rest' >>= fun (right, rest'') ->
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
    | _ as tokens -> statement tokens

  and var_declaration = function
    | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = EQUAL; _ } :: rest ->
        expression rest >>= fun (expr, rest') ->
        expect_statement rest' >>= fun (_, rest'') ->
        Ok (Statement.VAR_DEF (id, Some expr), rest'')
    | ({ ttype = IDENTIFIER; _ } as id) :: { ttype = SEMICOLON; _ } :: rest ->
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

  and statement = function
    | { ttype = PRINT; _ } :: rest -> print_statement rest
    | _ as tokens -> expression_statement tokens

  and print_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    expression tokens >>= fun (expr, rest) ->
    expect_statement rest >>= fun (_, rest') -> Ok (Statement.PRNT expr, rest')

  and expression_statement (tokens : Token.token list) :
      (Statement.stmt * Token.token list, Error.t list) result =
    expression tokens >>= fun (expr, rest) ->
    expect_statement rest >>= fun (_, rest') -> Ok (Statement.EXPR expr, rest')

  and expression tokens : (expr * Token.token list, Error.t list) result =
    equality tokens

  (* TODO: introduce abstraction to parse a binary left assoc expression *)
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
    | _ -> primary tokens

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
        let lit = if t.ttype = TRUE then true else false in
        Ok (LITERAL (L_BOOL lit), rest)
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

  let parse tokens : (Statement.stmt list, Error.t list) result = program tokens
end
