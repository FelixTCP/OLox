module Expression = struct
  type expr =
    | LITERAL of Lexer.Token.literal_type
    | UNARY of Lexer.Token.token * expr
    | BINARY of expr * Lexer.Token.token * expr
    | GROUPING of expr
end

module AST = struct
  open Expression

  type ast = NODE of expr * ast * ast | LEAF

  let to_string ast : (string, Error.t list) result =
    let rec aux indent = function
      | LEAF -> ""
      | NODE (expr, lc, rc) ->
          let indent_str = String.make (indent * 2) ' ' in
          let fmt = Printf.sprintf in
          let expr_str =
            match expr with
            | LITERAL lit -> (
                match lit with
                | L_BOOL b -> fmt "%sLiteral: Bool(%b)\n" indent_str b
                | L_STRING s -> fmt "%sLiteral: String(\"%s\")\n" indent_str s
                | L_NUM f -> fmt "%sLiteral: Num(%f)\n" indent_str f
                | L_NIL -> fmt "%sLiteral: Nil\n" indent_str
              )
            | UNARY (op, expr) ->
                let sub_expr = aux (indent + 1) (NODE (expr, LEAF, LEAF)) in
                fmt "%sUnary: %s\n%s" indent_str op.lexeme sub_expr
            | BINARY (l_expr, op, r_expr) ->
                let left_str = aux (indent + 1) (NODE (l_expr, LEAF, LEAF)) in
                let right_str = aux (indent + 1) (NODE (r_expr, LEAF, LEAF)) in
                fmt "%sBinary: %s\n%s%s" indent_str op.lexeme left_str right_str
            | GROUPING expr ->
                let sub_expr = aux (indent + 1) (NODE (expr, LEAF, LEAF)) in
                fmt "%sGrouping:\n%s" indent_str sub_expr
          in
          expr_str ^ aux (indent + 1) lc ^ aux (indent + 1) rc
    in
    Ok (aux 0 ast)

  let build_ast expr : (ast, Error.t list) result =
    let ast = NODE (expr, LEAF, LEAF) in
    Ok ast
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
  let rec expression tokens : (expr * Token.token list, Error.t list) result =
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

  let parse tokens : (expr, Error.t list) result =
    match expression tokens with
    | Ok (expr, [ { ttype = EOF; _ } ]) -> Ok expr
    | Ok (_, remaining) ->
        let remaining_token = List.hd remaining in
        Error
          [
            parse_error remaining_token
              ("Unexpected token after expression: " ^ remaining_token.lexeme);
          ]
    | Error errors -> Error errors
end
