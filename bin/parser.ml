module Expression = struct
  type expr =
    | Literal of Lexer.Token.literal_type
    | Unary of Lexer.Token.token * expr
    | Binary of expr * Lexer.Token.token * expr
    | Grouping of expr
end

module AST = struct
  open Expression

  type ast = NODE of expr * ast * ast | LEAF

  let to_string ast : (string, Error.t list) result =
    let rec aux indent = function
      | LEAF -> ""
      | NODE (expr, lc, rc) ->
          let indent_str = String.make (indent * 2) ' ' in
          let expr_str =
            match expr with
            | Literal lit -> (
                match lit with
                | L_BOOL b ->
                    Printf.sprintf "%sLiteral: Bool(%b)\n" indent_str b
                | L_STRING s ->
                    Printf.sprintf "%sLiteral: String(\"%s\")\n" indent_str s
                | L_NUM f -> Printf.sprintf "%sLiteral: Num(%f)\n" indent_str f
                | L_NIL -> Printf.sprintf "%sLiteral: Nil\n" indent_str
              )
            | Unary (op, expr) ->
                let sub_expr = aux (indent + 1) (NODE (expr, LEAF, LEAF)) in
                Printf.sprintf "%sUnary: %s\n%s" indent_str op.lexeme sub_expr
            | Binary (l_expr, op, r_expr) ->
                let left_str = aux (indent + 1) (NODE (l_expr, LEAF, LEAF)) in
                let right_str = aux (indent + 1) (NODE (r_expr, LEAF, LEAF)) in
                Printf.sprintf "%sBinary: %s\n%s%s" indent_str op.lexeme
                  left_str right_str
            | Grouping expr ->
                let sub_expr = aux (indent + 1) (NODE (expr, LEAF, LEAF)) in
                Printf.sprintf "%sGrouping:\n%s" indent_str sub_expr
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

  (* Helper functions for error handling *)
  let make_eof_token () = Lexer.make_token Token.EOF "" (-1)
  let parse_error (token : Token.token) msg = Error.ParseError (token.line, msg)

  (* Result combinators to reduce boilerplate *)
  let ( >>= ) result f =
    match result with
    | Ok x -> f x
    | Error e -> Error e

  let ( >>| ) result f =
    match result with
    | Ok x -> Ok (f x)
    | Error e -> Error e

  let bind_errors result1 result2_fn =
    match result1, result2_fn () with
    | Ok x, Ok y -> Ok (x, y)
    | Error e1, Error e2 -> Error (e1 @ e2)
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e

  (* Helper to expect a specific token type *)
  let expect_token token_type tokens msg =
    match (tokens : Token.token list) with
    | t :: rest when t.ttype = token_type -> Ok (t, rest)
    | t :: _ -> Error [ parse_error t msg ]
    | [] -> Error [ parse_error (make_eof_token ()) msg ]

  (* Main parsing functions with error handling *)
  let rec expression tokens : (expr * Token.token list, Error.t list) result =
    equality tokens

  and equality (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    comparison tokens >>= fun (left, rest) ->
    let rec loop l tkns : (expr * Token.token list, Error.t list) result =
      match (tkns : Token.token list) with
      | ({ ttype = BANG_EQUAL | EQUAL_EQUAL; _ } as t) :: rest' ->
          comparison rest' >>= fun (right, rest'') ->
          loop (Binary (l, t, right)) rest''
      | _ -> Ok (l, tkns)
    in
    loop left rest

  and comparison (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    term tokens >>= fun (left, rest) ->
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = GREATER | GREATER_EQUAL | LESS | LESS_EQUAL; _ } as t)
        :: rest' ->
          term rest' >>= fun (right, rest'') ->
          loop (Binary (l, t, right)) rest''
      | _ -> Ok (l, tkns)
    in
    loop left rest

  and term (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    factor tokens >>= fun (left, rest) ->
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = MINUS | PLUS; _ } as t) :: rest' ->
          factor rest' >>= fun (right, rest'') ->
          loop (Binary (l, t, right)) rest''
      | _ -> Ok (l, tkns)
    in
    loop left rest

  and factor (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    unary tokens >>= fun (left, rest) ->
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = STAR | SLASH; _ } as t) :: rest' ->
          unary rest' >>= fun (right, rest'') ->
          loop (Binary (l, t, right)) rest''
      | _ -> Ok (l, tkns)
    in
    loop left rest

  and unary (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    match tokens with
    | ({ ttype = BANG | MINUS; _ } as t) :: rest ->
        unary rest >>= fun (expr, rest') -> Ok (Unary (t, expr), rest')
    | _ -> primary tokens

  and primary (tokens : Token.token list) :
      (expr * Token.token list, Error.t list) result =
    match tokens with
    | { ttype = NIL; _ } :: rest -> Ok (Literal L_NIL, rest)
    | ({ ttype = STRING; _ } as t) :: rest ->
        Ok (Literal (L_STRING t.lexeme), rest)
    | ({ ttype = NUMBER; _ } as t) :: rest -> (
        try
          let num = Float.of_string t.lexeme in
          Ok (Literal (L_NUM num), rest)
        with Failure _ ->
          Error [ parse_error t ("Invalid number format: " ^ t.lexeme) ]
      )
    | ({ ttype = TRUE | FALSE; _ } as t) :: rest ->
        let lit = if t.ttype = TRUE then true else false in
        Ok (Literal (L_BOOL lit), rest)
    | { ttype = LEFT_PAR; _ } :: rest ->
        expression rest >>= fun (expr, rest') ->
        expect_token Token.RIGHT_PAR rest' "Expect ')' after expression"
        >>= fun (_, rest'') -> Ok (Grouping expr, rest'')
    | t :: _ -> Error [ parse_error t "Expect expression" ]
    | [] -> Error [ parse_error (make_eof_token ()) "Expect expression" ]

  let parse (tokens : Token.token list) : (expr, Error.t list) result =
    match expression tokens with
    | Ok (expr, [ { ttype = EOF; _ } ]) -> Ok expr
    | Ok (_, remaining) ->
        let remaining_token = List.hd remaining in
        Error
          [ parse_error remaining_token "Unexpected token after expression" ]
    | Error errors -> Error errors
end
