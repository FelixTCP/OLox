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

  let print ast =
    let rec aux indent = function
      | LEAF -> ()
      | NODE (expr, lc, rc) ->
          let indent_str = String.make (indent * 2) ' ' in
          ( match expr with
          | Literal lit -> (
              Printf.printf "%sLiteral: " indent_str ;
              match lit with
              | L_BOOL b -> Printf.printf "Bool(%b)\n" b
              | L_STRING s -> Printf.printf "String(\"%s\")\n" s
              | L_NUM f -> Printf.printf "Num(%f)\n" f
              | L_NIL -> Printf.printf "Nil\n"
            )
          | Unary (op, expr) ->
              Printf.printf "%sUnary: %s\n" indent_str op.lexeme ;
              aux (indent + 1) (NODE (expr, LEAF, LEAF))
          | Binary (l_expr, op, r_expr) ->
              Printf.printf "%sBinary: %s\n" indent_str op.lexeme ;
              aux (indent + 1) (NODE (l_expr, LEAF, LEAF)) ;
              aux (indent + 1) (NODE (r_expr, LEAF, LEAF))
          | Grouping expr ->
              Printf.printf "%sGrouping:\n" indent_str ;
              aux (indent + 1) (NODE (expr, LEAF, LEAF))
          ) ;
          aux (indent + 1) lc ;
          aux (indent + 1) rc
    in
    aux 0 ast

  let get_demo () =
    let expr1 =
      Unary
        ( { ttype = MINUS; lexeme = "-"; literal = L_NIL; line = 1 },
          Literal (L_NUM 123.0)
        )
    in
    let expr2 = Grouping (Literal (L_NUM 45.67)) in
    let root_expr =
      Binary
        (expr1, { ttype = STAR; lexeme = "*"; literal = L_NIL; line = 1 }, expr2)
    in
    NODE (root_expr, LEAF, LEAF)
end

module Parser = struct
  open Lexer
  open Error
  open Expression
  open AST

  let parse_error (token : Token.token) msg = Error.ParseError (token.line, msg)

  let expect_token (token_type : Token.token_type) (tokens : Token.token list)
      msg =
    match tokens with
    | t :: rest when t.ttype = token_type -> Ok (t, rest)
    | t :: _ -> Error (parse_error t msg)
    | [] -> Error (parse_error (Lexer.make_token EOF "" (-1)) msg)

  let rec expression tokens : expr * Token.token list = equality tokens

  and equality (tokens : Token.token list) : expr * Token.token list =
    let left, rest = comparison tokens in
    let rec loop (l : expr) (tkns : Token.token list) : expr * Token.token list
        =
      match (tkns : Token.token list) with
      | ({ ttype = BANG_EQUAL | EQUAL_EQUAL; _ } as t) :: rest' ->
          let right, rest'' = comparison rest' in
          loop (Binary (l, t, right)) rest''
      | _ -> l, tkns
    in
    loop left rest

  and comparison (tokens : Token.token list) : expr * Token.token list =
    let left, rest = term tokens in
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = GREATER | GREATER_EQUAL | LESS | LESS_EQUAL; _ } as t)
        :: rest' ->
          let right, rest'' = term rest' in
          loop (Binary (l, t, right)) rest''
      | _ -> l, tkns
    in
    loop left rest

  and term (tokens : Token.token list) : expr * Token.token list =
    let left, rest = factor tokens in
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = MINUS | PLUS; _ } as t) :: rest' ->
          let right, rest'' = factor rest' in
          loop (Binary (l, t, right)) rest''
      | _ -> l, tkns
    in
    loop left rest

  and factor (tokens : Token.token list) : expr * Token.token list =
    let left, rest = unary tokens in
    let rec loop l tkns =
      match (tkns : Token.token list) with
      | ({ ttype = STAR | SLASH; _ } as t) :: rest' ->
          let right, rest'' = unary rest' in
          loop (Binary (l, t, right)) rest''
      | _ -> l, tkns
    in
    loop left rest

  and unary (tokens : Token.token list) : expr * Token.token list =
    match tokens with
    | ({ ttype = BANG | MINUS; _ } as t) :: rest ->
        let expr, rest' = unary rest in
        Unary (t, expr), rest'
    | _ -> primary tokens

  and primary (tokens : Token.token list) : expr * Token.token list =
    match tokens with
    | { ttype = NIL; _ } :: rest -> Literal L_NIL, rest
    | ({ ttype = STRING; _ } as t) :: rest -> Literal (L_STRING t.lexeme), rest
    | ({ ttype = NUMBER; _ } as t) :: rest ->
        Literal (L_NUM (Float.of_string t.lexeme)), rest
    | ({ ttype = TRUE | FALSE; _ } as t) :: rest ->
        let lit = if t.ttype = TRUE then true else false in
        Literal (L_BOOL lit), rest
    | { ttype = LEFT_PAR; _ } :: rest -> (
        let expr, rest' = expression rest in
        match rest' with
        | { ttype = RIGHT_PAR; _ } :: rest'' -> Grouping expr, rest''
        | _ -> failwith "syntax error: exprected ')' after expression"
      )
    | _ -> failwith "syntax error: wrong token"

  (* let parse (tokens : Token.token list) : AST.ast = expression tokens *)
end
