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
