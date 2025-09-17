type t =
  | LITERAL of Lexer.Token.literal_type
  | UNARY of Lexer.Token.t * t
  | BINARY of t * Lexer.Token.t * t
  | LOGICAL of t * Lexer.Token.t * t
  | GROUPING of t
  | VARIABLE of Lexer.Token.t
  | ASSIGN of Lexer.Token.t * t
  | CALL of t * t list
  | GET of t * Lexer.Token.t
  | SET of t * Lexer.Token.t * t
  | THIS of Lexer.Token.t
  | SUPER of Lexer.Token.t * Lexer.Token.t

let rec hd_token expr =
  match expr with
  | UNARY (op, _) -> op
  | BINARY (l, _, _) -> hd_token l
  | LOGICAL (l, _, _) -> hd_token l
  | GROUPING e -> hd_token e
  | VARIABLE name -> name
  | ASSIGN (name, _) -> name
  | CALL (callee, _) -> hd_token callee
  | GET (object_expr, _) -> hd_token object_expr
  | SET (object_expr, _, _) -> hd_token object_expr
  | THIS keyword -> keyword
  | SUPER (keyword, _) -> keyword
  | LITERAL _ -> Lexer.Token.make_eof ()

let rec to_string ?(indent = 0) expr =
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
      let sub_expr_str = to_string ~indent:(indent + 1) sub_expr in
      fmt "%sUnary: %s\n%s" indent_str op.lexeme sub_expr_str
  | BINARY (l_expr, op, r_expr) ->
      let left_str = to_string ~indent:(indent + 1) l_expr in
      let right_str = to_string ~indent:(indent + 1) r_expr in
      fmt "%sBinary: %s\n%s%s" indent_str op.lexeme left_str right_str
  | LOGICAL (l_expr, op, r_expr) ->
      let left_str = to_string ~indent:(indent + 1) l_expr in
      let right_str = to_string ~indent:(indent + 1) r_expr in
      fmt "%sLogical: %s\n%s%s" indent_str op.lexeme left_str right_str
  | GROUPING sub_expr ->
      let sub_expr_str = to_string ~indent:(indent + 1) sub_expr in
      fmt "%sGrouping:\n%s" indent_str sub_expr_str
  | VARIABLE name -> fmt "%sVariable: %s\n" indent_str name.lexeme
  | ASSIGN (name, value) ->
      let value_str = to_string ~indent:(indent + 1) value in
      fmt "%sAssign: %s\n%s" indent_str name.lexeme value_str
  | CALL (callee, args) ->
      let callee_str = to_string ~indent:(indent + 1) callee in
      let args_str =
        args |> List.map (to_string ~indent:(indent + 2)) |> String.concat ""
      in
      fmt "%sCall:\n%s  Callee:\n  %s%s  Arguments:\n%s" indent_str indent_str
        callee_str indent_str args_str
  | GET (object_expr, name) ->
      let object_str = to_string ~indent:(indent + 1) object_expr in
      fmt "%sGet: %s\n%s" indent_str name.lexeme object_str
  | SET (object_expr, name, value_expr) ->
      let object_str = to_string ~indent:(indent + 1) object_expr in
      let value_str = to_string ~indent:(indent + 1) value_expr in
      fmt "%sSet: %s\n%s%s" indent_str name.lexeme object_str value_str
  | THIS keyword -> fmt "%sThis: %s\n" indent_str keyword.lexeme
  | SUPER (keyword, method_name) ->
      fmt "%sSuper: %s.%s\n" indent_str keyword.lexeme method_name.lexeme
