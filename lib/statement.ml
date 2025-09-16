type t =
  | EXPR of Expression.t
  | PRNT of Expression.t
  | VAR_DEF of Lexer.Token.t * Expression.t option
  | BLOCK of t list
  | IF of Expression.t * t * t option
  | WHILE of Expression.t * t
  | FOR of t * Expression.t option * Expression.t option * t
  | FUN_DEF of Lexer.Token.t * Lexer.Token.t list * t list
  | CLASS_DEC of Lexer.Token.t * t list
  | RETURN of Expression.t

let rec hd_token stmt =
  match stmt with
  | EXPR expr -> Expression.hd_token expr
  | PRNT expr -> Expression.hd_token expr
  | VAR_DEF (name, _) -> name
  | BLOCK (stmt :: _) -> hd_token stmt
  | BLOCK [] -> Lexer.Token.make_eof ()
  | IF (_, then_branch, _) -> hd_token then_branch
  | WHILE (_, body) -> hd_token body
  | FOR (init, _, _, _) -> hd_token init
  | FUN_DEF (name, _, _) -> name
  | CLASS_DEC (name, _) -> name
  | RETURN expr -> Expression.hd_token expr
