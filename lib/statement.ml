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
