type stmt =
  | EXPR of Expression.expr
  | PRNT of Expression.expr
  | VAR_DEF of Lexer.Token.token * Expression.expr option
  | BLOCK of stmt list
  | IF of Expression.expr * stmt * stmt option
  | WHILE of Expression.expr * stmt
  | FOR of stmt * Expression.expr option * Expression.expr option * stmt
  | FUN_DEF of Lexer.Token.token * Lexer.Token.token list * stmt list
  | CLASS_DEC of Lexer.Token.token * stmt list
  | RETURN of Expression.expr
