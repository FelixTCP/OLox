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

(* let rec to_string = function *)
(*   | EXPR expr -> "Expression Statement: " ^ Expression.expr_to_string expr *)
(*   | PRNT expr -> "Print Statement: " ^ Expression.expr_to_string expr *)
(*   | VAR_DEF (id, None) -> "Variable Declaration: " ^ id.lexeme *)
(*   | VAR_DEF (id, Some expr) -> *)
(*       "Variable Initialization: " ^ id.lexeme ^ " = " *)
(*       ^ Expression.expr_to_string expr *)
(*   | BLOCK stmts -> *)
(*       "Block Statement: " ^ (stmts |> List.map to_string |> String.concat "; ") *)
(*   | IF (_, _, _) -> "If Statement" *)
(*   | WHILE (_, _) -> "While Statement" *)
(*   | FOR (_, _, _, body) -> "For Statement" *)
(*   | FUN_DEF (name, params, body) -> *)
(*       let params_str = *)
(*         if params = [] then *)
(*           "No parameters" *)
(*         else *)
(*           params *)
(*           |> List.map (fun (p : Lexer.Token.token) -> p.lexeme) *)
(*           |> String.concat ", " *)
(*       in *)
(*       Printf.sprintf "Function Definition: %s(%s)" name.lexeme params_str *)
(*   | CLASS_DEC (name, methods) -> "Class Declaration: " ^ name.lexeme *)
(*   | RETURN expr -> "Return Statement" *)
