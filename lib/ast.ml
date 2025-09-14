open Parser
open Expression

type ast = PROGRAM of Statement.stmt list * Resolution.resolution_info

let to_string (ast : ast) : (string, Error.t list) result =
  let res_info_to_string (res_info : Resolution.resolution_info) : string =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) res_info.locals [] in
    let entry_strings =
      List.map
        (fun (expr, depth) ->
          Printf.sprintf "Expr: %s at depth %d"
            ( match expr with
            | LITERAL _ -> "LITERAL"
            | UNARY _ -> "UNARY"
            | BINARY _ -> "BINARY"
            | LOGICAL _ -> "LOGICAL"
            | GROUPING _ -> "GROUPING"
            | VARIABLE t -> Printf.sprintf "VARIABLE(%s)" t.lexeme
            | ASSIGN (t, _) -> Printf.sprintf "ASSIGN(%s)" t.lexeme
            | CALL _ -> "CALL"
            )
            depth
        )
        entries
    in
    String.concat "\n" entry_strings
  in
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
    | LOGICAL (l_expr, op, r_expr) ->
        let left_str = expr_to_string (indent + 1) l_expr in
        let right_str = expr_to_string (indent + 1) r_expr in
        fmt "%sLogical: %s\n%s%s" indent_str op.lexeme left_str right_str
    | GROUPING sub_expr ->
        let sub_expr_str = expr_to_string (indent + 1) sub_expr in
        fmt "%sGrouping:\n%s" indent_str sub_expr_str
    | VARIABLE name -> fmt "%sVariable: %s\n" indent_str name.lexeme
    | ASSIGN (name, value) ->
        let value_str = expr_to_string (indent + 1) value in
        fmt "%sAssign: %s\n%s" indent_str name.lexeme value_str
    | CALL (callee, args) ->
        let callee_str = expr_to_string (indent + 1) callee in
        let args_str =
          args |> List.map (expr_to_string (indent + 2)) |> String.concat ""
        in
        fmt "%sCall:\n%s  Callee:\n  %s%s  Arguments:\n%s" indent_str indent_str
          callee_str indent_str args_str
  in
  let rec aux indent (stmts : Statement.stmt list) =
    stmts
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
             | Statement.BLOCK stmts ->
                 let block_str = aux (indent + 1) stmts in
                 Printf.sprintf "%sBlock:\n%s" indent_str block_str
             | Statement.IF (cond, then_branch, else_branch) ->
                 let cond_str = expr_to_string (indent + 1) cond in
                 let then_str = aux (indent + 1) [ then_branch ] in
                 let else_str =
                   match else_branch with
                   | None -> ""
                   | Some e ->
                       Printf.sprintf "%sElse Statement:\n%s" indent_str
                         (aux (indent + 1) [ e ])
                 in
                 Printf.sprintf "%sIf Statement:\n%s  Condition: \n  %s%s%s"
                   indent_str indent_str cond_str then_str else_str
             | Statement.WHILE (cond, body) ->
                 let cond_str = expr_to_string (indent + 1) cond in
                 let body_str = aux (indent + 1) [ body ] in
                 Printf.sprintf "%sWhile Statement:\n%s  Condition: \n  %s%s"
                   indent_str indent_str cond_str body_str
             | Statement.FOR (init, cond, incr, body) ->
                 let init_str = aux (indent + 1) [ init ] in
                 let cond_str =
                   match cond with
                   | None -> "[NO CONDITION]"
                   | Some c -> expr_to_string (indent + 1) c
                 in
                 let incr_str =
                   match incr with
                   | None -> "[NO INCREMENT]"
                   | Some i -> expr_to_string (indent + 1) i
                 in
                 let body_str = aux (indent + 1) [ body ] in
                 Printf.sprintf
                   "%sFor Statement:\n\
                    %s  Init: \n\
                   \  %s%s  Condition: \n\
                   \  %s%s  Increment: \n\
                   \  %s%s"
                   indent_str indent_str init_str indent_str cond_str indent_str
                   incr_str body_str
             | Statement.FUN_DEF (name, params, body) ->
                 let params_str =
                   if params = [] then
                     "No parameters"
                   else
                     params
                     |> List.map (fun (p : Lexer.Token.token) -> p.lexeme)
                     |> String.concat ", "
                 in
                 let body_str = aux (indent + 1) body in
                 Printf.sprintf
                   "%sFunction Declaration: %s\n\
                    %s  Parameters: (%s)\n\
                    %s  Body:\n\
                   \  %s"
                   indent_str name.lexeme indent_str params_str indent_str body_str
             | Statement.RETURN expr ->
                 let expr_str = expr_to_string (indent + 1) expr in
                 Printf.sprintf "%sReturn Statement:\n%s" indent_str expr_str
           in
           acc ^ stmt_str
         )
         ""
  in
  match ast with
  | PROGRAM (p, r) ->
      Ok
        (aux 0 p ^ "\nResolution Info (WARN - currently buggy!):\n"
       ^ res_info_to_string r
        )

let build_ast ast : (ast, Error.t list) result =
  let stmts, res_info = ast in
  Ok (PROGRAM (stmts, res_info))
