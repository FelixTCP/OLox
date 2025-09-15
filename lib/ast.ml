open Expression

type ast = PROGRAM of Statement.stmt list * Resolver.resolution

let to_string (ast : ast) : (string, Error.t list) result =
  let res_to_string (res : Resolver.resolution) : string =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) res [] in
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
            | GET _ -> "GET"
            | SET _ -> "SET"
            | THIS t -> Printf.sprintf "THIS(%s)" t.lexeme
            )
            depth
        )
        entries
    in
    String.concat "\n" entry_strings
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
             | Statement.EXPR expr -> expr_to_string ~indent expr
             | Statement.VAR_DEF (id, None) ->
                 Printf.sprintf "%sVar Declaration: %s\n" indent_str id.lexeme
             | Statement.VAR_DEF (id, Some expr) ->
                 let expr_str = expr_to_string ~indent:(indent + 1) expr in
                 Printf.sprintf "%sVar Initilazation: %s\n%s" indent_str id.lexeme
                   expr_str
             | Statement.BLOCK stmts ->
                 let block_str = aux (indent + 1) stmts in
                 Printf.sprintf "%sBlock:\n%s" indent_str block_str
             | Statement.IF (cond, then_branch, else_branch) ->
                 let cond_str = expr_to_string ~indent:(indent + 1) cond in
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
                 let cond_str = expr_to_string ~indent:(indent + 1) cond in
                 let body_str = aux (indent + 1) [ body ] in
                 Printf.sprintf "%sWhile Statement:\n%s  Condition: \n  %s%s"
                   indent_str indent_str cond_str body_str
             | Statement.FOR (init, cond, incr, body) ->
                 let init_str = aux (indent + 1) [ init ] in
                 let cond_str =
                   match cond with
                   | None -> "[NO CONDITION]"
                   | Some c -> expr_to_string ~indent:(indent + 1) c
                 in
                 let incr_str =
                   match incr with
                   | None -> "[NO INCREMENT]"
                   | Some i -> expr_to_string ~indent:(indent + 1) i
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
                 let expr_str = expr_to_string ~indent:(indent + 1) expr in
                 Printf.sprintf "%sReturn Statement:\n%s" indent_str expr_str
             | Statement.CLASS_DEC (name, methods) ->
                 (* let superclass_str = *)
                 (*   match superclass with *)
                 (*   | None -> "No superclass" *)
                 (*   | Some s -> s.lexeme *)
                 (* in *)
                 let methods_str = aux (indent + 1) methods in
                 Printf.sprintf "%sClass Declaration: %s\n%s" indent_str name.lexeme
                   (* superclass_str *)
                   methods_str
           in
           acc ^ stmt_str
         )
         ""
  in
  match ast with
  | PROGRAM (p, r) ->
      Ok
        (aux 0 p ^ "\nResolution Info (WARN - currently buggy!):\n" ^ res_to_string r)

let build_ast ast : (ast, Error.t list) result =
  let stmts, res_info = ast in
  Ok (PROGRAM (stmts, res_info))
