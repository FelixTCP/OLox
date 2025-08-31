
module Token = struct
  type token_type = 
    (* Single-character tokens *)
    | LEFT_PAR | RIGHT_PAR | LEFT_BRA | RIGHT_BRA
    | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
    (* One or two character tokens *)
    | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL 
    | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL
    (* Literals *)
    | IDENTIFIER | STRING | NUMBER
    (* Keywords *)
    | TRUE | FALSE | NIL | IF | ELSE | WHILE | FOR 
    | CLASS | FUN | VAR | RETURN | SUPER | THIS  
    | AND | OR 
    | PRINT
    (* UTIL *)
    | SKIP | EOF

  type literal_type =
    | L_STRING of string
    | L_NUM of float
    | L_BOOL of bool
    | L_NIL

  type token = {
    ttype: token_type; 
    lexeme: string; 
    literal: literal_type;
    line: int
  } 

  let ttype_to_string = function 
    | LEFT_PAR -> "LEFT_PAR"
    | RIGHT_PAR -> "RIGHT_PAR"
    | LEFT_BRA -> "LEFT_BRA"
    | RIGHT_BRA-> "RIGHT_BRA"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | MINUS -> "MINUS"
    | PLUS -> "PLUS"
    | SEMICOLON -> "SEMICOLON"
    | SLASH -> "SLASH"
    | STAR-> "STAR"
    | BANG -> "BANG"
    | BANG_EQUAL -> "BANG_EQUAL"
    | EQUAL -> "EQUAL"
    | EQUAL_EQUAL -> "EQUAL_EQUAL"
    | GREATER -> "GREATER"
    | GREATER_EQUAL -> "GREATER_EQUAL"
    | LESS -> "LESS"
    | LESS_EQUAL-> "LESS_EQUAL"
    | IDENTIFIER -> "IDENTIFIER"
    | STRING -> "STRING"
    | NUMBER-> "NUMBER"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NIL -> "NIL"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | FOR -> "FOR"
    | CLASS -> "CLASS"
    | FUN -> "FUN"
    | VAR -> "VAR"
    | RETURN -> "RETURN"
    | SUPER -> "SUPER"
    | THIS  -> "THIS"
    | AND -> "AND"
    | OR -> "OR"
    | PRINT-> "PRINT"
    | EOF -> "EOF"
    | SKIP -> "SKIP" 

  let literal_to_string = function 
    | L_BOOL b -> string_of_bool b
    | L_STRING s -> "\"" ^ s ^ "\""
    | L_NUM f -> string_of_float f
    | L_NIL -> "NIL"

  let to_string token = 
    ttype_to_string token.ttype ^ 
    " " ^ 
    token.lexeme ^ 
    " " ^ literal_to_string token.literal 
end 

module Lexer = struct
  open Token
  open Error

  (* Helper to create a token *)
  let add_token t lexeme line idx : token * int * int =
    ({ ttype = t; lexeme; literal = L_NIL; line }, line, idx)

  let next_char_is source idx expected =
    let idx' = idx + 1 in
    idx' < String.length source && source.[idx'] = expected


  let read_until source idx pred =
    let rec aux i =
      if i >= String.length source then
        i
      else if pred source.[i] then
        i
      else
        aux (i + 1)
    in
    aux (idx + 1) 

  let comment_end_pos source idx = 
    read_until source idx (fun c -> c = '\n')

  let read_string source line idx: (string * int * int, Error.t) result =
    let end_pos = read_until source idx (fun c -> c = '"') in
    if end_pos >= String.length source then
      Error (ScanError(line, "Unterminated string"))
    else
      let lexeme = String.sub source (idx + 1) (end_pos - idx - 1) in 
      let line' = if (String.contains lexeme '\n') then line + 1 else line in
      Ok (lexeme, line', (end_pos + 1))

  let read_number source line idx: (string * int, Error.t) result = 
    let rec aux source idx dot = 
      if idx >= String.length source then
        (idx-1)
      else
        let c = source.[idx] in
        match c with 
        | ('0'..'9') -> aux source (idx+1) dot
        | '.' -> 
          if dot then 
            idx-1
          else
            aux source (idx+1) true
        | _ -> idx-1
    in
    let end_pos = aux source (idx + 1) false in
    let lexeme = String.sub source idx (end_pos - idx + 1) in
    let literal = Float.of_string_opt lexeme in
    match literal with 
    | Some _f -> Ok(lexeme, (end_pos + 1))
    | None -> Error(ScanError(line, "Unexpected error tokenizing number"))

  let is_alpha_num = function 
    | ('a'..'z') -> true
    | ('A'..'Z') -> true
    | ('0'..'9') -> true
    | '_' -> true
    | _ -> false

  let map_identifier_to_token = function
  | "and" -> AND
  | "class" -> CLASS
  | "else" -> ELSE
  | "false" -> FALSE
  | "for" -> FOR
  | "fun" -> FUN
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "print" -> PRINT
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "var" -> VAR
  | "while" -> WHILE
  | _ -> IDENTIFIER

  let read_identifier source line idx = 
    let end_pos = read_until source idx (fun c -> Bool.not(is_alpha_num c)) in
    let lexeme = String.sub source idx (end_pos - idx) in 
    let id_type = map_identifier_to_token lexeme in
    add_token id_type lexeme line end_pos

  let scan_token source line idx : (token * int * int, Error.t) result =
    if idx >= String.length source then
      Error(ScanError(line, "Unexpected end of input"))
    else
      let c = source.[idx] in
      match c with
      | '(' -> Ok(add_token LEFT_PAR "(" line (idx + 1))
      | ')' -> Ok(add_token RIGHT_PAR ")" line (idx + 1))
      | '{' -> Ok(add_token LEFT_BRA "{" line (idx + 1))
      | '}' -> Ok(add_token RIGHT_BRA "}" line (idx + 1))
      | ',' -> Ok(add_token COMMA "," line (idx + 1))
      | '.' -> Ok(add_token DOT "." line (idx + 1))
      | ';' -> Ok(add_token SEMICOLON ";" line (idx + 1))
      | '-' -> Ok(add_token MINUS "-" line (idx + 1))
      | '+' -> Ok(add_token PLUS "+" line (idx + 1))
      | '*' -> Ok(add_token STAR "*" line (idx + 1))
      | '/' -> 
          if next_char_is source idx '/' then
            Ok(add_token SKIP "" (line+1) (comment_end_pos source idx))
          else
            Ok(add_token SLASH "/" line (idx + 1))
      | '!' -> 
          if next_char_is source idx '=' then
            Ok (add_token BANG_EQUAL "!=" line (idx + 2))
          else
            Ok (add_token BANG "!" line (idx + 1))
      | '=' -> 
          if next_char_is source idx '=' then
            Ok (add_token EQUAL_EQUAL "==" line (idx + 2))
          else
            Ok (add_token EQUAL "=" line (idx + 1))
      | '<' -> 
          if next_char_is source idx '=' then
            Ok (add_token LESS_EQUAL "<=" line (idx + 2))
          else
            Ok (add_token LESS "<" line (idx + 1))
      | '>' -> 
          if next_char_is source idx '=' then
            Ok (add_token GREATER_EQUAL ">=" line (idx + 2))
          else
            Ok (add_token GREATER_EQUAL ">" line (idx + 1))
      | '"' -> 
          (match read_string source line idx with
           | Ok (lexeme, line', pos) -> Ok (add_token STRING lexeme line' pos)
           | Error e -> Error e)
      | ('0'..'9') -> 
          (match read_number source line idx with
           | Ok (lexeme, pos) -> Ok (add_token NUMBER lexeme line pos)
           | Error e -> Error e)
      | c when is_alpha_num c -> Ok(read_identifier source line idx)
      | '\n' -> Ok(add_token SKIP "" (line+1) (idx + 1)) 
      | ' ' -> Ok(add_token SKIP " " line (idx + 1))
      | _ -> Error(ScanError(line, Printf.sprintf "Unexpected character '%c'" c))

  let tokenize source : (token list) * (Error.t list) =
    let rec aux line idx tokens errors =
      if idx >= String.length source then
        (List.rev tokens, List.rev errors)
      else
        match scan_token source line idx with
        | Ok (token, line', idx') ->
            aux line' idx' (token :: tokens) errors
        | Error e ->
            aux line (idx + 1) tokens (e :: errors)
    in
    let tokens, errors = aux 1 0 [] [] in
    (List.filter (fun t -> t.ttype <> SKIP) tokens, errors)
end
