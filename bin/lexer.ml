
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
    (* EOF *)
    | EOF

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

  let make_token ttype lexeme ?(literal=L_NIL) line = 
    { ttype; lexeme; literal; line }

  let next_char_is source idx expected =
    let idx' = idx + 1 in
    idx' < String.length source && source.[idx'] = expected

  let read_until source idx pred =
    let rec aux i =
      if i >= String.length source then i
      else if pred source.[i] then i
      else aux (i + 1)
    in aux (idx + 1) 

  let comment_end_pos source idx = 
    read_until source idx (fun c -> c = '\n')

  let scan_string source line idx: (token * int * int, Error.t) result =
    let rec find_end current_idx current_line =
      if current_idx >= String.length source then
        Error (ScanError(current_line, "Unterminated string"))
      else match source.[current_idx] with
      | '"' -> Ok (current_idx, current_line)
      | '\n' -> find_end (current_idx + 1) (current_line + 1)
      | _ -> find_end (current_idx + 1) current_line
    in match find_end (idx + 1) line with
    | Error e -> Error e
    | Ok (end_pos, final_line) ->
        let content = String.sub source (idx + 1) (end_pos - idx - 1) in
        let lexeme = "\"" ^ content ^ "\"" in
        let token = make_token STRING lexeme ~literal:(L_STRING content) final_line in 
        Ok (token, final_line, end_pos + 1)

  let scan_number source line idx: (token * int * int, Error.t) result = 
    let rec aux idx dot = 
      if idx >= String.length source then (idx-1)
      else let c = source.[idx] in
        match c with 
        | ('0'..'9') -> aux (idx+1) dot
        | '.' -> 
          if dot then idx-1
          else aux (idx+1) true
        | _ -> idx-1
    in
    let end_pos = aux (idx + 1) false in
    let lexeme = String.sub source idx (end_pos - idx + 1) in
    let literal = Float.of_string_opt lexeme in
    match literal with 
    | None -> Error(ScanError(line, "Unexpected error tokenizing number"))
    | Some f -> 
      let token = make_token NUMBER lexeme ~literal:(L_NUM f) line in
      Ok(token, line, end_pos + 1)


  let is_digit = function 
    | ('0'..'9') -> true
    | _ -> false

  let is_alpha = function 
    | ('a'..'z') -> true
    | ('A'..'Z') -> true
    | '_' -> true
    | _ -> false

  let is_alpha_num = function 
    | c when is_alpha c -> true
    | c when is_digit c -> true
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

  let scan_identifier source line idx = 
    let end_pos = read_until source idx (fun c -> Bool.not(is_alpha_num c)) in
    let lexeme = String.sub source idx (end_pos - idx) in 
    let id_type = map_identifier_to_token lexeme in
    let literal = match id_type with
      | TRUE -> L_BOOL true
      | FALSE -> L_BOOL false
      | _ -> L_NIL
    in
    let token = make_token id_type lexeme ~literal:(literal) line in 
    token, line, end_pos

  let rec skip_whitespace_and_comments source line idx: int* int = 
    if idx >= String.length source then (line, idx) 
    else match source.[idx] with
    | ' ' |'\r' | '\t' -> skip_whitespace_and_comments source line (idx+1)
    | '\n' -> skip_whitespace_and_comments source (line+1) (idx+1)
    | '/' when next_char_is source idx '/' ->
        let end_pos = comment_end_pos source idx in
        skip_whitespace_and_comments source (line+1) end_pos
    | _ -> (line, idx)

  let scan_token source line idx: (token * int * int, Error.t) result =
    let single ttype lexeme = Ok (make_token ttype lexeme line, line, idx + 1) in
    let double ttype lexeme = Ok (make_token ttype lexeme line, line, idx + 2) in
    
    let c = source.[idx] in
    match c with
    | '(' -> single LEFT_PAR "(" 
    | ')' -> single RIGHT_PAR ")"
    | '{' -> single LEFT_BRA "{" 
    | '}' -> single RIGHT_BRA "}" 
    | ',' -> single COMMA "," 
    | '.' -> single DOT "." 
    | ';' -> single SEMICOLON ";" 
    | '-' -> single MINUS "-" 
    | '+' -> single PLUS "+" 
    | '*' -> single STAR "*" 
    | '/' -> single SLASH "/" 
    | '!' -> 
      if next_char_is source idx '=' 
      then double BANG_EQUAL "!=" 
      else single BANG "!" 
    | '=' -> 
      if next_char_is source idx '=' 
      then double EQUAL_EQUAL "==" 
      else single EQUAL "=" 
    | '<' -> 
      if next_char_is source idx '=' 
      then double LESS_EQUAL "<=" 
      else single LESS "<" 
    | '>' -> 
      if next_char_is source idx '=' 
      then double GREATER_EQUAL ">=" 
      else single GREATER ">" 
    | '"' -> scan_string source line idx
    | c when is_digit c -> scan_number source line idx
    | c when is_alpha c -> Ok (scan_identifier source line idx)
    | _ -> Error (ScanError (line, Printf.sprintf "Unexpected character '%c'" c))

  let tokenize source : (token list) * (Error.t list) =
    let rec aux line idx tokens errors =
      let (line', idx') = skip_whitespace_and_comments source line idx in 
      if idx' >= String.length source then
        let eof_token = make_token EOF "" line' in
        (List.rev (eof_token::tokens), List.rev errors)
      else match scan_token source line' idx' with
        | Error e -> aux line' (idx' + 1) tokens (e :: errors)
        | Ok (token, n_line, n_idx) ->
            aux n_line n_idx (token :: tokens) errors
    in let tokens, errors = aux 1 0 [] []
    in (tokens, errors)
end
