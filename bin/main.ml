open Lexer

let parse tokens = List.iter (fun t -> print_endline (Token.to_string t)) tokens

let parse_if_no_errors = function
  | tkns, errs ->
      (* if List.length errs = 0 then *)
      (*   parse tkns *)
      (* else *)
      (*   List.iter (fun e -> print_endline (Error.to_string e)) errs *)
      parse tkns ;
      List.iter (fun e -> print_endline (Error.to_string e)) errs

let run file =
  let tokens, errors = Lexer.tokenize file in
  parse_if_no_errors (tokens, errors)

let run_file filename =
  let file = In_channel.with_open_bin filename In_channel.input_all in
  run file

let exit_prompt () = print_endline "Thanks for running olox!"

let rec run_prompt () =
  print_endline "(Lox REPL) > " ;
  try
    match input_line stdin with
    | "exit" -> exit_prompt ()
    | "q" -> exit_prompt ()
    | line ->
        run line ;
        run_prompt ()
  with End_of_file -> exit_prompt ()

let print_ast () =
  let ast = Parser.AST.get_demo () in
  Parser.AST.print ast ;
  print_endline ""

let main () =
  let () = print_ast () in

  let amt = Array.length Sys.argv in
  if amt > 2 then
    print_endline "Usage: olox [filename]"
  else if amt = 2 then
    run_file Sys.argv.(1)
  else
    run_prompt ()
;;

main ()
