open Lexer
open Parser

let print_errors errors = List.iter (fun e -> Error.print_error e) errors

let run file =
  match Lexer.tokenize file with
  | Error errs -> print_errors errs
  | Ok tokens -> Parser.parse tokens |> AST.print

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

let main () =
  let amt = Array.length Sys.argv in
  if amt > 2 then
    print_endline "Usage: olox [filename]"
  else if amt = 2 then
    run_file Sys.argv.(1)
  else
    run_prompt ()
;;

main ()
