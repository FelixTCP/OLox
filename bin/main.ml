open Lexer
open Parser

let print_errors errors = List.iter (fun e -> Error.print_error e) errors
let print_result result = print_endline result

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let run_pipeline result ~(on_success : _ -> unit) ~(on_error : _ -> unit) =
  match result with
  | Ok x -> on_success x
  | Error e -> on_error e

let run file =
  Lexer.tokenize file >>= Parser.parse >>= AST.build_ast >>= AST.to_string
  |> run_pipeline ~on_success:print_result ~on_error:print_errors

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
