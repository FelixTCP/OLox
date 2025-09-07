open Lexer
open Parser
open Interpreter
open Value
open Options

let print_errors errors = List.iter (fun e -> Error.print_error e) errors
let print_usage () = print_endline "Usage: olox [--debug] [--help] [filename]"

let print_tokens (result : Token.token list) =
  result
  |> List.map (fun t -> Token.to_string t)
  |> List.iter (fun t -> print_endline t)

let print_ast result = result |> print_endline
let print_result result = result |> Value.stringify_result |> print_endline

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let run_pipeline result ~(on_success : _ -> unit) ~(on_error : _ -> unit) =
  match result with
  | Ok x -> on_success x
  | Error e -> on_error e

let run file =
  Lexer.tokenize file >>= Parser.parse >>= AST.build_ast
  >>= Interpreter.interpret_ast
  |> run_pipeline ~on_success:print_result ~on_error:print_errors

let run_debug file =
  let print_debug input printer =
    input |> run_pipeline ~on_success:printer ~on_error:print_errors
  in

  let tokens = Lexer.tokenize file in
  let ast = tokens >>= Parser.parse >>= AST.build_ast >>= AST.to_string in

  print_debug tokens print_tokens ;
  print_debug ast print_ast

let run_file ?(runner = run) filename =
  let file = In_channel.with_open_bin filename In_channel.input_all in
  runner file

let exit_prompt () = print_endline "Thanks for running olox!"

let rec run_prompt ?(runner : string -> unit = run) () =
  print_endline "(Lox REPL) > " ;
  try
    match input_line stdin with
    | "exit" | "q" -> exit_prompt ()
    | line ->
        runner line ;
        run_prompt ~runner ()
  with End_of_file -> exit_prompt ()

let main () =
  let args = Array.to_list Sys.argv |> List.tl in
  Options.parse_flags args ;
  let files = List.filter (fun s -> not (String.starts_with s ~prefix:"--")) args in

  let runner = if Options.is_active "debug" then run_debug else run in

  match files with
  | [] -> run_prompt ~runner ()
  | [ filename ] -> run_file ~runner filename
  | _ -> print_usage ()
;;

main ()
