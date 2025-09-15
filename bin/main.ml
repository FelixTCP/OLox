module Lexer = Olox.Lexer.Lexer
module Token = Olox.Lexer.Token
module Parser = Olox.Parser
module AST = Olox.Ast
module Interpreter = Olox.Interpreter.Interpreter
module Value = Olox.Value
module Options = Olox.Options
module Env = Olox.Environment
module Error = Olox.Error
module Version = Olox.Version

let print_errors errors = List.iter Error.print_error errors
let print_usage () = print_endline "Usage: olox [--debug] [--help] [filename]"

let print_tokens (result : Token.token list) =
  result |> List.map Token.to_string |> List.iter print_endline

let print_ast result = print_endline result

let print_result = function
  | Value.LOX_VOID -> ()
  | other -> other |> Value.stringify_result |> print_endline

let ( >>= ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let run_pipeline result ~on_success ~on_error =
  match result with
  | Ok x -> on_success x
  | Error e -> on_error e

let process env file =
  Lexer.tokenize file >>= Parser.parse >>= AST.build_ast
  >>= Interpreter.interpret_ast env

let run ?env file =
  let result =
    match env with
    | Some e -> process e file
    | None -> process (Env.create ()) file
  in
  result |> run_pipeline ~on_success:print_result ~on_error:print_errors

let run_debug ?env file =
  let print_debug result printer =
    run_pipeline ~on_success:printer ~on_error:print_errors result
  in

  let tokens = Lexer.tokenize file in
  let ast = tokens >>= Parser.parse >>= AST.build_ast in
  let ast_str = ast >>= AST.to_string in

  print_endline "[{ DEBUG MODE }]" ;
  print_endline "=== LEXER OUTPUT ===" ;
  print_debug tokens print_tokens ;
  print_endline "=== AST OUTPUT ===" ;
  print_debug ast_str print_ast ;
  print_endline "=== FINAL OUTPUT ===" ;

  let result =
    match env with
    | Some e -> process e file
    | None -> process (Env.create ()) file
  in
  result |> run_pipeline ~on_success:print_result ~on_error:print_errors

let run_file ?(runner = run) filename =
  let file = In_channel.with_open_bin filename In_channel.input_all in
  runner file

let exit_prompt () = print_endline "\n\nThanks for running olox!"

let start_repl () =
  let ver = Version.version in
  print_endline
    (Printf.sprintf
       "\x1b[38;5;208m\n\
       \  ██████╗ ██╗      ██████╗ ██╗  ██╗\n\
       \ ██╔═══██╗██║     ██╔═══██╗╚██╗██╔╝\n\
       \ ██║   ██║██║     ██║   ██║ ╚███╔╝\n\
       \ ██║   ██║██║     ██║   ██║ ██╔██╗\n\
       \ ╚██████╔╝███████╗╚██████╔╝██╔╝ ██╗\n\
       \  ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═╝\x1b[0m\n\
       \ version: (%s) \n"
       ver
    )

let run_prompt ?(runner = run) () =
  start_repl () ;
  let repl_env = Env.create () in
  let rec loop () =
    print_string "\x1b[38;5;208m> \x1b[0m" ;
    flush stdout ;
    try
      match input_line stdin with
      | "exit" | "q" -> exit_prompt ()
      | line ->
          runner ~env:repl_env line ;
          loop ()
    with End_of_file -> exit_prompt ()
  in
  loop ()

let main () =
  let args' = Array.to_list Sys.argv |> List.tl in
  let flags, args =
    List.partition (fun s -> String.starts_with s ~prefix:"--") args'
  in
  Options.parse_flags flags ;
  if Options.is_active "help" then
    print_usage ()
  else
    let runner = if Options.is_active "debug" then run_debug else run in
    match args with
    | [] -> run_prompt ~runner ()
    | [ filename ] -> run_file ~runner filename
    | _ -> print_usage ()

let () = main ()
