type t =
  | ScanError of int * string
  | ParseError of int * string
  | RuntimeError of int * string

let corpus line label msg = Printf.sprintf "[line %d] %s: %s" line label msg

let to_string = function
  | ScanError (line, msg) -> corpus line "ScanError" msg
  | ParseError (line, msg) -> corpus line "ParseError" msg
  | RuntimeError (line, msg) -> corpus line "RuntimeError" msg

let print_error e = print_endline (to_string e)
