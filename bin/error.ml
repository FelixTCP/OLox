type t =
  | ScanError of int * string
  | ParseError of int * string
  | RuntimeError of int * string

let corpus line label msg =
  let light_grey = "\x1b[37m" in
  let red = "\x1b[31m" in
  let reset = "\x1b[0m" in
  Printf.sprintf "%s%d | " light_grey line
  ^ Printf.sprintf "%s%s:%s " red label reset
  ^ Printf.sprintf "%s" msg

let to_string = function
  | ScanError (line, msg) -> corpus line "ScanError" msg
  | ParseError (line, msg) -> corpus line "ParseError" msg
  | RuntimeError (line, msg) -> corpus line "RuntimeError" msg

let print_error e = print_endline (to_string e)
