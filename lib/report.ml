open Token

exception RuntimeError of token * string

let had_error = ref false
let had_runtime_error = ref false

let report line where message =
  print_endline
    ("[line " ^ string_of_int line ^ "] Error" ^ where ^ ": " ^ message);
  had_error := true

let error_token (token : Token.token) message =
  match token.t with
  | EOF -> report token.line " at end" message
  | _ -> report token.line (" at '" ^ token.lexeme ^ "'") message

let error line message = report line "" message

let runtime_error (token, message) =
  print_endline (message ^ "\n[line " ^ string_of_int token.line ^ "]");
  had_runtime_error := true
