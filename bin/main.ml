open Olox

let run code =
  let tokens = Scanner.scan_tokens code in
  let statements = Parser.parse (Array.of_list tokens) in
  if not !Report.had_error then (
    List.iter Resolver.resolve_stmt statements;
    if not !Report.had_error then Interpreter.interpret statements)

let runFile name =
  print_endline ("Opening: " ^ name);
  let ic = open_in name in
  let len = in_channel_length ic in
  let result = Bytes.create len in
  really_input ic result 0 len;
  close_in ic;
  run (Bytes.to_string result);
  if !Report.had_error then exit 65;
  if !Report.had_runtime_error then exit 70

let runPrompt () =
  let rec loop () =
    let line = read_line () in
    run line;
    Report.had_error := false;
    loop ()
  in
  try loop () with End_of_file -> print_endline "good bye"

let () =
  match Array.length Sys.argv with
  | 1 ->
      Global.repl := true;
      Interpreter.init ();
      runPrompt ()
  | 2 ->
      Interpreter.init ();
      runFile Sys.argv.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
