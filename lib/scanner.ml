open Token
open Report

let keywords =
  [
    ("and", AND);
    ("class", CLASS);
    ("else", ELSE);
    ("false", FALSE);
    ("for", FOR);
    ("fun", FUN);
    ("if", IF);
    ("nil", NIL);
    ("or", OR);
    ("print", PRINT);
    ("return", RETURN);
    ("super", SUPER);
    ("this", THIS);
    ("true", TRUE);
    ("var", VAR);
    ("while", WHILE);
  ]
  |> List.to_seq |> Hashtbl.of_seq

type scanner = {
  source : string;
  mutable tokens : token list;
  mutable start : int;
  mutable current : int;
  mutable line : int;
}

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
let is_digit c = c >= '0' && c <= '9'
let is_alpha_numeric c = is_alpha c || is_digit c
let is_at_end scanner = scanner.current >= String.length scanner.source

let add_token scanner t =
  let text =
    String.sub scanner.source scanner.start (scanner.current - scanner.start)
  in
  scanner.tokens <- { t; lexeme = text; line = scanner.line } :: scanner.tokens

let advance scanner =
  let char = scanner.source.[scanner.current] in
  scanner.current <- scanner.current + 1;
  char

let peek scanner =
  if is_at_end scanner then None else Some scanner.source.[scanner.current]

let peek_next scanner =
  if scanner.current + 1 >= String.length scanner.source then None
  else Some scanner.source.[scanner.current + 1]

let matches_next scanner expected =
  if is_at_end scanner then false
  else if scanner.source.[scanner.current] != expected then false
  else (
    scanner.current <- scanner.current + 1;
    true)

let handle_identifier scanner =
  while
    match peek scanner with Some c -> is_alpha_numeric c | None -> false
  do
    ignore (advance scanner)
  done;
  let text =
    String.sub scanner.source scanner.start (scanner.current - scanner.start)
  in
  let token_type =
    match Hashtbl.find_opt keywords text with Some k -> k | None -> IDENTIFIER
  in
  add_token scanner token_type

let handle_string scanner =
  let rec loop () =
    match peek scanner with
    | Some '"' | None -> ()
    | Some c ->
        if c == '\n' then scanner.line <- scanner.line + 1;
        ignore (advance scanner);
        loop ()
  in
  loop ();
  if is_at_end scanner then error scanner.line "Unterminated string"
  else (
    ignore (advance scanner);
    let value =
      String.sub scanner.source (scanner.start + 1)
        (scanner.current - scanner.start - 2)
    in
    add_token scanner (STRING value))

let handle_number scanner =
  while match peek scanner with Some n -> is_digit n | None -> false do
    ignore (advance scanner)
  done;

  let float =
    match (peek scanner, peek_next scanner) with
    | Some c, Some n -> c == '.' && is_digit n
    | _ -> false
  in
  if float then ignore (advance scanner);
  while match peek scanner with Some c -> is_digit c | None -> false do
    ignore (advance scanner)
  done;
  let value =
    String.sub scanner.source scanner.start (scanner.current - scanner.start)
  in
  add_token scanner (NUMBER (float_of_string value))

let scan_token scanner =
  let c = advance scanner in
  match c with
  | '(' -> add_token scanner LEFT_PAREN
  | ')' -> add_token scanner RIGHT_PAREN
  | '{' -> add_token scanner LEFT_BRACE
  | '}' -> add_token scanner RIGHT_BRACE
  | ',' -> add_token scanner COMMA
  | '.' -> add_token scanner DOT
  | '-' -> add_token scanner MINUS
  | '+' -> add_token scanner PLUS
  | ';' -> add_token scanner SEMICOLON
  | '*' -> add_token scanner STAR
  | '?' -> add_token scanner QUESTION
  | ':' -> add_token scanner COLON
  | '!' ->
      add_token scanner (if matches_next scanner '=' then BANG_EQUAL else BANG)
  | '=' ->
      add_token scanner
        (if matches_next scanner '=' then EQUAL_EQUAL else EQUAL)
  | '<' ->
      add_token scanner (if matches_next scanner '=' then LESS_EQUAL else LESS)
  | '>' ->
      add_token scanner
        (if matches_next scanner '=' then GREATER_EQUAL else GREATER)
  | '/' ->
      if matches_next scanner '*' then (
        ignore (advance scanner);
        while
          match (peek scanner, peek_next scanner) with
          | Some c, Some n -> c != '*' || n != '/'
          | _ -> false
        do
          ignore (advance scanner)
        done;
        if not (is_at_end scanner) then ignore (advance scanner);
        if not (is_at_end scanner) then ignore (advance scanner))
      else if matches_next scanner '/' then
        while match peek scanner with Some c -> c != '\n' | None -> false do
          ignore (advance scanner)
        done
      else add_token scanner SLASH
  | ' ' | '\r' | '\t' -> ()
  | '\n' -> scanner.line <- scanner.line + 1
  | '"' -> handle_string scanner
  | '0' .. '9' -> handle_number scanner
  | _ ->
      if is_alpha c then handle_identifier scanner
      else error scanner.line "Unexpected character."

let scan_tokens source =
  let scanner = { source; tokens = []; start = 0; current = 0; line = 1 } in
  while not (is_at_end scanner) do
    scanner.start <- scanner.current;
    scan_token scanner
  done;
  scanner.tokens <-
    { t = EOF; lexeme = ""; line = scanner.line } :: scanner.tokens;
  List.rev scanner.tokens
