open Token
open Expr
open Stmt

exception ParserError

type parser = { tokens : token array; mutable current : int }

let p_peek parser = parser.tokens.(parser.current)
let p_prev parser = parser.tokens.(parser.current - 1)
let p_at_end parser = (p_peek parser).t == EOF

let p_advance parser =
  if not (p_at_end parser) then parser.current <- parser.current + 1;
  p_prev parser

let p_check parser token_type =
  if p_at_end parser then false else (p_peek parser).t == token_type

let p_match parser token_types =
  let is_of_type = Array.exists (fun tt -> p_check parser tt) token_types in
  if is_of_type then ignore (p_advance parser);
  is_of_type

let p_error (token : token) message =
  Report.error_token token message;
  ParserError

let p_synchronize parser =
  ignore (p_advance parser);
  let rec loop () =
    if p_at_end parser || (p_prev parser).t == SEMICOLON then ()
    else
      match (p_peek parser).t with
      | CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN -> ()
      | _ ->
          ignore (p_advance parser);
          loop ()
  in
  loop ()

let p_consume parser t message =
  if p_check parser t then p_advance parser
  else raise (p_error (p_peek parser) message)

let rec p_primary parser =
  let token = p_peek parser in
  match token.t with
  | FALSE ->
      ignore (p_advance parser);
      LITERAL (Some (BOOLEAN false))
  | TRUE ->
      ignore (p_advance parser);
      LITERAL (Some (BOOLEAN true))
  | NIL ->
      ignore (p_advance parser);
      LITERAL None
  | NUMBER n ->
      ignore (p_advance parser);
      LITERAL (Some (NUMBER n))
  | STRING s ->
      ignore (p_advance parser);
      LITERAL (Some (STRING s))
  | LEFT_PAREN ->
      ignore (p_advance parser);
      let expr = p_expression parser in
      ignore (p_consume parser RIGHT_PAREN "Expect ')' after expression");
      GROUPING { expression = expr }
  (* Handle binary operator at beginning of expression.
   * Should we discard the rigth hand operand instead of returning it? *)
  | COMMA ->
      let operator = p_advance parser in
      ignore (p_error operator "Missing left-hand operand");
      p_ternary parser
  | BANG_EQUAL | EQUAL_EQUAL ->
      let operator = p_advance parser in
      ignore (p_error operator "Missing left-hand operand");
      p_comparison parser
  | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ->
      let operator = p_advance parser in
      ignore (p_error operator "Missing left-hand operand");
      p_term parser
  | MINUS | PLUS ->
      let operator = p_advance parser in
      ignore (p_error operator "Missing left-hand operand");
      p_factor parser
  | SLASH | STAR ->
      let operator = p_advance parser in
      ignore (p_error operator "Missing left-hand operand");
      p_primary parser
  | SUPER ->
      ignore (p_advance parser);
      let keyword = p_prev parser in
      ignore (p_consume parser DOT "Expect '.' after 'super'.");
      let meth = p_consume parser IDENTIFIER "Expect superclass method name." in
      SUPER { keyword; meth; depth = None }
  | THIS ->
      ignore (p_advance parser);
      THIS_EXPR { keyword = p_prev parser; depth = None }
  | IDENTIFIER ->
      ignore (p_advance parser);
      VARIABLE { name = p_prev parser; depth = None }
  | _ -> raise (p_error token "Expect expression")

and p_finish_call parser callee =
  let arguments =
    if not (p_check parser RIGHT_PAREN) then
      let rec loop exprs =
        let new_exprs = p_assignment parser :: exprs in
        if List.length new_exprs >= 255 then
          ignore (p_error (p_peek parser) "Can't have more than 255 arguments.");
        if p_match parser [| COMMA |] then loop new_exprs
        else List.rev new_exprs
      in
      loop []
    else []
  in
  let paren = p_consume parser RIGHT_PAREN "Expect ')' after arguments." in
  CALL { callee; paren; arguments }

and p_call parser =
  let rec loop expr =
    if p_match parser [| LEFT_PAREN |] then loop (p_finish_call parser expr)
    else if p_match parser [| DOT |] then
      let name =
        p_consume parser IDENTIFIER "Expect property name after '.'."
      in
      loop (GET { obj = expr; name })
    else expr
  in
  loop (p_primary parser)

and p_unary parser =
  if p_match parser [| BANG; MINUS |] then
    let operator = p_prev parser in
    let right = p_unary parser in
    UNARY { operator; right }
  else p_call parser

and p_factor parser =
  let expr = ref (p_unary parser) in
  while p_match parser [| SLASH; STAR |] do
    let operator = p_prev parser in
    let right = p_unary parser in
    expr := BINARY { left = !expr; operator; right }
  done;
  !expr

and p_term parser =
  let expr = ref (p_factor parser) in
  while p_match parser [| MINUS; PLUS |] do
    let operator = p_prev parser in
    let right = p_factor parser in
    expr := BINARY { left = !expr; operator; right }
  done;
  !expr

and p_comparison parser =
  let expr = ref (p_term parser) in
  while p_match parser [| GREATER; GREATER_EQUAL; LESS; LESS_EQUAL |] do
    let operator = p_prev parser in
    let right = p_term parser in
    expr := BINARY { left = !expr; operator; right }
  done;
  !expr

and p_equality parser =
  let expr = ref (p_comparison parser) in
  while p_match parser [| BANG_EQUAL; EQUAL_EQUAL |] do
    let operator = p_prev parser in
    let right = p_comparison parser in
    expr := BINARY { left = !expr; operator; right }
  done;
  !expr

and p_ternary parser =
  let expr = p_equality parser in
  if p_match parser [| QUESTION |] then (
    let middle = p_ternary parser in
    ignore (p_consume parser COLON "Expect ':' after ? expression");
    let right = p_ternary parser in
    TERNARY { left = expr; middle; right })
  else expr

and p_and parser =
  let expr = ref (p_ternary parser) in
  while p_match parser [| AND |] do
    let operator = p_prev parser in
    let right = p_ternary parser in
    expr := LOGICAL { left = !expr; operator; right }
  done;
  !expr

and p_or parser =
  let expr = ref (p_and parser) in
  while p_match parser [| OR |] do
    let operator = p_prev parser in
    let right = p_and parser in
    expr := LOGICAL { left = !expr; operator; right }
  done;
  !expr

and p_assignment parser =
  let expr = p_or parser in
  if p_match parser [| EQUAL |] then (
    let equals = p_prev parser in
    let value = p_assignment parser in
    match expr with
    | VARIABLE e -> ASSIGN { name = e.name; value; depth = None }
    | GET e -> SET { obj = e.obj; name = e.name; value }
    | _ ->
        ignore (p_error equals "Invalid assignment target");
        expr)
  else expr

and p_comma parser =
  let expr = ref (p_assignment parser) in
  while p_match parser [| COMMA |] do
    let operator = p_prev parser in
    let right = p_assignment parser in
    expr := BINARY { left = !expr; operator; right }
  done;
  !expr

and p_expression parser = p_comma parser

and p_expression_statement parser =
  let expr = p_expression parser in
  ignore (p_consume parser SEMICOLON "Expect ';' after value.");
  EXPRESSION_STMT expr

and p_function parser kind =
  let name = p_consume parser IDENTIFIER ("Expect " ^ kind ^ " name.") in
  ignore (p_consume parser LEFT_PAREN ("Expect '(' after " ^ kind ^ " name."));
  let params =
    if not (p_check parser RIGHT_PAREN) then
      let rec loop plist =
        if List.length plist >= 255 then
          ignore
            (p_error (p_peek parser) "Can't have more than 255 parameters.");
        let new_plist =
          p_consume parser IDENTIFIER "Expenct parameter name." :: plist
        in
        if p_match parser [| COMMA |] then loop new_plist
        else List.rev new_plist
      in
      loop []
    else []
  in
  ignore (p_consume parser RIGHT_PAREN "Expect ')' after parameters.");
  ignore (p_consume parser LEFT_BRACE ("Expect '{' before " ^ kind ^ " body."));
  let body = p_block_statement parser in
  FUNCTION_STMT { name; params; body }

and p_block_statement parser =
  let rec loop items =
    if (not (p_check parser RIGHT_BRACE)) && not (p_at_end parser) then
      match p_declaration parser with
      | Some d -> loop (d :: items)
      | _ -> loop items
    else List.rev items
  in
  let statements = loop [] in
  ignore (p_consume parser RIGHT_BRACE "Expect '}' after block.");
  statements

and p_print_statement parser =
  let value = p_expression parser in
  ignore (p_consume parser SEMICOLON "Expect ';' after value.");
  PRINT_STMT value

and p_return_statement parser =
  let token = p_prev parser in
  let value =
    if not (p_check parser SEMICOLON) then Some (p_expression parser) else None
  in
  ignore (p_consume parser SEMICOLON "Expect ';' after return value.");
  RETURN_STMT (token, value)

and p_if_statement parser =
  ignore (p_consume parser LEFT_PAREN "Expect '(' after 'if'.");
  let condition = p_expression parser in
  ignore (p_consume parser RIGHT_PAREN "Expect ')' after if condition.");
  let then_branch = p_statement parser in
  let else_branch =
    if p_match parser [| ELSE |] then Some (p_statement parser) else None
  in
  IF_STMT { condition; then_branch; else_branch }

and p_for_statement parser =
  ignore (p_consume parser LEFT_PAREN "Expect '(' after 'for.");
  let initialiser =
    if p_match parser [| SEMICOLON |] then None
    else if p_match parser [| VAR |] then Some (p_var_declaration parser)
    else Some (p_expression_statement parser)
  in
  let condition =
    if not (p_check parser SEMICOLON) then p_expression parser
    else LITERAL (Some (BOOLEAN true))
  in
  ignore (p_consume parser SEMICOLON "Expect ';' after loop condition.");
  let increment =
    if not (p_check parser RIGHT_PAREN) then Some (p_expression parser)
    else None
  in
  ignore (p_consume parser RIGHT_PAREN "Expect ')' after for clauses.");
  let body = p_statement parser in
  let body_with_increment =
    match increment with
    | None -> body
    | Some inc_expr -> (
        let inc_stmt = EXPRESSION_STMT inc_expr in
        match body with
        | BLOCK_STMT stmts -> BLOCK_STMT (stmts @ [ inc_stmt ])
        | other -> BLOCK_STMT [ other; inc_stmt ])
  in
  let while_stmt = WHILE_STMT (condition, body_with_increment) in
  match initialiser with
  | None -> while_stmt
  | Some init_stmt -> BLOCK_STMT [ init_stmt; while_stmt ]

and p_statement parser =
  if p_match parser [| FOR |] then p_for_statement parser
  else if p_match parser [| IF |] then p_if_statement parser
  else if p_match parser [| LEFT_BRACE |] then
    BLOCK_STMT (p_block_statement parser)
  else if p_match parser [| PRINT |] then p_print_statement parser
  else if p_match parser [| RETURN |] then p_return_statement parser
  else if p_match parser [| WHILE |] then p_while_statement parser
  else p_expression_statement parser

and p_var_declaration parser =
  let token = p_consume parser IDENTIFIER "Expect variable name." in
  let initial =
    if p_match parser [| EQUAL |] then Some (p_expression parser) else None
  in
  ignore (p_consume parser SEMICOLON "Expect ';' after variable declaration.");
  VARIABLE_STMT (token, initial)

and p_while_statement parser =
  ignore (p_consume parser LEFT_PAREN "Expect '(' after 'while.");
  let condition = p_expression parser in
  ignore (p_consume parser RIGHT_PAREN "Expect ')' after while condition.");
  let body = p_statement parser in
  WHILE_STMT (condition, body)

and p_class_declaration parser =
  let name = p_consume parser IDENTIFIER "Expect class name." in
  let superclass =
    if p_match parser [| LESS |] then (
      ignore (p_consume parser IDENTIFIER "Expect superclass name.");
      Some { name = p_prev parser; depth = None })
    else None
  in
  ignore (p_consume parser LEFT_BRACE "Expect '{' before class body.");
  let rec loop items =
    if (not (p_check parser RIGHT_BRACE)) && not (p_at_end parser) then
      loop (p_function parser "method" :: items)
    else List.rev items
  in
  let methods = loop [] in
  ignore (p_consume parser RIGHT_BRACE "Expect '}' after class body.");
  CLASS_STMT { name; superclass; methods }

and p_declaration parser =
  try
    if p_match parser [| CLASS |] then Some (p_class_declaration parser)
    else if p_match parser [| FUN |] then Some (p_function parser "function")
    else if p_match parser [| VAR |] then Some (p_var_declaration parser)
    else Some (p_statement parser)
  with ParserError ->
    p_synchronize parser;
    None

let parse tokens =
  let parser = { tokens; current = 0 } in
  let rec loop list =
    if p_at_end parser then List.rev list
    else
      loop
        (match p_declaration parser with Some e -> e :: list | None -> list)
  in
  loop []
