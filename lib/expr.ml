open Token

type variable_expr = { name : token; mutable depth : int option }

type literal =
  | NUMBER of float
  | STRING of string
  | BOOLEAN of bool
  | FUNCTION of olox_function
  | CLASS of olox_class
  | INSTANCE of olox_instance

and olox_function = {
  arity : int;
  call : literal option list -> literal option;
  text : string;
  bind : literal option -> olox_function;
  init : bool;
}

and olox_class = {
  arity : int;
  call : literal option list -> literal option;
  text : string;
  methods : (string, olox_function) Hashtbl.t;
  find_method : string -> olox_function option;
}

and olox_instance = {
  klass : olox_class;
  fields : (string, literal option) Hashtbl.t;
  get : token -> literal option;
  set : token -> literal option -> unit;
}

and expr =
  | ASSIGN of { name : token; value : expr; mutable depth : int option }
  | BINARY of { left : expr; operator : token; right : expr }
  | CALL of { callee : expr; paren : token; arguments : expr list }
  | GET of { obj : expr; name : token }
  | GROUPING of { expression : expr }
  | LITERAL of literal option
  | LOGICAL of { left : expr; operator : token; right : expr }
  | SET of { obj : expr; name : token; value : expr }
  | SUPER of { keyword : token; meth : token; mutable depth : int option }
  | TERNARY of { left : expr; middle : expr; right : expr }
  | THIS_EXPR of { keyword : token; mutable depth : int option }
  | UNARY of { operator : token; right : expr }
  | VARIABLE of variable_expr

let string_of_literal literal =
  match literal with
  | Some (NUMBER e) ->
      let value = string_of_float e in
      if String.ends_with ~suffix:"." value then
        String.sub value 0 (String.length value - 1)
      else value
  | Some (STRING e) -> e
  | Some (BOOLEAN e) -> string_of_bool e
  | Some (FUNCTION e) -> e.text
  | Some (CLASS e) -> e.text
  | Some (INSTANCE e) -> "<instanceof " ^ e.klass.text ^ ">"
  | None -> "nil"

let rec expr_print expr =
  let parenthesize name exprs =
    "(" ^ name
    ^ String.concat " " (List.map (fun e -> " " ^ expr_print e) exprs)
    ^ ")"
  in
  match expr with
  | ASSIGN e -> "ASSIGN:" ^ parenthesize ("= i" ^ e.name.lexeme) [ e.value ]
  | BINARY e -> "BINARY:" ^ parenthesize e.operator.lexeme [ e.left; e.right ]
  | CALL e -> "CALL:" ^ parenthesize (expr_print e.callee) e.arguments
  | GET e -> "GET:" ^ parenthesize ("instance:" ^ e.name.lexeme) []
  | GROUPING e -> "GROUPING:" ^ parenthesize "group" [ e.expression ]
  | LITERAL e -> "LITERAL:" ^ string_of_literal e
  | LOGICAL e -> "LOGICAL:" ^ parenthesize e.operator.lexeme [ e.left; e.right ]
  | SET e -> "SET:" ^ parenthesize ("set:" ^ e.name.lexeme) [ e.value ]
  | SUPER e -> "SUPER:" ^ "super." ^ e.meth.lexeme
  | TERNARY e -> "TERNARY:" ^ parenthesize "?:" [ e.left; e.middle; e.right ]
  | THIS_EXPR e -> "THIS_EXPR:" ^ e.keyword.lexeme
  | UNARY e -> "UNARY:" ^ parenthesize e.operator.lexeme [ e.right ]
  | VARIABLE e -> "VARIABLE:" ^ e.name.lexeme
