open Token
open Expr

type stmt =
  | BLOCK_STMT of stmt list
  | CLASS_STMT of class_stmt
  | EXPRESSION_STMT of expr
  | FUNCTION_STMT of function_stmt
  | IF_STMT of if_stmt
  | PRINT_STMT of expr
  | RETURN_STMT of token * expr option
  | VARIABLE_STMT of token * expr option
  | WHILE_STMT of expr * stmt

and class_stmt = {
  name : token;
  superclass : variable_expr option;
  methods : stmt list;
}

and function_stmt = { name : token; params : token list; body : stmt list }

and if_stmt = {
  condition : expr;
  else_branch : stmt option;
  then_branch : stmt;
}
