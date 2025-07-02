open Expr
open Stmt

type fn_type = NO_FN | FUNCTION | INITIALIZER | METHOD
type class_type = NO_CLASS | CLASS | SUBCLASS

let current_fn_type = ref NO_FN
let current_class_type = ref NO_CLASS
let scopes : (string, bool) Hashtbl.t Stack.t = Stack.create ()
let begin_scope () = Stack.push (Hashtbl.create 24) scopes
let end_scope () = ignore (Stack.pop scopes)

let declare (name : Token.token) =
  if not (Stack.is_empty scopes) then (
    let scope = Stack.top scopes in
    if Hashtbl.mem scope name.lexeme then
      Report.error_token name "Already a variable with this name in this scope.";
    Hashtbl.replace scope name.lexeme false)

let define (name : Token.token) =
  if not (Stack.is_empty scopes) then
    let scope = Stack.top scopes in
    Hashtbl.replace scope name.lexeme true

let resolve_local (name : Token.token) =
  let index_o =
    scopes |> Stack.to_seq
    |> Seq.find_index (fun s -> Option.is_some (Hashtbl.find_opt s name.lexeme))
  in
  index_o

let rec resolve_expr expr =
  match expr with
  | ASSIGN ass ->
      resolve_expr ass.value;
      let at = resolve_local ass.name in
      ass.depth <- at
  | BINARY b ->
      resolve_expr b.left;
      resolve_expr b.right
  | CALL c ->
      resolve_expr c.callee;
      List.iter resolve_expr c.arguments
  | GET g -> resolve_expr g.obj
  | GROUPING group -> resolve_expr group.expression
  | LITERAL _ -> ()
  | LOGICAL b ->
      resolve_expr b.left;
      resolve_expr b.right
  | SET g ->
      resolve_expr g.value;
      resolve_expr g.obj
  | SUPER s ->
      if !current_class_type = NO_CLASS then
        Report.error_token s.keyword "Can't use 'super' outside of a class."
      else if !current_class_type = CLASS then
        Report.error_token s.keyword
          "Can't use 'super' in a class with no superclass.";
      let at = resolve_local s.keyword in
      s.depth <- at
  | TERNARY t ->
      resolve_expr t.left;
      resolve_expr t.middle;
      resolve_expr t.right
  | THIS_EXPR t ->
      if !current_class_type = NO_CLASS then
        Report.error_token t.keyword "Can't return from top-level code."
      else
        let at = resolve_local t.keyword in
        t.depth <- at
  | UNARY u -> resolve_expr u.right
  | VARIABLE v ->
      if
        (not (Stack.is_empty scopes))
        && Hashtbl.find_opt (Stack.top scopes) v.name.lexeme = Some false
      then
        Report.error_token v.name
          "Can't read local variable in its own initializer.";
      let at = resolve_local v.name in
      v.depth <- at

let rec resolve_stmt stmt =
  match stmt with
  | BLOCK_STMT stmts ->
      begin_scope ();
      List.iter resolve_stmt stmts;
      end_scope ()
  | CLASS_STMT s ->
      let has_super = Option.is_some s.superclass in
      let enclosing_class_t = !current_class_type in
      current_class_type := if has_super then SUBCLASS else CLASS;
      declare s.name;
      define s.name;
      if has_super then (
        let superclass = Option.get s.superclass in
        if s.name.lexeme = superclass.name.lexeme then
          Report.error_token superclass.name "A class can't inherit from itself";
        resolve_expr (VARIABLE superclass);
        begin_scope ();
        Hashtbl.replace (Stack.top scopes) "super" true);
      begin_scope ();
      Hashtbl.replace (Stack.top scopes) "this" true;
      List.iter
        (fun method_stmt ->
          match method_stmt with
          | FUNCTION_STMT fn ->
              let t =
                if fn.name.lexeme = "init" then INITIALIZER else FUNCTION
              in
              resolve_function fn.params fn.body t
          | _ ->
              failwith
                ("Tried to define non-function statement as method in class: "
               ^ s.name.lexeme ^ "."))
        s.methods;
      end_scope ();
      if has_super then end_scope ();
      current_class_type := enclosing_class_t
  | EXPRESSION_STMT s -> resolve_expr s
  | FUNCTION_STMT fn ->
      declare fn.name;
      define fn.name;
      resolve_function fn.params fn.body FUNCTION
  | IF_STMT sif ->
      resolve_expr sif.condition;
      resolve_stmt sif.then_branch;
      if Option.is_some sif.else_branch then
        resolve_stmt (Option.get sif.else_branch)
  | PRINT_STMT s -> resolve_expr s
  | RETURN_STMT (token, e_o) -> (
      if !current_fn_type == NO_FN then
        Report.error_token token "Can't return from top-level code.";
      match e_o with
      | Some e ->
          if !current_fn_type == INITIALIZER then
            Report.error_token token
              "Can't return from a value from an initializer.";
          resolve_expr e
      | None -> ())
  | VARIABLE_STMT (s, e) ->
      declare s;
      if Option.is_some e then resolve_expr (Option.get e);
      define s
  | WHILE_STMT (e, s) ->
      resolve_expr e;
      resolve_stmt s

and resolve_function params body t =
  let enclosing_fn_t = !current_fn_type in
  current_fn_type := t;
  begin_scope ();
  List.iter
    (fun param ->
      declare param;
      define param)
    params;
  List.iter resolve_stmt body;
  end_scope ();
  current_fn_type := enclosing_fn_t
