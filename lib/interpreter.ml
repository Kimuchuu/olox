open Report
open Token
open Expr
open Stmt

exception Return of literal option

let look_up_variable name depth_o env =
  match depth_o with
  | Some d -> Environment.get_at env d name.lexeme
  | None -> Environment.get Environment.global name

let is_truthy literal =
  match literal with Some (BOOLEAN b) -> b | None -> false | _ -> true

let is_equal a b =
  match (a, b) with
  | None, None -> true
  | Some (BOOLEAN sa), Some (BOOLEAN sb) -> sa = sb
  | Some (NUMBER sa), Some (NUMBER sb) -> sa = sb
  | Some (STRING sa), Some (STRING sb) -> sa = sb
  | _ -> false

let of_number_operand literal operator =
  match literal with
  | Some (NUMBER n) -> n
  | _ -> raise (RuntimeError (operator, "Operand must be number"))

let of_number_operands literals operator =
  match literals with
  | Some (NUMBER x), Some (NUMBER y) -> (x, y)
  | _ -> raise (RuntimeError (operator, "Operands must be numbers"))

let rec expr_eval expr env =
  match expr with
  | ASSIGN e ->
      let value = expr_eval e.value env in
      (match e.depth with
      | Some d -> Environment.assign_at env d e.name value
      | None -> Environment.assign Environment.global e.name value);
      value
  | BINARY e -> (
      let left = expr_eval e.left env in
      let right = expr_eval e.right env in
      match e.operator.t with
      | BANG_EQUAL -> Some (BOOLEAN (not (is_equal left right)))
      | EQUAL_EQUAL -> Some (BOOLEAN (is_equal left right))
      | GREATER ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (BOOLEAN (l > r))
      | GREATER_EQUAL ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (BOOLEAN (l >= r))
      | LESS ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (BOOLEAN (l < r))
      | LESS_EQUAL ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (BOOLEAN (l <= r))
      | MINUS ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (NUMBER (l -. r))
      | PLUS -> (
          match (left, right) with
          | Some (NUMBER l), Some (NUMBER r) -> Some (NUMBER (l +. r))
          | Some (STRING l), Some (STRING r) -> Some (STRING (l ^ r))
          | Some (STRING l), r -> Some (STRING (l ^ string_of_literal r))
          | l, Some (STRING r) -> Some (STRING (string_of_literal l ^ r))
          | _ ->
              raise
                (RuntimeError
                   (e.operator, "Operands must be two numbers or two strings."))
          )
      | SLASH ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (NUMBER (l /. r))
      | STAR ->
          let l, r = of_number_operands (left, right) e.operator in
          Some (NUMBER (l *. r))
      | _ -> failwith ("invalid " ^ e.operator.lexeme))
  | CALL e -> (
      let callee = expr_eval e.callee env in
      let arguments = List.map (fun arg -> expr_eval arg env) e.arguments in
      let call arity call =
        let n_arguments = List.length arguments in
        if n_arguments != arity then
          raise
            (RuntimeError
               ( e.paren,
                 "Expected " ^ string_of_int arity ^ " arguments " ^ " but got "
                 ^ string_of_int n_arguments ^ "." ));
        call arguments
      in
      match callee with
      | Some (FUNCTION c) -> call c.arity c.call
      | Some (CLASS c) -> call c.arity c.call
      | _ -> raise (RuntimeError (e.paren, "Unsupported call type")))
  | GET g -> (
      match expr_eval g.obj env with
      | Some (INSTANCE instance) -> instance.get g.name
      | _ -> raise (RuntimeError (g.name, "Only instances have properties.")))
  | GROUPING e -> expr_eval e.expression env
  | LITERAL e -> e
  | LOGICAL e -> (
      let left = expr_eval e.left env in
      match e.operator.t with
      | OR -> if is_truthy left then left else expr_eval e.right env
      | _ -> if not (is_truthy left) then left else expr_eval e.right env)
  | SET e -> (
      match expr_eval e.obj env with
      | Some (INSTANCE instance) ->
          let value = expr_eval e.value env in
          instance.set e.name value;
          value
      | _ -> raise (RuntimeError (e.name, "Only instances have fields.")))
  | SUPER e -> (
      match Environment.get_at env (Option.get e.depth) "super" with
      | Some (CLASS superclass) -> (
          let obj = Environment.get_at env (Option.get e.depth - 1) "this" in
          match superclass.find_method e.meth.lexeme with
          | Some fn -> Some (FUNCTION (fn.bind obj))
          | None ->
              raise
                (RuntimeError
                   (e.meth, "Undefined property '" ^ e.meth.lexeme ^ "'.")))
      | _ -> raise (RuntimeError (e.keyword, "Super must reference a class.")))
  | TERNARY e ->
      let condition = expr_eval e.left env in
      if is_truthy condition then expr_eval e.middle env
      else expr_eval e.right env
  | THIS_EXPR e -> look_up_variable e.keyword e.depth env
  | UNARY e -> (
      let right = expr_eval e.right env in
      match e.operator.t with
      | MINUS ->
          let n = of_number_operand right e.operator in
          Some (NUMBER (-.n))
      | BANG -> Some (BOOLEAN (not (is_truthy right)))
      | _ -> failwith ("invalid operation: " ^ e.operator.lexeme))
  | VARIABLE e -> look_up_variable e.name e.depth env

and stmt_exec stmt env top =
  match stmt with
  | BLOCK_STMT stmts ->
      let block_env = Environment.create_env env in
      block_exec stmts block_env
  | CLASS_STMT s ->
      let superclass_o =
        match s.superclass with
        | Some superclass -> (
            let superclass_value = expr_eval (VARIABLE superclass) env in
            match superclass_value with
            | Some (CLASS c) -> Some c
            | _ ->
                raise
                  (RuntimeError (superclass.name, "Superclass must be a  class"))
            )
        | _ -> None
      in
      Environment.define env s.name.lexeme None;
      let class_env =
        match superclass_o with
        | Some superclass ->
            let new_env = Environment.create_env env in
            Environment.define new_env "super" (Some (CLASS superclass));
            new_env
        | None -> env
      in
      let methods : (string, olox_function) Hashtbl.t =
        Hashtbl.create (List.length s.methods)
      in
      List.iter
        (fun meth ->
          match meth with
          | FUNCTION_STMT fn ->
              Hashtbl.replace methods fn.name.lexeme
                (build_fn class_env fn.name fn.params fn.body false)
          | _ -> failwith "Expected function stmt as class method")
        s.methods;

      let find_method name =
        if Hashtbl.mem methods name then Hashtbl.find_opt methods name
        else
          match superclass_o with
          | Some superclass -> superclass.find_method name
          | None -> None
      in

      let arity =
        match find_method "init" with Some fn -> fn.arity | _ -> 0
      in

      let rec klass : olox_class =
        {
          arity;
          text = s.name.lexeme;
          methods;
          find_method;
          call =
            (fun arguments ->
              let rec instance =
                let fields : (string, literal option) Hashtbl.t =
                  Hashtbl.create 16
                in
                let get name =
                  if Hashtbl.mem fields name.lexeme then
                    Hashtbl.find fields name.lexeme
                  else
                    match find_method name.lexeme with
                    | None ->
                        raise
                          (RuntimeError
                             (name, "Undefined property '" ^ name.lexeme ^ "'."))
                    | Some fn ->
                        Some (FUNCTION (fn.bind instance))
                in
                let set name value = Hashtbl.replace fields name.lexeme value in
                Some (INSTANCE { klass; fields; get; set })
              in

              (match find_method "init" with
              | Some fn -> (
                  match fn.bind instance with fn -> ignore (fn.call arguments))
              | _ -> ());

              instance);
        }
      in
      Environment.assign env s.name (Some (CLASS klass))
  | EXPRESSION_STMT expr ->
      let value = expr_eval expr env in
      if !Global.repl && top then print_endline (string_of_literal value)
  | FUNCTION_STMT stmt ->
      Environment.define env stmt.name.lexeme
        (Some (FUNCTION (build_fn env stmt.name stmt.params stmt.body false)))
  | IF_STMT stmt -> (
      if is_truthy (expr_eval stmt.condition env) then
        stmt_exec stmt.then_branch env false
      else
        match stmt.else_branch with
        | Some branch -> stmt_exec branch env false
        | _ -> ())
  | PRINT_STMT expr ->
      let value = expr_eval expr env in
      print_endline (string_of_literal value)
  | RETURN_STMT (_token, expr_o) ->
      let value = match expr_o with Some x -> expr_eval x env | _ -> None in
      raise_notrace (Return value)
  | VARIABLE_STMT (token, expr_o) ->
      let value =
        match expr_o with Some e -> expr_eval e env | None -> None
      in
      Environment.define env token.lexeme value
  | WHILE_STMT (condition, body) ->
      while is_truthy (expr_eval condition env) do
        stmt_exec body env false
      done

and build_fn env name params body is_init : olox_function =
  {
    arity = List.length params;
    init = is_init;
    bind =
      (fun (instance : literal option) ->
        let bound_env = Environment.create_env env in
        Environment.define bound_env "this" instance;
        build_fn bound_env name params body is_init);
    call =
      (fun arguments ->
        let fenv = Environment.create_env env in
        List.iteri
          (fun i param ->
            Environment.define fenv param.lexeme (List.nth arguments i))
          params;
        try
          block_exec body fenv;
          if is_init then Environment.get_at fenv 0 "this" else None
        with Return value ->
          if is_init then Environment.get_at fenv 0 "this" else value);
    text = "<fn" ^ name.lexeme ^ ">";
  }

and block_exec stmts env =
  List.iter (fun stmt -> stmt_exec stmt env false) stmts

let init () =
  let time _arguments =
    let seconds = Unix.time () in
    Some (NUMBER seconds)
  in
  Environment.define Environment.global "clock"
    (Some
       (FUNCTION
          {
            arity = 0;
            call = time;
            text = "<native fn>";
            init = false;
            bind = (fun _ -> failwith "Can't bind built-ins to instances");
          }))

let interpret (stmts : stmt list) =
  try List.iter (fun stmt -> stmt_exec stmt Environment.global true) stmts
  with RuntimeError (token, message) -> runtime_error (token, message)
