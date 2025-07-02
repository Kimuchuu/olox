open Token
open Expr

type env = { parent : env option; values : (string, literal option) Hashtbl.t }

let global = { parent = None; values = Hashtbl.create 128 }
let create_env env = { parent = Some env; values = Hashtbl.create 128 }
let define env name value = Hashtbl.replace env.values name value

let rec print_env env i =
  print_endline ("env: " ^ string_of_int i);
  Hashtbl.iter
    (fun key value -> print_endline (key ^ "=" ^ string_of_literal value))
    env.values;
  match env.parent with Some parent -> print_env parent (i + 1) | None -> ()

let rec get env token =
  match Hashtbl.find_opt env.values token.lexeme with
  | Some v -> v
  | None -> (
      match env.parent with
      | Some parent -> get parent token
      | None -> raise (Report.RuntimeError (token, "")))

let ancestor env distance =
  let rec loop e d = if d > 0 then loop (Option.get e.parent) (d - 1) else e in
  loop env distance

let get_at env distance name =
  let at_env = ancestor env distance in
  Hashtbl.find at_env.values name

let rec assign env token value =
  match Hashtbl.find_opt env.values token.lexeme with
  | Some _ -> Hashtbl.replace env.values token.lexeme value
  | None -> (
      match env.parent with
      | Some parent -> assign parent token value
      | None ->
          raise
            (Report.RuntimeError
               (token, "Undefined variable '" ^ token.lexeme ^ "'.")))

let assign_at env distance token value =
  let at_env = ancestor env distance in
  Hashtbl.replace at_env.values token.lexeme value
