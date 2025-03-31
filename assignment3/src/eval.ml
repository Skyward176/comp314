open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Print env to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Print env to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  | Var x -> (try List.assoc x env with Not_found -> raise UndefinedVar)
  | Number n -> Int_Val n
  | Plus (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 + v2)
       | _ -> raise TypeError)
  | Minus (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 - v2)
       | _ -> raise TypeError)
  | Times (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 * v2)
       | _ -> raise TypeError)
  | Div (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 ->
           if v2 = 0 then raise DivByZeroError else Int_Val (v1 / v2)
       | _ -> raise TypeError)
  | Mod (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 ->
           if v2 = 0 then raise DivByZeroError else Int_Val (v1 mod v2)
       | _ -> raise TypeError)
  | Eq (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 = v2)
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 = b2)
       | _ -> raise TypeError)
  | Leq (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <= v2)
       | _ -> raise TypeError)
  | Lt (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 < v2)
       | _ -> raise TypeError)
  | True -> Bool_Val true
  | False -> Bool_Val false
  | Not e ->
      (match eval_expr e env with
       | Bool_Val b -> Bool_Val (not b)
       | _ -> raise TypeError)
  | And (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 && b2)
       | _ -> raise TypeError)
  | Or (e1, e2) ->
      (match eval_expr e1 env, eval_expr e2 env with
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 || b2)
       | _ -> raise TypeError)
  | Fun (x, body) -> Closure (env, x, body)
  | App (e1, e2) ->
      (match eval_expr e1 env with
       | Closure (closure_env, param, body) ->
           let arg_val = eval_expr e2 env in
           eval_expr body ((param, arg_val) :: closure_env)
       | _ -> raise TypeError)

(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  | Assg (x, e) ->
      let v = eval_expr e env in
      (x, v) :: List.remove_assoc x env
  | Declare (t, x) ->
      let default_val = match t with
        | Int_Type -> Int_Val 0
        | Bool_Type -> Bool_Val false
        | Lambda_Type -> raise TypeError in
      (x, default_val) :: env
  | Comp (c1, c2) ->
      let env' = eval_command c1 env in
      eval_command c2 env'
  | Cond (e, c1, c2) ->
      (match eval_expr e env with
       | Bool_Val true -> eval_command c1 env
       | Bool_Val false -> eval_command c2 env
       | _ -> raise TypeError)
  | While (e, body) ->
      let rec loop env =
        match eval_expr e env with
        | Bool_Val true -> loop (eval_command body env)
        | Bool_Val false -> env
        | _ -> raise TypeError in
      loop env
  (*| For (_, _) -> raise Not_implemeted (* Placeholder for For loop *)*)
  | Skip -> env
