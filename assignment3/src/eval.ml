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


let rec eval_expr (e : exp) (env : environment) : value =
  match e with (*match expression*)
  (*if variable usage, try to find it in environment, else raise undefined*)
  | Var x -> (try List.assoc x env with Not_found -> raise UndefinedVar)
  (*if we have a number expression, evaluate to Int_Val*)
  | Number n -> Int_Val n
  (*If we have addition fo two values*)
  | Plus (e1, e2) ->
      (*If we have to integers add them together else typeerror*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 + v2)
       | _ -> raise TypeError)
  | Minus (e1, e2) ->
      (*same as plus*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 - v2)
       | _ -> raise TypeError)
  | Times (e1, e2) ->
      (*same as plus*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Int_Val (v1 * v2)
       | _ -> raise TypeError)
  | Div (e1, e2) ->
      (*same as plus*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 ->
           if v2 = 0 then raise DivByZeroError else Int_Val (v1 / v2)
       | _ -> raise TypeError)
  | Mod (e1, e2) ->
      (*same as plus*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 ->
           if v2 = 0 then raise DivByZeroError else Int_Val (v1 mod v2)
       | _ -> raise TypeError)
  | Eq (e1, e2) ->
      (*equality works with either bool or int val*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 = v2)
       | Bool_Val b1, Bool_Val b2 -> Bool_Val (b1 = b2)
       | _ -> raise TypeError)
  | Leq (e1, e2) ->
      (*return bool for leq*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <= v2)
       | _ -> raise TypeError)
  | Lt (e1, e2) ->
      (*same as leq*)
      (match eval_expr e1 env, eval_expr e2 env with
       | Int_Val v1, Int_Val v2 -> Bool_Val (v1 < v2)
       | _ -> raise TypeError)
  (*bool literals*)
  | True -> Bool_Val true
  | False -> Bool_Val false
  (*bool operations, self explanatory*)
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
  (*function definition*)
  | Fun (x, body) -> Closure (env, x, body)
  (*apply a a function *)
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
      (* Check if the variable x is declared in the environment *)
      if not (List.mem_assoc x env) then raise UndefinedVar;
      (* Evaluate the expression e *)
      let v = eval_expr e env in
      (* Update the environment with the new value of x *)
      (x, v) :: List.remove_assoc x env
  | Declare (t, x) ->
      (*declare variable x of type t*)
      let default_val = match t with
        | Int_Type -> Int_Val 0
        | Bool_Type -> Bool_Val false
        | Lambda_Type -> Closure (env, "x", Var "x") in
      (x, default_val) :: env
      (*compose a program recursively. evaluate, mutate environment, then evaluate again*)
  | Comp (c1, c2) ->
      let env' = eval_command c1 env in
      eval_command c2 env'
  (*conditional execution*)
  | Cond (e, c1, c2) ->
      (match eval_expr e env with
       | Bool_Val true -> eval_command c1 env
       | Bool_Val false -> eval_command c2 env
       | _ -> raise TypeError)
  (*while loop. recursively run while condition is true*)
  | While (e, body) ->
      let rec loop env =
        match eval_expr e env with
        | Bool_Val true -> loop (eval_command body env)
        | Bool_Val false -> env
        | _ -> raise TypeError in
      loop env
  (*for loop, similar to while loop but with a count rather than a condition*)
  | For (e, body) ->
      (match eval_expr e env with
       | Int_Val n when n >= 0 ->
           let rec loop env count =
             if count = 0 then env
             else loop (eval_command body env) (count - 1)
           in
           loop env n
       | Int_Val _ -> raise TypeError
       | _ -> raise TypeError)
    (*pass*)
  | Skip -> env
