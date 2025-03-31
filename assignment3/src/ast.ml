(* IMP abstract syntax *)
type value =
  | Int_Val of int
  | Bool_Val of bool
  | Closure of environment * string * exp

and exp =
  | Var of string
  | Number of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | Eq of exp * exp
  | Leq of exp * exp
  | Lt of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | True
  | False
  | App of exp * exp
  | Fun of string * exp

and environment = (string * value) list

type com =
  | While of exp * com
  | For   of exp * com
  | Cond of exp * com * com
  | Comp of com * com
  | Assg of string * exp
  | Declare of dtype * string
  | Skip

and dtype =
  | Int_Type
  | Bool_Type
  | Lambda_Type

let rec indent n =
  if n <= 0 then ""
  else
    " " ^ (indent (n - 1))



let print_dtype t =
  match t with
  | Int_Type -> "int"
  | Bool_Type -> "bool"
  | Lambda_Type -> "lambda"

let rec print_exp e =
  match e with
  | Var n -> n
  | Number n -> string_of_int n
  | Plus (e1, e2) -> (print_exp e1) ^ " + " ^ (print_exp e2)
  | Minus (e1, e2) -> (print_exp e1) ^ " - " ^ (print_exp e2)
  | Times (e1, e2) -> (print_exp e1) ^ " * " ^ (print_exp e2)
  | Div (e1, e2) -> (print_exp e1) ^ " + " ^ (print_exp e2)
  | Mod (e1, e2) -> (print_exp e1) ^ " % " ^ (print_exp e2)
  | Eq (e1, e2) -> (print_exp e1) ^ " == " ^ (print_exp e2)
  | Leq (e1, e2) -> (print_exp e1) ^ " <= " ^ (print_exp e2)
  | Lt (e1, e2) -> (print_exp e1) ^ " < " ^ (print_exp e2)
  | Not e -> "Not" ^ (print_exp e)
  | And (e1, e2) -> (print_exp e1) ^ " && " ^ (print_exp e2)
  | Or (e1, e2) -> (print_exp e1) ^ " || " ^ (print_exp e2)
  | True -> "True"
  | False -> "False"
  | App (e1, e2) -> (print_exp e1) ^ " " ^ (print_exp e2)
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ (print_exp e)

let print_com com =
  let rec helper com space =
    match com with
    | While (e, c) ->
      (indent space) ^ "while (" ^ print_exp e ^ ") {\n" ^
      (helper c (space+1)) ^ "\n" ^
      (indent space) ^ "};"
    | For (e, c) ->
      (indent space) ^ "For (" ^ print_exp e ^ ") {\n" ^
      (helper c (space+1)) ^
      (indent space) ^ "};"
    | Cond (e, c1, c2) ->
      (indent space) ^ "if (" ^ print_exp e ^ ") {\n" ^
      (helper c1 (space+1)) ^
      (indent space) ^ "} else {" ^
      (helper c2 (space+1)) ^
      (indent space) ^ "};"
    | Comp (c1, c2) ->
      (helper c1 space) ^ "\n" ^
      (helper c2 space)
    | Assg (x, e) ->
      (indent space) ^ x ^ " := " ^ (print_exp e) ^ ";"
    | Declare (t, x) ->
      (indent space) ^ (print_dtype t) ^ " " ^ x ^ ";"
    | Skip ->
      "" in
  helper com 0

let print_val v =
  match v with
  | Int_Val i -> string_of_int i
  | Bool_Val b -> string_of_bool b
  | Closure (_, x, e) -> "fun x -> " ^ (print_exp e)
