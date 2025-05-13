open MicroCamlTypes

(* Part 3: Type Inference *)

(*******************************************************************|
|**********************   Environment   ****************************|
|*******************************************************************|
| - The environment is a map that holds type information of         |
|   variables                                                       |
|*******************************************************************)
type environment = (var * typeScheme) list

exception OccursCheckException

exception UndefinedVar

type substitutions = (string * typeScheme) list

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable; T(Char.escaped (Char.chr c1))
;;

let string_of_constraints (constraints: (typeScheme * typeScheme) list) =
  List.fold_left (fun acc (l, r) -> Printf.sprintf "%s%s = %s\n" acc (string_of_type l) (string_of_type r)) "" constraints

let string_of_subs (subs: substitutions) =
  List.fold_left (fun acc (s, t) -> Printf.sprintf "%s%s: %s\n" acc s (string_of_type t)) "" subs





(*********************************************************************|
|******************   Annotate Expressions   *************************|
|*********************************************************************|
| Arguments:                                                          |
|   env -> A typing environment                                       |
|   e -> An expression that has to be annotated                       |
|*********************************************************************|
| Returns:                                                            |
|   returns an annotated expression of type aexpr that holds          |
|   type information for the given expression e.                      |
|   and the type of e                                                 |
|   and a list of typing constraints.                                 |
|*********************************************************************|
| - This method takes every expression/sub-expression in the          |
|   program and assigns some type information to it.                  |
| - This type information maybe something concrete like a TNum        |
|   or it could be a unique parameterized type(placeholder) such      |
|   as 'a.                                                            |
| - Concrete types are usually assigned when you encounter            |
|   simple literals like 10, true and "hello"                         |
| - Whereas, a random type placeholder is assigned when no            |
|   explicit information is available.                                |
| - The algorithm not only infers types of variables and              |
|   functions defined by user but also of every expression and        |
|   sub-expression since most of the inference happens from           |
|   analyzing these expressions only.                                 |
| - A constraint is a tuple of two typeSchemes. A strict equality     |
|   is being imposed on the two types.                                |
| - Constraints are generated from the expresssion being analyzed,    |
|   for e.g. for the expression ABinop(x, Add, y, t) we can constrain |
|   the types of x, y, and t to be TNum.                              |
| - In short, most of the type checking rules will be added here in   |
|   the form of constraints.                                          |
| - Further, if an expression contains sub-expressions, then          |
|   constraints need to be obtained recursively from the              |
|   subexpressions as well.                                           |
| - Lastly, constraints obtained from sub-expressions should be to    |
|   the left of the constraints obtained from the current expression  |
|   since constraints obtained from current expression holds more     |
|   information than constraints from subexpressions and also later   |
|   on we will be working with these constraints from right to left.  |
|*********************************************************************)
let rec gen (env: environment) (e: expr): aexpr * typeScheme * (typeScheme * typeScheme) list =
  match e with
  (*integers, booleans, strings, other literals*)
  | Int(n) -> AInt(n, TNum), TNum, []
  | Bool(b) -> ABool(b, TBool), TBool, []
  | String(s) -> AString(s, TStr), TStr, []
  (*variables*)
  | ID(x) ->
    (match List.assoc_opt x env with
    | Some(t) -> AID(x, t), t, []
    | None -> raise UndefinedVar)
  (*functions*)
  | Fun(x, body) ->
    let a = gen_new_type () in
    let b = gen_new_type () in
    let env' = (x, a) :: env in 
    let e_body, t_body, q_body = gen env' body in
    AFun(x, e_body, TFun(a,b)), TFun(a, b), q_body @ [(t_body,b)]
  (*Negated boolean*)
  | Not(u) ->
    let e_u, t_u, q_u = gen env u in
    ANot(e_u, TBool), TBool, q_u @ [(t_u, TBool)]
  (*Binary operations*)
  | Binop(op, u1, u2) ->
    let e1, t1, q1 = gen env u1 in
    let e2, t2, q2 = gen env u2 in
    (match op with
     | Add | Sub | Mult | Div ->
       ABinop(op, e1, e2, TNum), TNum, q1 @ q2 @ [(t1, TNum); (t2, TNum)]
     | Concat ->
       ABinop(op, e1, e2, TStr), TStr, q1 @ q2 @ [(t1, TStr); (t2, TStr)]
     | Less | Greater | Equal | LessEqual | GreaterEqual ->
       ABinop(op, e1, e2, TBool), TBool, q1 @ q2 @ [(t1, t2)]
     | NotEqual ->
       ABinop(op, e1, e2, TBool), TBool, q1 @ q2 @ [(t1, t2)]
     | Or | And ->
       ABinop(op, e1, e2, TBool), TBool, q1 @ q2 @ [(t1, TBool); (t2, TBool)])
  (*conditional*)
  | If(u1, u2, u3) ->
    let e1, t1, q1 = gen env u1 in
    let e2, t2, q2 = gen env u2 in
    let e3, t3, q3 = gen env u3 in
    AIf(e1, e2, e3, t2), t2, q1 @ q2 @ q3 @ [(t1, TBool); (t2, t3)]
  (*function calls*)
  | FunctionCall(u1, u2) ->
    let e1, t1, q1 = gen env u1 in
    let e2, t2, q2 = gen env u2 in
    let a = gen_new_type () in
    AFunctionCall(e1, e2, a), a, q1 @ q2 @ [(t1, TFun(t2, a))]
  (*let expression*)
  | Let(x, false, u1, u2) ->
  let e1, t1, q1 = gen env u1 in
  let env' = (x, t1) :: env in
  let e2, t2, q2 = gen env' u2 in
  ALet(x, false, e1, e2, t2), t2, q1 @ q2

  | Let(x, true, u1, u2) ->
    let a = gen_new_type () in
    let env' = (x, a) :: env in
    let e1, t1, q1 = gen env' u1 in
    let env'' = (x, t1) :: env in
    let e2, t2, q2 = gen env'' u2 in
    ALet(x, true, e1, e2, t2), t2, q1 @ [(a, t1)] @ q2
;;
(******************************************************************|
|**********************   Unification   ***************************|
|**********************    Algorithm    ***************************|
|******************************************************************)


(******************************************************************|
|**********************   Substitute   ****************************|
|******************************************************************|
|Arguments:                                                        |
|   t -> type in which substitutions have to be made.              |
|   (x, u) -> (type placeholder, resolved substitution)            |
|******************************************************************|
|Returns:                                                          |
|   returns a valid substitution for t if present, else t as it is.|
|******************************************************************|
|- In this method we are given a substitution rule that asks us to |
|  replace all occurrences of type placeholder x with u, in t.     |
|- We are required to apply this substitution to t recursively, so |
|  if t is a composite type that contains multiple occurrences of  |
|  x then at every position of x, a u is to be substituted.        |
*******************************************************************)
let rec substitute (u: typeScheme) (x: string) (t: typeScheme) : typeScheme =
  match t with
  | TNum | TBool | TStr -> t
  | T(c) -> if c = x then u else t
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)
;;

(******************************************************************|
|*************************    Apply    ****************************|
|******************************************************************|
| Arguments:                                                       |
|   subs -> list of substitution rules.                            |
|   t -> type in which substitutions have to be made.              |
|******************************************************************|
| Returns:                                                         |
|   returns t after all the substitutions have been made in it     |
|   given by all the substitution rules in subs.                   |
|******************************************************************|
| - Works from right to left                                       |
| - Effectively what this function does is that it uses            |
|   substitution rules generated from the unification algorithm and|
|   applies it to t. Internally it calls the substitute function   |
|   which does the actual substitution and returns the resultant   |
|   type after substitutions.                                      |
| - Substitution rules: (type placeholder, typeScheme), where we   |
|   have to replace each occurrence of the type placeholder with   |
|   the given type t.                                              |
|******************************************************************)
let apply (subs: substitutions) (t: typeScheme) : typeScheme =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;


(******************************************************************|
|***************************   Unify   ****************************|
|******************************************************************|
| Arguments:                                                       |
|   constraints -> list of constraints (tuple of 2 types)          |
|******************************************************************|
| Returns:                                                         |
|   returns a list of substitutions                                |
|******************************************************************|
| - The unify function takes a bunch of constraints it obtained    |
|   from the collect method and turns them into substitutions.     |
| - It is crucial to remember that these constraints are dependent |
|   on each other, therefore we have separate function unify_one   |
|   and unify.                                                     |
| - Since constraints on the right have more precedence we start   |
|   from the rightmost constraint and unify it by calling the      |
|   unify_one function. unify_one transforms the constraint to a   |
|   substitution. More details given below.                        |
| - Now these substitutions will be applied to both types of the   |
|   second rightmost constraint following which they will be       |
|   unified by again calling the unify_one function.               |
| - This process of unification(unify_one) and substitution(apply) |
|   goes on till all the constraints have been accounted for.      |
| - In the end we get a complete list of substitutions that helps  |
|   resolve types of all expressions in our program.               |
|******************************************************************)
let rec unify (constraints: (typeScheme * typeScheme) list) : substitutions =
  match constraints with
  | [] -> [] (*no constraints, then carry on*)
  | (x, y) :: xs -> (*grab first pair off the list of subs*)
    (* generate substitutions of the rest of the list *)
    let rest = unify xs in (*recursively unify the rest*)

    let t1' = apply rest x in (*apply substitutions to the LHS*)
    let t2' = apply rest y in (*apply substitutions to the RHS*)

    let subs = unify_one t1' t2' in (*unify the LHS and RHS*)

    subs @ (List.map (fun (x,t) -> (x, apply subs t)) rest) (*subs = subs + apply subs to all constraints in rest of subs*)

(******************************************************************|
|*************************   Unify One  ***************************|
|******************************************************************|
| Arguments:                                                       |
|   t1, t2 -> two types (one pair) that need to be unified.        |
|******************************************************************|
| Returns:                                                         |
|   returns a substitution rule for the two types if one of them   |
|   is a parameterized type else nothing.                          |
|******************************************************************|
| - A constraint is converted to a substitution here.              |
| - As mentioned several times before a substitution is nothing    |
|   but a resolution rule for a type placeholder.                  |
| - If a constraint yields multiple type resolutions then the      |
|   resolutions should be broken up into a list of constraints and |
|   be passed to the unify function.                               |
| - If both types are concrete then we need not create a new       |
|   substitution rule.                                             |
| - If the types are concrete but don't match then that indicates  |
|   a type mismatch.                                               |
|******************************************************************)
and unify_one (t1: typeScheme) (t2: typeScheme) : substitutions =
  match t1, t2 with
  | TNum, TNum | TBool, TBool | TStr, TStr -> [] (*if they're two concrete types, don't do anything*)
  | T(x), z | z, T(x) -> 
    if occurs_check x z then raise OccursCheckException
    else [(x, z)] (*if either side is a type variable, create a substitution*)
  | TFun(a, b), TFun(x, y) -> unify [(a, x); (b, y)](*if we have two function types, we need to unify them*)
  | _ -> raise (failwith "mismatched types")(*error if this fails*)
and occurs_check (x:string) (t:typeScheme) : bool = 
  match t with
  | TNum | TBool | TStr -> false
  | T(y) -> x = y
  | TFun(t1, t2) -> occurs_check x t1 || occurs_check x t2
;;

(* applies a final set of substitutions on the annotated expr *)
let rec apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABool(b, t) -> ABool(b, apply subs t)
  | AInt(n, t) -> AInt(n, apply subs t)
  | AString(s, t) -> AString(s, apply subs t)
  | AID(s, t) -> AID(s, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
  | ANot(e, t) -> ANot(apply_expr subs e, apply subs t)
  | ABinop(op, e1, e2, t) -> ABinop(op, apply_expr subs e1, apply_expr subs e2, apply subs t)
  | AIf(e1, e2, e3, t) -> AIf(apply_expr subs e1, apply_expr subs e2, apply_expr subs e3, apply subs t)
  | AFunctionCall(fn, arg, t) -> AFunctionCall(apply_expr subs fn, apply_expr subs arg, apply subs t)
  | ALet(id, b, e1, e2, t) -> ALet(id, b, apply_expr subs e1, apply_expr subs e2, apply subs t)
;;

(******************************************************************|
|**********************   Main Interface  *************************|
|******************************************************************)

(* 1. annotate expression with placeholder types and generate constraints
   2. unify types based on constraints *)
let infer (e: expr) : typeScheme =
  let env = [] in
  let ae, t, constraints = gen env e in
  (*let _ = print_string "\n"; print_string (string_of_constraints constraints) in
  let _ = print_string "\n"; print_string (string_of_aexpr ae) in *)
  let subs = unify constraints in
  (* let _ = print_string "\n"; print_string (string_of_subs subs) in *)
  (* reset the type counter after completing inference *)
  type_variable := (Char.code 'a');
  (* apply_expr subs annotated_expr *)
  apply subs t
;;
