
# MicroCaml TypeChecker.


## Introduction

Over the course of this project, you will implement type inference for MicroCaml — a subset of OCaml.

### Ground Rules

In your code, you may use any OCaml modules and features we have taught in this class **except imperative OCaml** features like references, mutable records, and arrays.

### Testing & Submitting

Please have all your code in [infer.ml](./infer.ml).

Please submit **infer.ml** to Canvas. To test locally, compile your code with `make` and run `./frontend`.

If you do not have `make`, you can compile the code by `ocamlc -o frontend -I +str microCamlTypes.ml infer.ml main.ml`.

All tests will be run on direct calls to your code, comparing your return values to the expected return values. Any other output (e.g., for your own debugging) will be ignored. You are free and encouraged to have additional output.

### AST

Below is the AST type `expr`.

```ocaml
type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | ID of var
  | Fun of var * expr (* an anonymous function: var is the parameter and expr is the body *)
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr (* bool determines whether var is recursive *)
```

### Typing Constraint Generation

As stated in the lecture 13, type inference consists of three steps: (1) Annotate (2) Generate Constraints (3) Solve Constraints. In this part of the project, we will implement a function `gen` for both annotation and constraint generation:
```ocaml
let rec gen (env: environment) (e: expr): aexpr * typeScheme * (typeScheme * typeScheme) list = ...
```
which takes a typing environment `env` and an AST for an input expression of type `expr` and outputs an annotated expression of type `aexpr` that holds the type information for the (sub-expressions) of the given expression `e`, the type of `e` which is represented as a `typeScheme`, and a list of typing constraints `(typeScheme * typeScheme) list`. We provide detailed information about `environment`, `aexpr`, `typeScheme` below.

We defined the typing environment in [infer.ml](./infer.ml):
```ocaml
type environment = (var * typeScheme) list
```
An environment is a map that holds the type information `typeScheme` of any variables `var` currently in scope. As an example, consider an expression `let x = 5 in let y = 10 in x + y`. The typing environment of the expression `x+y` is a list `[(y:int); (x: int)]` that maps both `x` and `y` to the `int` type.

We defined the data structure for `typeScheme` in [microCamlTypes.ml](./microCamlTypes.ml):
```ocaml
type typeScheme =
    | TNum                                  (int type)
    | TBool                                 (bool type)
    | TStr                                  (string type)
    | T of string                           (type variable for an unknown type)
    | TFun of typeScheme * typeScheme       (function type)
```
More information about type schemes can be found in the lecture 13 slides. As an example, consider a function `fun x -> x 1`. Its OCaml type is `(int -> 'a) -> 'a` (the higher order function takes a function `x` as input and returns the results of applying the function `x` to 1). In our project, this type is written as  `TFun(TFun(TNum, T "'a"), T "'a")`.

We defined the data structure for an expression annotated with types as `aexpr` in [microCamlTypes.ml](./microCamlTypes.ml):
```ocaml
type aexpr =
  | AInt of int * typeScheme
  | ABool of bool * typeScheme
  | AString of string * typeScheme
  | AID of var * typeScheme
  | AFun of var * aexpr * typeScheme
  | ANot of aexpr * typeScheme
  | ABinop of op * aexpr * aexpr * typeScheme
  | AIf of aexpr * aexpr * aexpr * typeScheme
  | AFunctionCall of aexpr * aexpr * typeScheme
  | ALet of var * bool * aexpr * aexpr * typeScheme
```
It follows `type expr` defined above except that any `aexpr` expression must be paired with a type scheme which holds the type information of the expression. For example, `5` should be annotated as `AInt (5, TNum)`. As another example, consider the anonymous function `fun x -> x 1`. As we initially have no idea of the type of the function parameter `x`, we use a type scheme `T "a"` to represent the unknown type of `x`. Similarly, we do not know the type of the return of the anonymous function so we use a type scheme `T "b"` to represent this unknown type. Observe that `x` is applied to `1` in the body of the anonymous function. As we have no knowledge about the return type of the function `x`, we once again annotate `x 1` a type scheme `T "c"`. Thus, the annotated expression is:
```ocaml
(fun x -> ((x: a) (1: int)): c): (a -> b)
```
or in OCaml code:
```ocaml
AFun("x", AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), TFun(T "a", T "b"))
```
It is important to note that the annotation is recursive as every subexpression is also annotated in the above example.

The third element in the return of `gen env e` is a list of typing constraints `(typeScheme * typeScheme) list` collected by recursively traversing the input expression `e` under the typing environment `env`. For the anonymous function `fun x -> x 1`, the following typing constraints should be generated:
```
a = (int -> c)
c = b
```
or in OCaml code:
```ocaml
[(T "a", TFun(TNum, T "c"));
 (T "c", T "b")]
```
In the first constraint, `T "a"` is the annotated type of `x` (see above). In the body of the anonymous function, `x` is used a function that applies to an integer (`TNum`) and the function application is annotated with `T "c"`. We constrain `T "a"` same as `TFun(TNum, T "c")`. The second constraint is derived from the fact that the result of the function application `x 1`, annotated with type `T "c"`, is used as the return value of the anonymous function, annotated with return type `T "b"`, so we constrain `T "c"` same as `T "b"`.

Formally, the function `gen` should implement the following typing rules to collect the typing constraints. The rules are (recursively) defined in the shape of `G |- u ==> e, t, q` where `G` is a typing environment (initialized to []), `u` is an unannotated expression of type `expr`, `e` is an annotated expression of type `aexpr`, `t` is the annotated type of `u`, and `q` is the list of typing constraints for `u` that must be solved. In other words, the following typing rules jointly define `gen G u = e t q`.

1. Typing the integers.
```
------------------------------
G |- n ==> (n: int), int, []
```

For example, `gen [] (Int 5) = AInt (5, TNum), TNum, []`. No typing constraint is generated.

2. Typing the Booleans.
```
------------------------------
G |- b ==> (b: bool), bool, []
```

For example, `gen [] (Bool true) = ABool (true, TBool), TBool, []`. No typing constraint is generated.

3. Typing the strings.
```
----------------------------------
G |- s ==> (s: string), string, []
```

For example, `gen [] (String "CS314") = AString ("CS314", TStr), TStr, []`. No typing constraint is generated.

4. Typing the variables (identifiers).
```
----------------------------------------------
G |- x ==> (x: t), t, []   (if G(x) = t)
```

For example, `gen [("x", TNum); ("y", TNum)] (ID "x") = AID ("x", TNum), TNum, []`. No typing constraint is generated.

If a variable does not appear in its typing environment, raise `UndefinedVar` exception. `gen [] (ID "x")` raises `UndefinedVar` because `x` is undefined in the typing environment.

5. Typing the function definitions.
```
G; x: a |- u ==> e, t, q               (for fresh a, b)
--------------------------------------------------------------------  
G |- (fun x -> u) ==> (fun x -> e : (a -> b)), a -> b, q @ [(t, b)]
```

See the above example on the anonymous function `fun x -> x 1` for how this rule executes. We have `gen [] (Fun ("x", FunctionCall (ID "x", Int 1))) = AFun("x", AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), TFun(T "a", T "b")), TFun(T "a", T "b"), [(T "a", TFun(TNum, T "c")); (T "c", T "b")]`. The typing constraints `[a = (int -> c); c = b]` are explained above.


6. Typing the negation of Boolean expressions.

```
G |- u ==> e, t, q
---------------------------------------------------
G |- not u ==> (not e: bool), bool, q @ [(t, bool)]   
```

For example, `gen [] (Not (Bool true)) = ANot (ABool (true, TBool), TBool), TBool, [(TBool, TBool)]`. The typing constraint requires the annotated type `t` for `u` (in this example `TBool`) same as `TBool`.

7. Typing binary operations.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-----------------------------------------------------------------------------------
G |- u1 ^ u2 ==>  (e1 ^ e2: string), string, q1 @ q2 @ [(t1, string), (t2, string)]
```

We constrain the annotated types for `u1` and `u2` in `u1 ^ u2` as string.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-----------------------------------------------------------------------
G |- u1 + u2 ==>  (e1 + e2: int), int, q1 @ q2 @ [(t1, int), (t2, int)]
```

We constrain the annotated types for `u1` and `u2` in `u1 + u2` as int.

```
G |- u1 ==> e1, t1, q1   G |- u2 ==> e2, t2, q2
-------------------------------------------------------------
G |- u1 < u2 ==>  (e1 < e2: bool), bool, q1 @ q2 @ [(t1, t2)]
```

We constrain the annotated type for `u1` and `u2` in `u1 < u2` to be the same. For comparison operators ("<", ">", "=", "<=", ">="), we assume they are generic meaning that they can take values of arbitrary types, provided that the operands have the same type.

8. Typing if expressions.

```
G |- u1 ==> e1, t1, q1   G |= u2 ==> e2, t2, q2   G |- u3 ==> e3, t3, q3
-------------------------------------------------------------------------------------------------------
G |- (if u1 then u2 else u3) ==> (if e1 then e2 else e3: t2), t2, q1 @ q2 @ q3 @ [(t1, bool); (t2, t3)]
```

We constrain the annotated type for `u1` as bool and the annotated types for `u2` and `u3` the same.

9. Typing function applications.

```
G |- u1 ==> e1, t1, q1
G |- u2 ==> e2, t2, q2                 (for fresh a)
--------------------------------------------------------
G |- u1 u2 ==> (e1 e2: a), a, q1 @ q2 @ [(t1 = t2 -> a)]
```

See the above example on the anonymous function `fun x -> x 1` for how this rule executes. We have `gen [("x", T "a")] (FunctionCall (ID "x", Int 1)) = AFunctionCall(AID("x", T "a"), AInt(1, TNum), T "c"), T "c", [(T "a", TFun(TNum, T "c"))]`. Here as we have no knowledge about the return type of the function `x`, we annotate `x 1` a type scheme `T "c"`.  As `x` is used a function that applies to an integer (`TNum`), we constrain `T "a"` (the type of `x` in the typing environment) as `TFun(TNum, T "c")`. Informally, the constraint is `a = (int -> c)`.

10. Typing let expressions.

```
G |- u1 ==> e1, t1, q1   G; x: t1 |- u2 ==> e2, t2, q2
---------------------------------------------------------------
G |- (let x = u1 in u2) ==> (let x = e1 in e2: t2), t2, q1 @ q2  
```

```
G; f: a |- u1 ==> e1, t1, q1   G; f: t1 |- u2 ==> e2, t2, q2  (for fresh a)
-----------------------------------------------------------------------------------
G |- (let rec f = u1 in u2) ==> (let rec f = e1 in e2: t2), t2, q1 @ [(a, t1)] @ q2  
```

The second rule above is for typing recursive functions e.g. `let rec f = fun x -> if x <= 0 then 1 else x * f(x-1) in f 5`. When type checking for `u1`, it is important to add to the type environment a type for `f`. This is because `f` can be recursively used in `u1`. Since we do not know the type of `f` initially, we annotate it with an unknown type `a` and build constraints over it. When type checking `u2`, it is also necessary to extend the typing environment with `(f: t1)` as `f` may be used in `u2`.

### Typing Constraint Solver

Given the typing constraints returned by `gen`, the unification algorithm (defined in [infer.ml](./infer.ml))
```ocaml
let rec unify (constraints: (typeScheme * typeScheme) list) : substitutions = ...
```
solves a given set of typing constraints obtained from the `gen` method and returns the solution of type `substitutions`.

In [infer.ml](./infer.ml), we define the type `substitutions` as:
```ocaml
type substitutions = (string * typeScheme) list
```

As an example, consider the typing constraints generated for the program `fun x -> x 1` (explained above):
```
a = (int -> c)
c = b
```
or in OCaml code:
```ocaml
[(T "a", TFun(TNum, T "c"));
 (T "c", T "b")]
```
The solution to the typing constraints is
```
[("a", TFun(TNum, T"b")); ("c", T "b")]
```
Intuitively, this solution is a substitution `[int->b/a, b/c]`. Applying this substitution to the typing constraints mentioned above would render the left-hand side and right-hand side of each constraint equivalent. With this solution, we have the type of `fun x -> x 1` as `(int -> b) -> b`.

We have already provided you a working implementation of `unify` in [infer.ml](./infer.ml). Please read the comment associated with `unify` in the code to understand how it executes. The implementation is similar to the pseudocode presented in lecture 13. Reviewing the examples covered in lecture 13 would be beneficial.

However, there are two issues with this implementation that you need to resolve.

First, the solution (`substitutions`) found by the existing `unify` code is suboptimal. For example, consider the program `(fun x -> x 1) (fun x -> x + 1)`. Evaluating this program yields 2. This expression is of type `int`. Applying `gen` to this program should generate the following annotated program:
```ocaml
((fun x -> ((x: a) (1: int)): c): (a -> b) (fun x -> ((x: d) + (1: int): int)): (d -> e)): f
```
where `a`, `b`, `c`, `d`, `e`, `f` and `g` are unknown type variables to be solved, and generate the following typing constraints over the type variables:
```
a = (int -> c)
c = b
d = int
int = int
int = e
(a -> b) = ((d -> e) -> f)
```

Applying `unify` to these constraints would render the following substitutions as the solution.
```
f: int
c: f
d: int
e: int
a: (d -> e)
b: f
```
The solution figures out that `f` should be mapped to `int`. Thus, we are able to conclude that `(fun x -> x 1) (fun x -> x + 1)` (annotated by `f`) is of type `int`. The solution is, however, suboptimal because it does fully infer the types of all the subexpressions. For example, `(fun x -> x 1)` was annotated by `gen` as `a -> b`. The solution does not directly map `a` and `b` to a meaningful concrete type.

Your task is to modify the `unify` implementation so it can derive the following ideal solution:
```
f: int
c: int
d: int
e: int
a: (int -> int)
b: int
```

The second issue in the existing `unify` implementation is that it does not consider occurs check. In lecture 13, we discussed that the program `fun x -> x x` should be rejected by the type checker. Applying `gen` to this program should generate the following annotated program:
```ocaml
(fun x -> ((x: a) (x: a)): c): (a -> b)
```
where `a`, `b` and `c` are unknown type variables to be solved, and generate the following typing constraints over the type variables:
```
a = (a -> c)
c = b
```
The typing constraints are unsolvable because of the constraint `a = (a -> c)`. The type variable `a` occurs on both sides of the constraint.

Your task is to modify the `unify` implementation to conduct occurs check so as to reject a program like `fun x -> x x` by raising the `OccursCheckException` exception in `unify` when a typing constraint like `a = (a -> c)` presents.

### Provided functions

#### `gen_new_type`
- **Type:** `unit -> typeScheme`
- **Description:** Returns T(string) as a new unknown type placeholder. Every call to `gen_new_type` generates a fresh type variable. For example, the program `let a = gen_new_type() in let b = gen_new_type in (a, b)` returns two type variables `T "a"` and `T "b"`. This function is particularly useful for implementing the typing rules. The function is defined in [infer.ml](./infer.ml).

#### `string_of_op`, `string_of_type`, `string_of_aexpr`, `string_of_expr`
- **Description:** Returns a string representation of a given operator `op`, type `typeScheme`, annotated expression `aexpr`, and unannotated expression `expr`. These functions are defined [microCamlTypes.ml](./microCamlTypes.ml) and would be very useful for debugging your implementation.

#### `pp_string_of_type`, `pp_string_of_aexpr`
- **Description:** Pretty printers for returning a string representation of a given type `typeScheme` and annotated expression `aexpr`. These functions outputs types in the OCaml style. For exmple, consider the expression `fun x -> x 1`:
```ocaml
let e = (Fun("x", FunctionCall(ID "x", Int 1))) in
let annotated_expr, t, constraints = gen env e in
let subs = unify constraints in
let annotated_expr = apply_expr subs annotated_expr in
let _ = print_string (pp_string_of_aexpr annotated_expr) in
print_string (pp_string_of_type t
```
The type of `fun x -> x 1` would be printed as the OCaml type `((int -> 'a) -> 'a)`. These functions are defined [microCamlTypes.ml](./microCamlTypes.ml) and would be useful to present the type inference result back to the programmer.

#### `string_of_constraints`, `string_of_subs`
- **Description:** These functions return the string representation of typing constraints and solutions. They can be helpful for debugging. The functions are defiend in [infer.ml](./infer.ml).

#### `infer`
- **Type:**  `expr -> typeScheme`
- **Description:** This function is what we will use to test your code for type inference. Formally, `infer e` invokes the `gen` function to collect the typing constraints from the given expression `e`, solves the typing constraints using the `unify` function, and finally returns the inferred type of `e`. The function is defined in [infer.ml](./infer.ml).

```ocaml
let e = (Fun("x", FunctionCall(ID "x", Int 1))) in
let t = infer e in
pp_string_of_type (t)   (* ((int -> 'a) -> 'a) *)
```
The above program prints out the OCaml type of `fun x -> x 1`.
