
# MicroCaml Lexer and Parser.


## Introduction

Over the course of this project, you will implement the frontend for MicroCaml — a subset of OCaml. The project consists of 2 parts. You will implement a lexer and a parser for MicroCaml. Your lexer function will convert an input string of MicroCaml into a list of tokens, and your parser function will consume these tokens to produce an abstract symbol tree (AST) for a MicroCaml expression.

Here is an example call to the lexer and parser on a MicroCaml expression (as a string):

```ocaml
parse_expr (tokenize "let x = true in x")
```

This will return the AST as the following OCaml value (which we will explain in due course):

```ocaml
 Let ("x", false, (Bool true), ID "x")
```

### Ground Rules

In your code, you may use any OCaml modules and features we have taught in this class, **except imperative OCaml** features like references, mutable records, and arrays.

### Testing & Submitting

Please submit **lexer.ml** and **parser.ml** files to Canvas. To test locally, compile your code with `make` and run `./frontend`.

If you do not have `make`, you can compile the code by `ocamlc -o frontend -I +str str.cma microCamlTypes.ml tokenTypes.ml lexer.mli lexer.ml utils.ml parser.mli parser.ml main.ml`.

All tests will be run on direct calls to your code, comparing your return values to the expected return values. Any other output (e.g., for your own debugging) will be ignored. You are free and encouraged to have additional output. For lexer and parser, the only requirement for error handling is that input that cannot be lexed/parsed according to the provided rules should raise an `InvalidInputException`. We recommend using relevant error messages when raising these exceptions to make debugging easier. We are not requiring intelligent messages that pinpoint an error to help a programmer debug, but as you do this project, you might find you see where you could add those.

You can write your own tests which only test the parser by feeding it a custom token list. For example, to see how the expression `let x = true in x` would be parsed, you can construct the token list manually:

```ocaml
parse_expr [Tok_Let; Tok_ID "x"; Tok_Equal; Tok_Bool true; Tok_In; Tok_ID "x"];;
```

This way, you can work on the parser even if your lexer is not complete yet.

## Part 1: The Lexer (aka Scanner or Tokenizer)

Your parser will take as input a list of tokens; this list is produced by the *lexer* (also called a *scanner*) as a result of processing the input string. Lexing is readily implemented by use of regular expressions, as demonstrated in **lecture 16 slides 3-5**. Information about OCaml's regular expressions library can be found in the [`Str` module documentation](https://ocaml.org/manual/5.3/api/Str.html). You aren't required to use it, but you may find it helpful.

Your lexer must be written in [lexer.ml](./lexer.ml). You will need to implement the following function:

#### `tokenize`

- **Type:** `string -> token list`
- **Description:** Converts MicroCaml syntax (given as a string) to a corresponding token list.
- **Examples:**
  ```ocaml
  tokenize "1 + 2" = [Tok_Int 1; Tok_Add; Tok_Int 2]

  tokenize "1 (-1)" = [Tok_Int 1; Tok_Int (-1)]

  tokenize ";;" = [Tok_DoubleSemi]

  tokenize "+ - let def" = [Tok_Add; Tok_Sub; Tok_Let; Tok_Def]

  tokenize "let rec ex = fun x -> x || true;;" =
    [Tok_Let; Tok_Rec; Tok_ID "ex"; Tok_Equal; Tok_Fun; Tok_ID "x"; Tok_Arrow; Tok_ID "x"; Tok_Or; Tok_Bool true; Tok_DoubleSemi]
  ```

The `token` type is defined in [tokenTypes.ml](./tokenTypes.ml).

Notes:
- The lexer input is case sensitive.
- Tokens can be separated by arbitrary amounts of whitespace, which your lexer should discard. Spaces, tabs ('\t'), and newlines ('\n') are all considered whitespace.
- When excaping characters with `\` within Ocaml strings/regexp, you must use `\\` to escape from the string and regexp.
- If the beginning of a string could match multiple tokens, the **longest** match should be preferred, for example:
  - "let0" should not be lexed as `Tok_Let` followed by `Tok_Int 0`, but as `Tok_ID("let0")`, since it is an identifier.
  - "314dlet" should be tokenized as `[Tok_Int 314; Tok_ID "dlet"]`. Arbitrary amounts of whitespace also include no whitespace.
  - "(-1)" should not be lexed as `[Tok_LParen; Tok_Sub; Tok_Int(1); Tok_LParen]` but as `Tok_Int(-1)`. (This is further explained below)
- There is no "end" token  -- when you reach the end of the input, you are done lexing.

Most tokens only exist in one form (for example, the only way for `Tok_Concat` to appear in the program is as `^` and the only way for `Tok_Let` to appear in the program is as `let`). However, a few tokens have more complex rules. The regular expressions for these more complex rules are provided here:

- `Tok_Bool of bool`: The value will be set to `true` on the input string "true" and `false` on the input string "false".
  - *Regular Expression*: `true|false`
- `Tok_Int of int`: Valid ints may be positive or negative and consist of 1 or more digits. **Negative integers must be surrounded by parentheses** (without extra whitespace) to differentiate from subtraction (examples below). You may find the functions `int_of_string` and `String.sub` useful in lexing this token type.
  - *Regular Expression*: `[0-9]+` OR `(-[0-9]+)`
  - *Examples of int parenthesization*:
    - `tokenize "x -1" = [Tok_ID "x"; Tok_Sub; Tok_Int 1]`
    - `tokenize "x (-1)" = [Tok_ID "x"; Tok_Int (-1)]`
- `Tok_String of string`: Valid string will always be surrounded by `""` and **should accept any character except quotes** within them (as well as nothing). You have to "sanitize" the matched string to remove surrounding escaped quotes.
  - *Regular Expression*: `\"[^\"]*\"`
  - *Examples*:
    - `tokenize "314" = [Tok_Int 314]`
    - `tokenize "\"314\"" = [Tok_String "314"]`
    - `tokenize "\"\"\"" (* InvalidInputException *)`
- `Tok_ID of string`: Valid IDs must start with a letter and can be followed by any number of letters or numbers. **Note: Keywords may be substrings of IDs**.
  - *Regular Expression*: `[a-zA-Z][a-zA-Z0-9]*`
  - *Valid examples*:
    - "a"
    - "ABC"
    - "a1b2c3DEF6"
    - "fun1"
    - "ifthenelse"

MicroCaml syntax with its corresponding token is shown below, excluding the four literal token types specified above.

Token Name | Lexical Representation
--- | ---
`Tok_LParen` | `(`
`Tok_RParen` | `)`
`Tok_Equal` | `=`
`Tok_NotEqual` | `<>`
`Tok_Greater` | `>`
`Tok_Less` | `<`
`Tok_GreaterEqual` | `>=`
`Tok_LessEqual` | `<=`
`Tok_Or` | `\|\|`
`Tok_And` | `&&`
`Tok_Not` | `not`
`Tok_If` | `if`
`Tok_Then` | `then`
`Tok_Else` | `else`
`Tok_Add` | `+`
`Tok_Sub` | `-`
`Tok_Mult` | `*`
`Tok_Div` | `/`
`Tok_Concat` | `^`
`Tok_Let` | `let`
`Tok_Def` | `def`
`Tok_In` | `in`
`Tok_Rec` | `rec`
`Tok_Fun` | `fun`
`Tok_Arrow` | `->`
`Tok_DoubleSemi` | `;;`

Notes:
- Your lexing code will feed the tokens into your parser, so a broken lexer can cause you to fail tests related to parsing.
- In the grammars given below, the syntax matching tokens (lexical representation) are used instead of the token name. For example, the grammars below will use `(` instead of `Tok_LParen`.

## Part 2: Parsing MicroCaml Expressions

In this part, you will implement `parse_expr`, which takes a stream of tokens and outputs an AST for the input expression of type `expr`. Put all of your parser code in [parser.ml](./parser.ml) in accordance with the signature found in [parser.mli](./parser.mli).

We present a quick overview of `parse_expr` first, then the definition of AST types it should return, and finally the grammar it should parse.

### `parse_expr`
- **Type:** `token list -> token list * expr`
- **Description:** Takes a list of tokens and returns an AST representing the MicroCaml expression corresponding to the given tokens, along with any tokens left in the token list.
- **Exceptions:** Raise `InvalidInputException` if the input fails to parse i.e, does not match the MicroCaml expression grammar.
- **Examples** (more below):
  ```ocaml
  parse_expr [Tok_Int(1); Tok_Add; Tok_Int(2)] =  Binop (Add, (Int 1), (Int 2))

  parse_expr [Tok_Int(1)] = (Int 1)

  parse_expr [Tok_Let; Tok_ID("x"); Tok_Equal; Tok_Bool(true); Tok_In; Tok_ID("x")] =
  Let ("x", false, (Bool true), ID "x")

  parse_expr [Tok_DoubleSemi] (* raises InvalidInputException *)
  ```

You may want to implement your parser using the `lookahead` and `match_tok` functions that we have provided; more information about them is given at the end of this README.

### AST and Grammar for `parse_expr`

Below is the AST type `expr`, which is returned by `parse_expr`.

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

The CFG below describes the language of MicroCaml expressions. This CFG is right-recursive, so something like `1 + 2 + 3` will parse as `Add (Int 1, Add (Int 2, Int 3))`, essentially implying parentheses in the form `(1 + (2 + 3))`. In the given CFG note that all non-terminals are capitalized, all syntax literals (terminals) are formatted `as non-italicized code` and will come in to the parser as tokens from your lexer. Variant token types (i.e. `Tok_Bool`, `Tok_Int`, `Tok_String` and `Tok_ID`) will be printed *`as italicized code`*.

- Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr
- LetExpr -> `let` Recursion *`Tok_ID`* `=` Expr `in` Expr
  -	Recursion -> `rec` | ε
- FunctionExpr -> `fun` *`Tok_ID`* `->` Expr
- IfExpr -> `if` Expr `then` Expr `else` Expr
- OrExpr -> AndExpr `||` OrExpr | AndExpr
- AndExpr -> EqualityExpr `&&` AndExpr | EqualityExpr
- EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
  - EqualityOperator -> `=` | `<>`
- RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
  - RelationalOperator -> `<` | `>` | `<=` | `>=`
- AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  - AdditiveOperator -> `+` | `-`
- MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
  - MultiplicativeOperator -> `*` | `/`
- ConcatExpr -> UnaryExpr `^` ConcatExpr | UnaryExpr
- UnaryExpr -> `not` UnaryExpr | FunctionCallExpr
- FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr
- PrimaryExpr -> *`Tok_Int`* | *`Tok_Bool`* | *`Tok_String`* | *`Tok_ID`* | `(` Expr `)`

Notice that this grammar is not actually quite compatible with recursive descent parsing. In particular, the first sets of the productions of many of the non-terminals overlap. For example:

- OrExpr -> AndExpr `||` OrExpr | AndExpr

defines two productions for nonterminal OrExpr, separated by |. Notice that both productions start with AndExpr, so we can't use the lookahead (via FIRST sets) to determine which one to take. This is clear when we rewrite the two productions thus:

- OrExpr -> AndExpr `||` OrExpr
- OrExpr -> AndExpr

When my parser is handling OrExpr, which production should it use? From the above, you cannot tell. The solution is: **You need to refactor the grammar, as shown in lecture 16** slides 35-37.

To illustrate `parse_expr` in action, we show several examples of input and their output AST.

### Example 1: Basic math

**Input:**
```ocaml
(1 + 2 + 3) / 3
```

**Output (after lexing and parsing):**
```ocaml
Binop (Div,
  Binop (Add, (Int 1), Binop (Add, (Int 2), (Int 3))),
  (Int 3))
```

In other words, if we run `parse_expr (tokenize "(1 + 2 + 3) / 3")`, it will return the AST above.

### Example 2: `let` expressions

**Input:**
```ocaml
let x = 2 * 3 / 5 + 4 in x - 5
```

**Output (after lexing and parsing):**
```ocaml
Let ("x", false,
  Binop (Add,
    Binop (Mult, (Int 2), Binop (Div, (Int 3), (Int 5))),
    (Int 4)),
  Binop (Sub, ID "x", (Int 5)))
```

### Example 3: `if then ... else ...`

**Input:**
```ocaml
let x = 3 in if not true then x > 3 else x < 3
```

**Output (after lexing and parsing):**
```ocaml
Let ("x", false, (Int 3),
  If (Not ((Bool true)), Binop (Greater, ID "x", (Int 3)),
   Binop (Less, ID "x", (Int 3))))
```

### Example 4: Anonymous functions

**Input:**
```ocaml
let rec f = fun x -> x ^ 1 in f 1
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", true, Fun ("x", Binop (Concat, ID "x", (Int 1))),
  FunctionCall (ID "f", (Int 1)))
```

Keep in mind that the parser is not responsible for finding type errors. This is the job of type inference. For example, the AST for `1 1` should be parsed as `FunctionCall ((Int 1), (Int 1))` (which will be flagged as a type error by a type checker).

### Example 5: Recursive anonymous functions

Notice how the AST for `let` expressions uses a `bool` flag to determine whether a function is recursive or not. When a recursive anonymous function `let rec f = fun x -> ... in ...` is defined, `f` will bind to `fun x -> ...` when evaluating the function.

**Input:**
```ocaml
let rec f = fun x -> f (x*x) in f 2
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", true,
  Fun ("x", FunctionCall (ID "f", Binop (Mult, ID "x", ID "x"))),
  FunctionCall (ID "f", (Int 2))))
```

### Example 6: Currying

We will use currying for multivariable functions. Here is an example:

**Input:**
```ocaml
let f = fun x -> fun y -> x + y in (f 1) 2
```

**Output (after lexing and parsing):**
```ocaml
Let ("f", false,
  Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y"))),
  FunctionCall (FunctionCall (ID "f", (Int 1)), (Int 2)))
```

### Provided functions

We have provided some helper functions in the `parser.ml` file. You are not required to use these, but they are recommended.

#### `match_token`
- **Type:** `token list -> token -> token list`
- **Description:** Takes the list of tokens and a single token as arguments, and returns a new token list with the first token removed IF the first token matches the second argument.
- **Exceptions:** Raise `InvalidInputException` if the first token does not match the second argument to the function.

#### `match_many`
- **Type:** `token list -> token list -> token list`
- **Description:** An extension of `match_token` that matches a sequence of tokens given as the second token list and returns a new token list with that matches each token in the order in which they appear in the sequence. For example, `match_many toks [Tok_Let]` is equivalent to `match_token toks Tok_Let`.
- **Exceptions:** Raise `InvalidInputException` if the tokens do not match.

#### `lookahead`
- **Type:** `token list -> token option`
- **Description:** Returns the top token in the list of tokens as an option, returning `None` if the token list is empty. **In constructing your parser, the lack of lookahead token (None) is fine for the epsilon case.**

#### `lookahead_many`
- **Type:** `token list -> int -> token option`
- **Description:** An extension of `lookahead` that returns token at the nth index in the list of tokens as an option, returning `None` if the token list is empty at the given index or the index is negative. For example, `lookahead_many toks 0` is equivalent to `lookahead toks`.
