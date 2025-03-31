type token =
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | EQ
  | LEQ
  | LT
  | GEQ
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | ASSG
  | COMP
  | SKIP
  | IF
  | ELSE
  | WHILE
  | FOR
  | PRINT
  | INPUT
  | VAR of (string)
  | NUMBER of (int)
  | EOL
  | INT
  | BOOL
  | LAMBDA
  | FUN
  | ARROW

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.com
