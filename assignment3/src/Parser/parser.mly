%{
open Ast
%}

%token TRUE FALSE AND OR NOT
%token PLUS MINUS TIMES DIV MOD
%token EQ LEQ LT GEQ GT
%token LPAREN RPAREN LBRACE RBRACE
%token ASSG COMP SKIP IF ELSE WHILE FOR PRINT INPUT
%token <string> VAR
%token <int> NUMBER
%token EOL
%token INT BOOL LAMBDA
%token FUN ARROW

%nonassoc RPAREN RBRACE /* lowest precedence - always reduce */
%nonassoc EQ LEQ LT GEQ GT
%left VAR
%nonassoc NUMBER
%nonassoc INT BOOL LAMBDA
%left ARROW
%left PLUS MINUS MOD
%left TIMES DIV
%nonassoc TRUE FALSE
%left AND OR
%nonassoc NOT
%nonassoc COMP
%nonassoc SKIP IF ELSE WHILE FOR PRINT INPUT
%nonassoc ASSG
%nonassoc LPAREN LBRACE /* highest precedence - always shift */
%nonassoc FUN

%start main /* entry points */
%type <Ast.com> main

%%

main:
  | comlist EOL { $1 }
;

comlist:
  | com COMP comlist { Comp ($1, $3) }
  | combr comlist { Comp ($1, $2) } /* ends in brace -> doesn't need ; */
  | com COMP { $1 }
  | com { $1 }
  | combr COMP comlist { Comp ($1, $3) }
  | combr COMP { $1 }
;

com:
  | WHILE LPAREN exp RPAREN com { While ($3, $5) }
  | FOR   LPAREN exp RPAREN com { For   ($3, $5) }
  | IF LPAREN exp RPAREN com ELSE com { Cond ($3, $5, $7) }
  | IF LPAREN exp RPAREN combr ELSE com { Cond ($3, $5, $7) }
  | VAR ASSG exp { Assg ($1, $3) }
  | SKIP { Skip }
  | INT VAR { Declare (Int_Type, $2) }
  | BOOL VAR { Declare (Bool_Type, $2) }
  | LAMBDA VAR { Declare (Lambda_Type, $2) }
;

combr:
  | LBRACE comlist RBRACE { $2 }
  | WHILE LPAREN exp RPAREN combr { While ($3, $5) }
  | FOR   LPAREN exp RPAREN combr { For   ($3, $5) }
  | IF LPAREN exp RPAREN com ELSE combr { Cond ($3, $5, $7) }
  | IF LPAREN exp RPAREN combr ELSE combr { Cond ($3, $5, $7) }
;

exp:
  | VAR { Var $1 }
  | NUMBER { Number $1 }
  | LPAREN exp RPAREN { $2 }
  | exp PLUS exp { Plus ($1, $3) }
  | exp MINUS exp { Minus ($1, $3) }
  | exp TIMES exp { Times ($1, $3) }
  | exp DIV exp { Div ($1, $3) }
  | exp MOD exp { Mod ($1, $3) }
  | MINUS exp { Minus (Number 0, $2) }
  | exp EQ exp { Eq ($1, $3) }
  | exp LEQ exp { Leq ($1, $3) }
  | exp LT exp { Lt ($1, $3) }
  | exp GEQ exp { Leq ($3, $1) }
  | exp GT exp { Lt ($3, $1) }
  | LPAREN exp RPAREN { $2 }
  | NOT exp { Not $2 }
  | exp AND exp { And ($1, $3) }
  | exp OR exp { Or ($1, $3) }
  | TRUE { True }
  | FALSE { False }
  | exp exp8 { App ($1, $2) }
  | FUN VAR ARROW exp  { Fun ($2, $4) }
;

exp8:
      NUMBER { Number $1 }
    | VAR { Var $1 }
    | TRUE { True }
    | FALSE { False }
    | LPAREN exp RPAREN { $2 }
;
%%
