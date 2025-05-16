open TokenTypes
open String

(*type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi*)


(* We provide the regular expressions that may be useful to your code *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equal = Str.regexp "=";;
let re_not_equal = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greater_equal = Str.regexp ">=";;
let re_less_equal = Str.regexp "<=";;
let re_or = Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^";;
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_true = Str.regexp "true";;
let re_false = Str.regexp "false";;
let re_string = Str.regexp "\"[^\"]*\"";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_double_semi = Str.regexp ";;";;
let re_whitespace = Str.regexp "[ \t\n]+";;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input =
  let input = String.trim input in
  if input = "" then []
  else if Str.string_match re_whitespace input 0 then
    tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_double_semi input 0 then
    Tok_DoubleSemi :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_not_equal input 0 then
    Tok_NotEqual :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_greater_equal input 0 then
    Tok_GreaterEqual :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_less_equal input 0 then
    Tok_LessEqual :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_or input 0 then
    Tok_Or :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_and input 0 then
    Tok_And :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_arrow input 0 then
    Tok_Arrow :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_lparen input 0 then
    Tok_LParen :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_rparen input 0 then
    Tok_RParen :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_equal input 0 then
    Tok_Equal :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_greater input 0 then
    Tok_Greater :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_less input 0 then
    Tok_Less :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_not input 0 then
    Tok_Not :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_if input 0 then
    Tok_If :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_then input 0 then
    Tok_Then :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_else input 0 then
    Tok_Else :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_add input 0 then
    Tok_Add :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_sub input 0 then
    Tok_Sub :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_mult input 0 then
    Tok_Mult :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_div input 0 then
    Tok_Div :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_concat input 0 then
    Tok_Concat :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_let input 0 then
    Tok_Let :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_rec input 0 then
    Tok_Rec :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_in input 0 then
    Tok_In :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_def input 0 then
    Tok_Def :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_fun input 0 then
    Tok_Fun :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_true input 0 then
    Tok_Bool true :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_false input 0 then
    Tok_Bool false :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_neg_int input 0 then
    let matched = Str.matched_string input in
    let value = int_of_string (String.sub matched 1 (String.length matched - 2)) in
    Tok_Int value :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_pos_int input 0 then
    let matched = Str.matched_string input in
    let value = int_of_string matched in
    Tok_Int value :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_string input 0 then
    let matched = Str.matched_string input in
    let value = String.sub matched 1 (String.length matched - 2) in
    Tok_String value :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else if Str.string_match re_id input 0 then
    let matched = Str.matched_string input in
    Tok_ID matched :: tokenize (String.sub input (Str.match_end ()) (String.length input - Str.match_end ()))
  else
    raise (InvalidInputException ("Unrecognized token: " ^ input))
