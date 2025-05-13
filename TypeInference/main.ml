open Infer
open MicroCamlTypes

let _ = Printexc.record_backtrace(true)

(* Simple Expression Tests: *)

let public_expr_simple_equal_type _ =
  (* 1 = 1  *)
  let prog = (Binop (Equal, (Int 1), (Int 1))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_concat_type _ =
  (* "\"Hello\" ^ \" World!\"" *)
  let prog = (Binop (Concat, (String "Hello"), (String " World!"))) in
  let result = TStr in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_div_type _ =
  (* "15 / 3" *)
  let prog =  (Binop (Div, (Int 15), (Int 3))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_mult_type _ =
  (* "5 * 3" *)
  let prog = (Binop (Mult, (Int 5), (Int 3))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_sub_type _ =
  (* "3 - 2" *)
  let prog = (Binop (Sub, (Int 3), (Int 2))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_simple_sum_type _ =
  (* "1 + 2" *)
  let prog = (Binop (Add, (Int 1), (Int 2))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_and_type _ =
  (* "true && true" *)
  let prog = (Binop (And, (Bool true), (Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_bool_type _ =
  (* "false" *)
  let prog = ((Bool false)) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_fun_type _ =
  (* fun x -> x + 1 *)
  let prog = (Fun ("x", Binop (Add, ID "x", (Int 1)))) in
  let result = TFun(TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_single_if_type _ =
  (* if 1 = 2 then false else true *)
  let prog = (If (Binop (Equal, (Int 1), (Int 2)), (Bool false),
   (Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_let_type _ =
  (* let x = 42 in x *)
  let prog = (Let ("x", false, (Int 42), ID "x")) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_notequal_type _ =
  (* "1 <> 2" *)
  let prog = (Binop (NotEqual, (Int 1), (Int 2))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_not_type _ =
  (* "not true" *)
  let prog = (Not ((Bool true))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_number_type _ =
  (* "42" *)
  let prog = ((Int 42)) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_single_or_type _ =
  (* "true || false" *)
  let prog = (Binop (Or, (Bool true), (Bool false))) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_single_string_type _ =
  (* "\"Hello World!\""  *)
  let prog = ((String "Hello World!")) in
  let result = TStr in
  let student = infer prog in
  assert (student = result)

(* Complex Expressions *)

let public_expr_add1_type _ =
  (* let add1 = fun x -> x + 1 in add1 *)
  let prog = (Let ("add1", false, Fun ("x", Binop (Add, ID "x", (Int 1))), ID "add1")) in
  let result = TFun (TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_apply_type _ =
  (* let apply = fun x -> fun y -> x y in let add1 = fun z -> z + 1 in (apply add1) 5 *)
  let prog = (Let ("apply", false, Fun ("x", Fun ("y", FunctionCall (ID "x", ID "y"))),
   Let ("add1", false, Fun ("z", Binop (Add, ID "z", (Int 1))),
    FunctionCall (FunctionCall (ID "apply", ID "add1"), (Int 5))))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_double_fun_type _ =
  (* fun x -> fun y -> x + y *)
  let prog = (Fun ("x", Fun ("y", Binop (Add, ID "x", ID "y")))) in
  let result = TFun(TNum, TFun(TNum, TNum)) in
  let student = infer prog in
  assert (student = result)

let public_expr_let_if_type _ =
  (* let sanity = if 1 = 1 then true else false in sanity *)
  let prog = (Let ("sanity", false,
   If (Binop (Equal, (Int 1), (Int 1)), (Bool true),
    (Bool false)),
   ID "sanity")) in
  let result = TBool in
  let student = infer prog in
  assert (student = result)

let public_expr_let_fun_type _ =
  (* let abc = fun a -> a + 1 in abc 1 *)
  let prog = (Let ("abc", false, Fun ("a", Binop (Add, ID "a", (Int 1))),
    FunctionCall (ID "abc", (Int 1)))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_minus_one_type _ =
  (* let x = 1-1 in x *)
  let prog = (Let ("x", false, Binop (Sub, (Int 1), (Int 1)), ID "x")) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_nested_let_type _ =
  (* let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in let f = 6 in let g = 7 in let h = 8 in let i = 9 in let j = 10 in a+b+c+d+e+f+g+h *)
  let prog = (
  Let ("a", false, (Int 1),
   Let ("b", false, (Int 2), Let ("c", false, (Int 3),
     Let ("d", false, (Int 4),
      Let ("e", false, (Int 5),
       Let ("f", false, (Int 6),
        Let ("g", false, (Int 7),
         Let ("h", false, (Int 8),
          Let ("i", false, (Int 9),
           Let ("j", false, (Int 10),
            Binop (Add, ID "a",
             Binop (Add, ID "b",
              Binop (Add, ID "c",
               Binop (Add, ID "d",
                Binop (Add, ID "e",
                 Binop (Add, ID "f", Binop (Add, ID "g", ID "h")))))))))))))))))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_sub1_type _ =
  (* let sub1 = fun x -> x - 1 in sub1 *)
  let prog = (Let ("sub1", false, Fun ("x", Binop (Sub, ID "x", (Int 1))), ID "sub1")) in
  let result = TFun(TNum, TNum) in
  let student = infer prog in
  assert (student = result)

let public_expr_fact_type _ =
  let prog = (Let ("f", true,
                  Fun("x", If (Binop(NotEqual, ID "x", Int 0), Int 1, Binop(Mult, ID "x", FunctionCall(ID "f", Binop(Sub, ID "x", Int 1))))),
                  FunctionCall(ID "f", Int 5))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

(* Higher-order functions *)

let public_expr_ho_type _ =
  (* fun x -> x 1 *)
  let prog = (Fun("x", FunctionCall(ID "x", Int 1))) in
  let a = gen_new_type() in
  (* (int -> 'a) -> 'a *)
  let result = pp_string_of_type (TFun(TFun(TNum, a), a)) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_ho2_type _ =
  (* fun a -> (fun b -> a(a b)) *)
  let prog = (Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b"))))) in
  let a = gen_new_type() in
  (* ('a -> 'a) -> 'a -> 'a *)
  let result = pp_string_of_type (TFun(TFun(a, a), TFun(a, a))) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_ho3_type _ =
  (* fun c -> (fun d -> (fun e -> e c d)) *)
  let prog = (Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d"))))) in
  let a = gen_new_type() in
  let b = gen_new_type() in
  let c = gen_new_type() in
  (* 'a -> 'b -> ('a -> 'b -> 'c) -> 'c *)
  let result = pp_string_of_type (TFun(a, TFun(b, TFun(TFun(a, TFun(b, c)), c)))) in
  let student = pp_string_of_type (infer prog) in
  assert (student = result)

let public_expr_hoapp_type _ =
  (* (fun x -> x 1) (fun x -> x + 1) *)
  let prog = (FunctionCall(Fun("x", FunctionCall(ID "x", Int 1)), Fun("x", Binop(Add, ID "x", Int 1)))) in
  let result = TNum in
  let student = infer prog in
  assert (student = result)

let public_expr_hoapp2_type _ =
  (* (fun a -> (fun b -> a(a b))) (fun c -> (fun d -> (fun e -> e c d))) *)
  let prog = FunctionCall (Fun("a", Fun("b", FunctionCall(ID "a", FunctionCall(ID "a", ID "b")))), Fun("c", Fun("d", Fun ("e", FunctionCall(FunctionCall(ID "e", ID "c"), ID "d"))))) in
  try
    let _ = infer prog in
    assert false
  with OccursCheckException -> (type_variable := (Char.code 'a')) | _ -> (type_variable := (Char.code 'a'); assert (1 = 0))

let public_expr_xx_type _ =
  let prog = (Fun("x", FunctionCall(ID "x", ID "x"))) in
  try
    let _ = infer prog in
    assert false
  with OccursCheckException -> (type_variable := (Char.code 'a')) | _ -> (type_variable := (Char.code 'a'); assert (1 = 0))

let public_constraint_solving _ =
  (*a = (int -> c)
  c = b
  d = int
  int = int
  int = e
  (a -> b) = ((d -> e) -> f)*)
  let constraints = [
    (T "a", TFun(TNum, T "c"));
    (T "c", T "b");
    (T "d", TNum);
    (TNum, TNum);
    (TNum, T "e");
    (TFun(T "a", T "b"), TFun(TFun(T "d", T "e"), T "f"))
  ] in
  let student =  unify constraints in
  let result = [("f", TNum); ("c", TNum); ("d", TNum); ("e", TNum); ("a", TFun(TNum, TNum)); ("b", TNum)] in
  let f x y = if x < y then -1 else if x = y then 0 else 1 in
  assert (List.sort f student = List.sort f result)


(*********************)
(* Testing your code *)
(*********************)

let _ = print_string ("Testing your code ...\n")

let error_count = ref 0

let main () =

  (*********************************)
  (* Test cases for type inference *)
  (*********************************)

  let _ = try public_expr_simple_equal_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_concat_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_div_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ =  try public_expr_simple_mult_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sub_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_simple_sum_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_and_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_bool_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_if_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_let_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_notequal_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_not_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_number_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_or_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_single_string_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_add1_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_apply_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_double_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_if_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_let_fun_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_minus_one_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_nested_let_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_sub1_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_fact_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_expr_ho_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho2_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_ho3_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_hoapp2_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in
  let _ = try public_expr_xx_type()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  let _ = try public_constraint_solving()
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s\n" msg stack) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 32 programming questions are incorrect.\n") (!error_count)

let _ = main()
