open Ast
open Eval

let _ = Printexc.record_backtrace(true)


(************ DO NOT CHANGE ANY CODE HERE ***********)
(**** Your code should be implemented in eval.ml ****)
(****************************************************)

(* Parse a file of Imp source code *)
let load (filename : string) : Ast.com =
  let ch =
    try open_in filename
    with Sys_error s -> failwith ("Cannot open file: " ^ s) in
  let parse : com =
    try Parser.main Lexer.token (Lexing.from_channel ch)
    with e ->
      let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
      Printf.eprintf "there was an error: %s%s\n" msg stack;
      close_in ch; failwith "Cannot parse program" in
  close_in ch;
  parse

(* Interpret a parsed AST with the eval_command function defined in eval.ml *)
let eval (parsed_ast : Ast.com) : environment =
  let env = [] in
  eval_command parsed_ast env


(*********************)
(* Testing your code *)
(*********************)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (***************************)
  (**** 1. Test eval_expr ****)
  (***************************)

  let env1 = [("x", Int_Val 4); ("y", Bool_Val false)] in

  let _ =
    try
      (* eval_expr (x + 4) = 8 when x = 4 in the environment *)
      assert (Int_Val 8 = eval_expr (Plus ((Var "x"), (Number 4))) env1);
      (* eval_expr (50 - 8) = 42 *)
      assert (Int_Val 42 = eval_expr (Minus (Number 50, Number 8)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (5 * -2) = -10  *)
      assert (Int_Val (-10) = eval_expr (Times (Number 5, Number (-2))) []);
      (* eval_expr (70 / 7) = 10 *)
      assert (Int_Val 10 = eval_expr (Div (Number 70, Number 7)) []);
      (* eval_expr (3 % 2) = 1 *)
      assert (Int_Val 1 = eval_expr (Mod (Number 3, Number 2)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (false || y) = false when y = false in the environment *)
      assert (Bool_Val false = eval_expr (Or (False, Var "y")) env1);
      (* eval_expr (false && true) = false *)
      assert (Bool_Val false = eval_expr (And (False, True)) []);
      (* eval_expr (not true) = false *)
      assert (Bool_Val false = eval_expr (Not (True)) env1)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x = 10) = false when x = 4 in the environment *)
      assert (Bool_Val false = eval_expr (Eq (Var "x", Number 10)) env1);
      (* eval_expr (x < 10) = true when x = 4 in the environment *)
      assert (Bool_Val true = eval_expr (Lt (Var "x", Number 10)) env1);
      (* eval_expr (1 <= 0) = false *)
      assert (Bool_Val false = eval_expr (Leq (Number 1, Number 0)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr ((fun x -> x + 3) 5) = 8 *)
      let e1 = Fun ("x", Plus (Var "x", Number 3)) in
      let e2 = Number 5 in
      assert (Int_Val 8 = eval_expr (App (e1, e2)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (fun z -> z - 2) ((fun x -> x + 1) 2) = 1 *)
      let e1 = Fun ("x", Plus (Var "x", Number 1)) in
      let e2 = Number 2 in
      let e3 = App (e1, e2) in
      let e4 = Fun ("z", Minus (Var "z", Number 2)) in
      let e5 = App (e4, e3) in
      assert (Int_Val 1 = eval_expr e5 [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (fun x -> fun y -> x) 2 3 =  2 *)
      let e1 = Fun ("x", Fun ("y", Var "x")) in
      let e2 = App (App (e1, Number 2), Number 3) in
      assert (Int_Val 2 = eval_expr e2 [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  (****************************)
  (*** 2. Test eval_command ***)
  (****************************)

  let _ =
    try
      let parsed_ast = load ("programs/aexp-add.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- x => 10\n\
         - y => 15\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/aexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- w => -13\n\
         - x => 1\n\
         - y => 2\n\
         - z => 3\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/bexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- res1 => 1\n\
         - res10 => 0\n\
         - res11 => 0\n\
         - res12 => 0\n\
         - res13 => 1\n\
         - res14 => 1\n\
         - res15 => 1\n\
         - res16 => 0\n\
         - res2 => 0\n\
         - res3 => 1\n\
         - res4 => 0\n\
         - res5 => 0\n\
         - res6 => 1\n\
         - res7 => 0\n\
         - res8 => 0\n\
         - res9 => 1\n\
         - w => 5\n\
         - x => 3\n\
         - y => 5\n\
         - z => -3\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/cond.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n1 => 255\n\
         - n2 => -5\n\
         - res1 => 1\n\
         - res2 => 255\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/fact.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f => 120\n\
         - n => 1\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/fib.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f0 => 5\n\
         - f1 => 8\n\
         - k => 6\n\
         - n => 5\n\
         - res => 8\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/for.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- i => 101\n\
         - n => 101\n\
         - sum => 5151\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/palindrome.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 135\n\
         - res => 1\n\
         - res2 => 0\n\
         - reverse => 123454321\n\
         - reverse2 => 531\n\
         - temp => 0\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/while.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 0\n\
         - sum => 5050\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/lambda.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert (
      "- a => 0\n\
       - b => 5\n\
       - x => 10\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> x + 3;
          g := f;
          result := g 5;)
        =
          "- result => 8\n"
      *)
      let e1 = Fun ("x", Plus (Var "x", Number 3)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", Var "f") in
      let p5 = Assg ("result", App (Var "g", Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 8\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          f := (fun x -> fun y -> fun w -> w * (x + y)) 3 4;
          result := f 5;)
        =
          "- result => 35\n"
      *)
      let e1 = Fun ("x", Fun ("y", Fun ("w", Times (Var "w", Plus (Var "x", Var "y"))))) in
      let e2 = App (App (e1, Number 3), Number 4) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Assg ("f", e2) in
      let p3 = Assg ("result", App (Var "f", Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, p3))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 35\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> fun y -> x - y;
          g := f 2;
          int x;
          x := 3;
          result := g 4;
          )
        =
          "- result => -2\n\
          - x => 3\n"
      *)
      let e1 = Fun ("x", Fun ("y", Minus (Var "x", Var "y"))) in
      let e2 = App (Var "f", Number 2) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", e2) in
      let p5 = Declare (Int_Type, "x") in
      let p6 = Assg ("x", Number 3) in
      let p7 = Assg ("result", App (Var "g", Number 4)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, Comp (p5, Comp (p6, p7))))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => -2\n\
       - x => 3\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun f -> fun x -> f (f x);
          g := fun x -> x + 3;
          result := f g 5;)
        =
          "- result => 11\n"
      *)
      let e1 = Fun ("f", Fun ("x", App (Var "f", App (Var "f", Var "x")))) in
      let e2 = Fun ("x", Plus (Var "x", Number 3)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", e2) in
      let p5 = Assg ("result", App (App (Var "f", Var "g"), Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 11\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  (******************************)
  (*** 3. Test Error Handling ***)
  (******************************)

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> x + 1;
          g := f;
          result := g (fun i -> i + 1) 1;
          )
        =
          TypeError
      *)
      let e1 = Fun ("x", Plus (Var "x", Number 1)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", Var "f") in
      let p5 = Assg ("result", App (App (Var "g", Fun("i", Plus (Var "i", Number 1))), Number 1)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      try
        ignore (eval_command p []);
        assert false
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in


  let env1 : environment = [("x", Int_Val(1)); ("p", Bool_Val(false))] in
  let env2 : environment = [("x", Int_Val(1)); ("p", Bool_Val(false)); ("y", Int_Val(6)); ("q", Bool_Val(true))] in

  let _ =
    try
      (* eval_expr (1 + p) =  TypeError when p = false in the environment *)
      try
        ignore(eval_expr (Plus (Number 1, Var "p")) env1);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (q && True < y) = TypeError *)
      try
        ignore(eval_expr (And (Var "q", Lt (True, Var "y"))) env2);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x / 0) = DivByZeroError *)
      try
        ignore(eval_expr (Div (Var "x", Number 0)) env2);
        assert false (* this line shouldn't be reached! *)
      with
        | DivByZeroError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x % y) = UndefinedVar in the environment without any definition of y *)
      try
        ignore(eval_expr (Mod (Var "x", Var "y")) env1);
        assert false (* this line shouldn't be reached! *)
      with
        | UndefinedVar -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in


  let _ =
    try
      (*
        eval
          (int f;
          n := 5;
          f := 1;
          while (f <= 1) {
            f = f * 5;
            n = n - 1;
          })
        =
          UndefinedVar
      *)
      let p =
          Comp(Declare (Int_Type, "f"),
              Comp (Assg ("n", Number 5),
              Comp (Assg ("f", Number 1),
                  While (Leq (Var "f", Number 1),
                      Comp(Assg ("f", Times (Var "f", Number 5)),
                           Assg ("n", Minus (Var "n", Number 1))))))) in
      try
        ignore (eval_command p []);
        assert false (* this line shouldn't be reached! *)
      with
        | UndefinedVar -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int f;
          f := 1;
          while (f) {
            f = f * 5;
          })
        =
          TypeError
      *)
      let p =
          Comp(Declare (Int_Type, "f"),
              Comp (Assg ("f", Number 1),
                  While (Var "f",
                      Assg ("f", Times (Var "f", Number 5))))) in
      try
        ignore (eval_command p []);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 28 programming questions are incorrect.\n") (!error_count)

let _ = main()
