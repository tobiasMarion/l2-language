open L2
open Ast

let test_interpreter () =
  let open Interpreter in
  assert (interpret (Binop (Sum, Num 5, Num 3)) = "8 : int");
  assert (interpret (New (Num 10)) = "<location 0> : ref int");
  assert (interpret (Deref (New (Num 5))) = "5 : int");
  let assignment =
    Let ("x", TRef TInt, New (Num 5), Seq (Asg ("x", Num 10), Deref (Id "x")))
  in
  assert (interpret assignment = "10 : int");
  assert (
    interpret (Binop (Sum, Num 1, Bool true))
    = "Type error: the right operand of + must have type int, but has type bool");
  assert (
    interpret (Binop (Div, Num 6, Num 0)) = "Runtime error: division by zero")

let () =
  Test_type_printer.run ();
  Test_typing_env.run ();
  Test_type_errors.run ();
  Test_type_inference.run ();
  Test_value_printer.run ();
  Test_runtime_errors.run ();
  Test_evaluation_env.run ();
  Test_big_step.run ();
  test_interpreter ()
