open L2
open Ast

let () =
  Test_type_printer.run ();
  Test_typing_env.run ();
  Test_type_errors.run ();
  Test_type_inference.run ();
  let open Interpreter in
  assert (interpret (Binop (Sum, Num 5, Num 3)) = "8 : int");
  assert (interpret (New (Num 10)) = "<location 0> : ref int");
  assert (interpret (Deref (New (Num 5))) = "5 : int");
  let assignment =
    Let ("x", TRef TInt, New (Num 5), Seq (Asg ("x", Num 10), Deref (Id "x")))
  in
  assert (interpret assignment = "10 : int");
  let loop =
    Let
      ( "x",
        TRef TInt,
        New (Num 0),
        Whi
          ( Binop (Lt, Deref (Id "x"), Num 3),
            Asg ("x", Binop (Sum, Deref (Id "x"), Num 1)) ) )
  in
  assert (Big_step.evaluate loop = (Value.VUnit, [ (0, Value.VNum 3) ]));
  assert (
    Type_inference.infer_with_store_typing [ (42, TBool) ] (Loc 42) = TRef TBool);
  assert (
    Big_step.evaluate_with_store [ (42, Value.VBool true) ] (Loc 42)
    = (Value.VLoc 42, [ (42, Value.VBool true) ]));
  assert (
    Test_helpers.raises_type_error (fun () ->
        Type_inference.infer (Binop (Sum, Num 1, Bool true))));
  assert (
    interpret (Binop (Div, Num 6, Num 0)) = "Runtime error: division by zero")
