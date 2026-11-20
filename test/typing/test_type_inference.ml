open L2
open Ast
open Type_inference

let fails (e : ast) : bool = Test_helpers.raises_type_error (fun () -> infer e)

let message (e : ast) : string =
  Test_helpers.type_error_message (fun () -> infer e)

let test_operator_type () =
  assert (operator_type Sum = (TInt, TInt));
  assert (operator_type Sub = (TInt, TInt));
  assert (operator_type Mul = (TInt, TInt));
  assert (operator_type Div = (TInt, TInt));
  assert (operator_type Eq = (TInt, TBool));
  assert (operator_type Lt = (TInt, TBool));
  assert (operator_type Gt = (TInt, TBool));
  assert (operator_type And = (TBool, TBool));
  assert (operator_type Or = (TBool, TBool))

let test_constants () =
  assert (infer (Num 1) = TInt);
  assert (infer (Bool true) = TBool);
  assert (infer Unit = TUnit)

let test_variables () =
  assert (infer_expression [ ("x", TInt) ] [] (Id "x") = TInt);
  assert (fails (Id "x"))

let test_locations () =
  assert (infer_with_store_typing [ (0, TInt) ] (Loc 0) = TRef TInt);
  assert (fails (Loc 0))

let test_not () =
  assert (infer (Not (Bool true)) = TBool);
  assert (
    message (Not (Num 1))
    = "the operand of not must have type bool, but has type int")

let test_binop () =
  assert (infer (Binop (Sum, Num 1, Num 2)) = TInt);
  assert (infer (Binop (Lt, Num 1, Num 2)) = TBool);
  assert (infer (Binop (And, Bool true, Bool false)) = TBool);
  assert (
    message (Binop (Sum, Bool true, Num 1))
    = "the left operand of + must have type int, but has type bool");
  assert (
    message (Binop (Sum, Num 1, Bool true))
    = "the right operand of + must have type int, but has type bool");
  assert (fails (Binop (Or, Num 1, Bool true)));
  (* = only compares integers. *)
  assert (fails (Binop (Eq, Bool true, Bool true)))

let test_let () =
  assert (infer (Let ("x", TInt, Num 1, Binop (Sum, Id "x", Num 1))) = TInt);
  assert (
    message (Let ("x", TInt, Bool true, Id "x"))
    = "the value bound to x must have type int, but has type bool");
  (* The inner x shadows the outer one. *)
  assert (
    infer (Let ("x", TInt, Num 1, Let ("x", TBool, Bool true, Id "x"))) = TBool);
  (* x only exists inside the body of its let. *)
  assert (fails (Binop (Sum, Let ("x", TInt, Num 1, Id "x"), Id "x")))

let test_if () =
  assert (infer (If (Bool true, Num 1, Num 2)) = TInt);
  assert (
    message (If (Num 1, Num 1, Num 2))
    = "the condition of if must have type bool, but has type int");
  assert (
    message (If (Bool true, Num 1, Bool false))
    = "the else branch of if must have type int, but has type bool")

let test_references () =
  assert (infer (New (Num 1)) = TRef TInt);
  assert (infer (New (New (Bool true))) = TRef (TRef TBool));
  assert (infer (Deref (New (Num 1))) = TInt);
  assert (
    message (Deref (Num 1))
    = "the operand of ! must be a reference, but has type int")

let test_assignment () =
  let context = [ ("x", TRef TInt); ("n", TInt) ] in
  assert (infer_expression context [] (Asg ("x", Num 2)) = TUnit);
  assert (
    Test_helpers.type_error_message (fun () ->
        infer_expression context [] (Asg ("x", Bool true)))
    = "the value assigned to x must have type int, but has type bool");
  assert (
    Test_helpers.type_error_message (fun () ->
        infer_expression context [] (Asg ("n", Num 2)))
    = "the variable n must be a reference, but has type int");
  assert (fails (Asg ("y", Num 2)))

let test_sequence () =
  assert (infer (Seq (Unit, Num 1)) = TInt);
  assert (
    message (Seq (Num 1, Num 2))
    = "the first expression of a sequence must have type unit, but has type int")

let test_while () =
  assert (infer (Whi (Bool false, Unit)) = TUnit);
  assert (
    message (Whi (Num 1, Unit))
    = "the condition of while must have type bool, but has type int");
  assert (
    message (Whi (Bool false, Num 1))
    = "the body of while must have type unit, but has type int")

let test_program () =
  (* let x : ref int = new 0 in while !x < 3 do x := !x + 1; !x *)
  let program =
    Let
      ( "x",
        TRef TInt,
        New (Num 0),
        Seq
          ( Whi
              ( Binop (Lt, Deref (Id "x"), Num 3),
                Asg ("x", Binop (Sum, Deref (Id "x"), Num 1)) ),
            Deref (Id "x") ) )
  in
  assert (infer program = TInt)

let run () =
  test_operator_type ();
  test_constants ();
  test_variables ();
  test_locations ();
  test_not ();
  test_binop ();
  test_let ();
  test_if ();
  test_references ();
  test_assignment ();
  test_sequence ();
  test_while ();
  test_program ()
