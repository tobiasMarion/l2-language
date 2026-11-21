open L2
open Ast
open Value
open Big_step

let value_of (e : ast) : value = fst (evaluate e)

let message (e : ast) : string =
  Test_helpers.runtime_error_message (fun () -> evaluate e)

let test_apply_operator () =
  assert (apply_operator Sum (VNum 2) (VNum 3) = VNum 5);
  assert (apply_operator Sub (VNum 2) (VNum 3) = VNum (-1));
  assert (apply_operator Mul (VNum 2) (VNum 3) = VNum 6);
  assert (apply_operator Div (VNum 7) (VNum 2) = VNum 3);
  assert (apply_operator Eq (VNum 2) (VNum 2) = VBool true);
  assert (apply_operator Lt (VNum 2) (VNum 3) = VBool true);
  assert (apply_operator Gt (VNum 2) (VNum 3) = VBool false);
  assert (apply_operator And (VBool true) (VBool false) = VBool false);
  assert (apply_operator Or (VBool true) (VBool false) = VBool true);
  assert (
    Test_helpers.runtime_error_message (fun () ->
        apply_operator Div (VNum 1) (VNum 0))
    = "division by zero");
  assert (
    Test_helpers.runtime_error_message (fun () ->
        apply_operator Sum (VNum 1) (VBool true))
    = "incompatible operands")

let test_constants () =
  assert (evaluate (Num 1) = (VNum 1, []));
  assert (evaluate (Bool true) = (VBool true, []));
  assert (evaluate Unit = (VUnit, []))

let test_variables () =
  assert (evaluate_expression [ ("x", VNum 1) ] [] (Id "x") = (VNum 1, []));
  assert (message (Id "x") = "undeclared variable: x")

let test_locations () =
  assert (
    evaluate_with_store [ (42, VBool true) ] (Loc 42)
    = (VLoc 42, [ (42, VBool true) ]));
  assert (message (Loc 0) = "unknown location 0")

let test_not () =
  assert (value_of (Not (Bool true)) = VBool false);
  assert (message (Not (Num 1)) = "the operand of not must be a bool")

let test_binop () =
  assert (value_of (Binop (Sum, Num 5, Num 3)) = VNum 8);
  (* The left operand runs first: its store reaches the right operand. *)
  assert (
    evaluate (Binop (Eq, Deref (New (Num 1)), Deref (New (Num 2))))
    = (VBool false, [ (1, VNum 2); (0, VNum 1) ]))

let test_if () =
  assert (value_of (If (Bool true, Num 1, Num 2)) = VNum 1);
  assert (value_of (If (Bool false, Num 1, Num 2)) = VNum 2);
  (* Only the chosen branch runs. *)
  assert (value_of (If (Bool true, Num 1, Binop (Div, Num 1, Num 0))) = VNum 1);
  assert (
    message (If (Num 1, Num 1, Num 2)) = "the condition of if must be a bool")

let test_let () =
  assert (value_of (Let ("x", TInt, Num 1, Binop (Sum, Id "x", Num 1))) = VNum 2);
  (* The inner x shadows the outer one. *)
  assert (
    value_of (Let ("x", TInt, Num 1, Let ("x", TBool, Bool true, Id "x")))
    = VBool true);
  (* x only exists inside the body of its let. *)
  assert (
    message (Binop (Sum, Let ("x", TInt, Num 1, Id "x"), Id "x"))
    = "undeclared variable: x")

let test_references () =
  assert (evaluate (New (Num 10)) = (VLoc 0, [ (0, VNum 10) ]));
  (* Nested new: the inner location is created first. *)
  assert (
    evaluate (New (New (Bool true))) = (VLoc 1, [ (1, VLoc 0); (0, VBool true) ]));
  assert (value_of (Deref (New (Num 5))) = VNum 5);
  assert (message (Deref (Num 1)) = "the operand of ! must be a reference")

let test_assignment () =
  (* let x : ref int = new 5 in x := 10; !x *)
  let program =
    Let ("x", TRef TInt, New (Num 5), Seq (Asg ("x", Num 10), Deref (Id "x")))
  in
  assert (evaluate program = (VNum 10, [ (0, VNum 10) ]));
  assert (
    evaluate_expression [ ("x", VLoc 0) ] [ (0, VNum 5) ] (Asg ("x", Num 7))
    = (VUnit, [ (0, VNum 7) ]));
  assert (
    message (Let ("n", TInt, Num 1, Asg ("n", Num 2)))
    = "the variable n must be a reference");
  assert (message (Asg ("y", Num 2)) = "undeclared variable: y")

let test_sequence () =
  assert (value_of (Seq (Unit, Num 1)) = VNum 1);
  assert (
    message (Seq (Num 1, Num 2))
    = "the first expression of a sequence must be unit")

let test_while () =
  (* let x : ref int = new 0 in while !x < 3 do x := !x + 1 *)
  let loop =
    Let
      ( "x",
        TRef TInt,
        New (Num 0),
        Whi
          ( Binop (Lt, Deref (Id "x"), Num 3),
            Asg ("x", Binop (Sum, Deref (Id "x"), Num 1)) ) )
  in
  assert (evaluate loop = (VUnit, [ (0, VNum 3) ]));
  (* A loop whose condition starts false leaves the store untouched. *)
  assert (
    evaluate
      (Let ("x", TRef TInt, New (Num 5), Whi (Bool false, Asg ("x", Num 0))))
    = (VUnit, [ (0, VNum 5) ]));
  assert (message (Whi (Num 1, Unit)) = "the condition of while must be a bool");
  assert (message (Whi (Bool true, Num 1)) = "the body of while must be unit")

let test_division_by_zero () =
  let zero = Binop (Div, Num 1, Num 0) in
  assert (message zero = "division by zero");
  assert (message (Let ("x", TInt, zero, Id "x")) = "division by zero");
  assert (message (If (Bool true, zero, Num 1)) = "division by zero");
  (* let x : ref int = new 0 in while true do x := 1 / !x *)
  assert (
    message
      (Let
         ( "x",
           TRef TInt,
           New (Num 0),
           Whi (Bool true, Asg ("x", Binop (Div, Num 1, Deref (Id "x")))) ))
    = "division by zero")

let run () =
  test_apply_operator ();
  test_constants ();
  test_variables ();
  test_locations ();
  test_not ();
  test_binop ();
  test_if ();
  test_let ();
  test_references ();
  test_assignment ();
  test_sequence ();
  test_while ();
  test_division_by_zero ()
