open L2
open Ast
open Type_errors

let test_expect_type () =
  (* Matching types: nothing happens. *)
  expect_type TInt TInt "x";
  expect_type (TRef TBool) (TRef TBool) "x";
  assert (
    Test_helpers.type_error_message (fun () ->
        expect_type TInt TBool "the condition")
    = "the condition must have type int, but has type bool");
  (* Reference types are compared all the way down. *)
  assert (
    Test_helpers.raises_type_error (fun () ->
        expect_type (TRef TInt) (TRef TBool) "x"))

let test_expect_ref () =
  assert (expect_ref (TRef TInt) "x" = TInt);
  assert (expect_ref (TRef (TRef TBool)) "x" = TRef TBool);
  assert (
    Test_helpers.type_error_message (fun () -> expect_ref TInt "the operand")
    = "the operand must be a reference, but has type int")

let test_type_error () =
  assert (Test_helpers.type_error_message (fun () -> type_error "boom") = "boom")

let run () =
  test_expect_type ();
  test_expect_ref ();
  test_type_error ()
