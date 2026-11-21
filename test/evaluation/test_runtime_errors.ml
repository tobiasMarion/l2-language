open L2
open Value
open Runtime_errors

let test_expect_bool () =
  assert (expect_bool (VBool true) "x");
  assert (not (expect_bool (VBool false) "x"));
  assert (
    Test_helpers.runtime_error_message (fun () ->
        expect_bool (VNum 1) "the condition")
    = "the condition must be a bool")

let test_expect_unit () =
  (* Unit: nothing happens. *)
  expect_unit VUnit "x";
  assert (
    Test_helpers.runtime_error_message (fun () ->
        expect_unit (VNum 1) "the body")
    = "the body must be unit")

let test_expect_location () =
  assert (expect_location (VLoc 3) "x" = 3);
  assert (
    Test_helpers.runtime_error_message (fun () ->
        expect_location (VBool true) "the operand")
    = "the operand must be a reference")

let test_runtime_error () =
  assert (
    Test_helpers.runtime_error_message (fun () -> runtime_error "boom") = "boom")

let run () =
  test_expect_bool ();
  test_expect_unit ();
  test_expect_location ();
  test_runtime_error ()
