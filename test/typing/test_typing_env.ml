open L2
open Ast
open Typing_env

let test_lookup_variable () =
  assert (lookup_variable [ ("x", TInt) ] "x" = TInt);
  assert (lookup_variable [ ("x", TInt); ("y", TBool) ] "y" = TBool);
  (* The innermost let puts its variable first, so it shadows the outer one. *)
  assert (lookup_variable [ ("x", TBool); ("x", TInt) ] "x" = TBool);
  assert (Test_helpers.raises_type_error (fun () -> lookup_variable [] "x"));
  assert (
    Test_helpers.raises_type_error (fun () ->
        lookup_variable [ ("x", TInt) ] "z"))

let test_lookup_location () =
  assert (lookup_location [ (0, TInt) ] 0 = TInt);
  assert (lookup_location [ (0, TInt); (1, TBool) ] 1 = TBool);
  assert (Test_helpers.raises_type_error (fun () -> lookup_location [] 0));
  assert (
    Test_helpers.raises_type_error (fun () -> lookup_location [ (0, TInt) ] 1))

let test_error_messages () =
  assert (
    Test_helpers.type_error_message (fun () -> lookup_variable [] "x")
    = "undeclared variable x");
  assert (
    Test_helpers.type_error_message (fun () -> lookup_location [] 7)
    = "unknown location 7")

let run () =
  test_lookup_variable ();
  test_lookup_location ();
  test_error_messages ()
