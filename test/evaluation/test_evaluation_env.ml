open L2
open Value
open Evaluation_env

let test_lookup_variable () =
  assert (lookup_variable [ ("x", VNum 1) ] "x" = VNum 1);
  assert (lookup_variable [ ("x", VNum 1); ("y", VBool true) ] "y" = VBool true);
  (* The innermost let puts its variable first, so it shadows the outer one. *)
  assert (lookup_variable [ ("x", VBool true); ("x", VNum 1) ] "x" = VBool true);
  assert (
    Test_helpers.runtime_error_message (fun () -> lookup_variable [] "x")
    = "undeclared variable: x")

let test_lookup_location () =
  assert (lookup_location [ (0, VNum 1) ] 0 = VNum 1);
  assert (lookup_location [ (0, VNum 1); (1, VUnit) ] 1 = VUnit);
  assert (
    Test_helpers.runtime_error_message (fun () -> lookup_location [] 7)
    = "unknown location 7")

let test_fresh_location () =
  assert (fresh_location [] = 0);
  assert (fresh_location [ (0, VUnit) ] = 1);
  (* One past the highest location, even if the store is not in order. *)
  assert (fresh_location [ (2, VUnit); (5, VUnit); (1, VUnit) ] = 6)

let test_write () =
  assert (write [ (0, VNum 1) ] 0 (VNum 2) = [ (0, VNum 2) ]);
  (* The other locations are kept. *)
  let store = write [ (0, VNum 1); (1, VNum 5) ] 0 (VNum 2) in
  assert (List.assoc 0 store = VNum 2);
  assert (List.assoc 1 store = VNum 5);
  assert (List.length store = 2);
  assert (
    Test_helpers.runtime_error_message (fun () -> write [] 0 (VNum 1))
    = "unknown location 0")

let run () =
  test_lookup_variable ();
  test_lookup_location ();
  test_fresh_location ();
  test_write ()
