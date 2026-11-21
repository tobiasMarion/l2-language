open L2
open Value
open Value_printer

let test_string_of_value () =
  assert (string_of_value (VNum 42) = "42");
  assert (string_of_value (VNum (-3)) = "-3");
  assert (string_of_value (VBool true) = "true");
  assert (string_of_value (VBool false) = "false");
  assert (string_of_value VUnit = "()");
  assert (string_of_value (VLoc 0) = "<location 0>")

let run () = test_string_of_value ()
