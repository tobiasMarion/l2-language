open L2
open Ast
open Type_printer

let test_string_of_type () =
  assert (string_of_type TInt = "int");
  assert (string_of_type TBool = "bool");
  assert (string_of_type TUnit = "unit");
  assert (string_of_type (TRef TInt) = "ref int");
  assert (string_of_type (TRef (TRef TInt)) = "ref ref int");
  assert (string_of_type (TRef (TRef TBool)) = "ref ref bool")

let test_string_of_bop () =
  assert (string_of_bop Sum = "+");
  assert (string_of_bop Sub = "-");
  assert (string_of_bop Mul = "*");
  assert (string_of_bop Div = "/");
  assert (string_of_bop Eq = "=");
  assert (string_of_bop Lt = "<");
  assert (string_of_bop Gt = ">");
  assert (string_of_bop And = "&&");
  assert (string_of_bop Or = "||")

let run () =
  test_string_of_type ();
  test_string_of_bop ()
