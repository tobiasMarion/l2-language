(* Runs a program: infers its type first, and only evaluates it if it is well
   typed. *)

open Ast

let interpret (e : ast) : string =
  try
    let t = Type_inference.infer e in
    let v, _ = Big_step.evaluate e in
    Value_printer.string_of_value v ^ " : " ^ Type_printer.string_of_type t
  with
  | Type_errors.Type_error message -> "Type error: " ^ message
  | Runtime_errors.Runtime_error message -> "Runtime error: " ^ message
