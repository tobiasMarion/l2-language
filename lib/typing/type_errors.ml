(* Type errors, and the checks that raise them. *)

open Ast
open Type_printer

exception Type_error of string

let type_error (message : string) = raise (Type_error message)

(* Fails unless [actual] is [expected]. [what] describes the checked
   expression, e.g. "the condition of if". *)
let expect_type (expected : tp) (actual : tp) (what : string) : unit =
  if expected <> actual then
    type_error
      (what ^ " must have type " ^ string_of_type expected ^ ", but has type "
     ^ string_of_type actual)

(* Returns [t] when [actual] is [ref t], and fails otherwise. *)
let expect_ref (actual : tp) (what : string) : tp =
  match actual with
  | TRef t -> t
  | _ ->
      type_error
        (what ^ " must be a reference, but has type " ^ string_of_type actual)
