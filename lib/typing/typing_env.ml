(* What the type inference knows about names: variables and locations. *)

open Ast
open Type_errors

(* Typing context Γ: maps each variable to its type. *)
type context = (string * tp) list

(* Store typing: maps each memory location to the type of the value it holds.
   Only needed to type locations, which appear during evaluation but are not
   written in source programs. *)
type store_typing = (int * tp) list

let lookup_variable (context : context) (x : string) : tp =
  match List.assoc_opt x context with
  | Some t -> t
  | None -> type_error ("undeclared variable " ^ x)

let lookup_location (store_typing : store_typing) (l : int) : tp =
  match List.assoc_opt l store_typing with
  | Some t -> t
  | None -> type_error ("unknown location " ^ string_of_int l)
