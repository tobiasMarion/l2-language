(* What the evaluator knows about names and memory: variables and the store. *)

open Value
open Runtime_errors

(* Environment: maps each variable to its value. *)
type environment = (string * value) list

(* Store σ: maps each memory location to the value it holds. *)
type store = (int * value) list

let lookup_variable (environment : environment) (x : string) : value =
  match List.assoc_opt x environment with
  | Some v -> v
  | None -> runtime_error ("undeclared variable: " ^ x)

let lookup_location (store : store) (l : int) : value =
  match List.assoc_opt l store with
  | Some v -> v
  | None -> runtime_error ("unknown location " ^ string_of_int l)

(* A location that is not yet in the store. *)
let fresh_location (store : store) : int =
  1 + List.fold_left (fun highest (l, _) -> max highest l) (-1) store

(* Replaces the value of an existing location, keeping the others. *)
let write (store : store) (l : int) (v : value) : store =
  ignore (lookup_location store l);
  (l, v) :: List.remove_assoc l store
