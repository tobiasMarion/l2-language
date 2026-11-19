(* Big-step evaluator for L2: e, σ ⇓ v, σ′.
   Variables are kept in an environment instead of being substituted, which is
   equivalent for this language and avoids rewriting the expression. *)

open Ast

(* Values: numbers, booleans, unit and memory locations (references). *)
type value = VNum of int | VBool of bool | VUnit | VLoc of int

(* Environment: maps each variable to its value. *)
type environment = (string * value) list

(* Store σ: maps each memory location to the value it holds. *)
type store = (int * value) list

exception Runtime_error of string

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VLoc l -> "<location " ^ string_of_int l ^ ">"

(* A location that is not yet in the store. *)
let fresh_location (store : store) : int =
  1 + List.fold_left (fun highest (l, _) -> max highest l) (-1) store

(* Replaces the value of an existing location, keeping the others. *)
let write (store : store) (l : int) (v : value) : store =
  if List.mem_assoc l store then (l, v) :: List.remove_assoc l store
  else raise (Runtime_error ("unknown location " ^ string_of_int l))

(* Evaluates [e] to a value, threading the store from each subexpression to
   the next. *)
let rec evaluate_expression (environment : environment) (store : store)
    (e : ast) : value * store =
  let evaluate = evaluate_expression environment in
  match e with
  | Num n -> (VNum n, store)
  | Bool b -> (VBool b, store)
  | Unit -> (VUnit, store)
  | Loc l ->
      if List.mem_assoc l store then (VLoc l, store)
      else raise (Runtime_error ("unknown location " ^ string_of_int l))
  | Id x -> (
      match List.assoc_opt x environment with
      | Some v -> (v, store)
      | None -> raise (Runtime_error ("undeclared variable: " ^ x)))
  | Not e1 -> (
      match evaluate store e1 with
      | VBool b, store1 -> (VBool (not b), store1)
      | _ -> raise (Runtime_error "the operand of not must be a bool"))
  | Binop (op, e1, e2) ->
      let v1, store1 = evaluate store e1 in
      let v2, store2 = evaluate store1 e2 in
      let result =
        match (op, v1, v2) with
        | Sum, VNum n1, VNum n2 -> VNum (n1 + n2)
        | Sub, VNum n1, VNum n2 -> VNum (n1 - n2)
        | Mul, VNum n1, VNum n2 -> VNum (n1 * n2)
        | Div, VNum _, VNum 0 -> raise (Runtime_error "division by zero")
        | Div, VNum n1, VNum n2 -> VNum (n1 / n2)
        | Eq, VNum n1, VNum n2 -> VBool (n1 = n2)
        | Gt, VNum n1, VNum n2 -> VBool (n1 > n2)
        | Lt, VNum n1, VNum n2 -> VBool (n1 < n2)
        | And, VBool b1, VBool b2 -> VBool (b1 && b2)
        | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
        | _ -> raise (Runtime_error "incompatible operands")
      in
      (result, store2)
  | If (e1, e2, e3) -> (
      match evaluate store e1 with
      | VBool true, store1 -> evaluate store1 e2
      | VBool false, store1 -> evaluate store1 e3
      | _ -> raise (Runtime_error "the condition of if must be a bool"))
  | Let (x, _, e1, e2) ->
      let v1, store1 = evaluate store e1 in
      evaluate_expression ((x, v1) :: environment) store1 e2
  | New e1 ->
      let v1, store1 = evaluate store e1 in
      let l = fresh_location store1 in
      (VLoc l, (l, v1) :: store1)
  | Deref e1 -> (
      match evaluate store e1 with
      | VLoc l, store1 -> (
          match List.assoc_opt l store1 with
          | Some v -> (v, store1)
          | None ->
              raise (Runtime_error ("unknown location " ^ string_of_int l)))
      | _ -> raise (Runtime_error "only a reference can be dereferenced"))
  | Asg (x, e1) -> (
      match List.assoc_opt x environment with
      | Some (VLoc l) ->
          let v1, store1 = evaluate store e1 in
          (VUnit, write store1 l v1)
      | Some _ ->
          raise (Runtime_error ("cannot assign to " ^ x ^ ": not a reference"))
      | None -> raise (Runtime_error ("undeclared variable: " ^ x)))
  | Seq (e1, e2) -> (
      match evaluate store e1 with
      | VUnit, store1 -> evaluate store1 e2
      | _ ->
          raise
            (Runtime_error "the first expression of a sequence must be unit"))
  | Whi (condition, body) -> (
      match evaluate store condition with
      | VBool false, store1 -> (VUnit, store1)
      | VBool true, store1 -> (
          match evaluate store1 body with
          | VUnit, store2 -> evaluate store2 e
          | _ -> raise (Runtime_error "the body of while must be unit"))
      | _ -> raise (Runtime_error "the condition of while must be a bool"))

(* Final value and store of a closed program. *)
let evaluate (e : ast) : value * store = evaluate_expression [] [] e

(* Final value and store, starting from a given store. *)
let evaluate_with_store (store : store) (e : ast) : value * store =
  evaluate_expression [] store e
