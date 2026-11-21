(* Big-step evaluator for L2: e, σ ⇓ v, σ′.
   Variables are kept in an environment instead of being substituted, which is
   equivalent for this language and avoids rewriting the expression. *)

open Ast
open Value
open Runtime_errors
open Evaluation_env

(* The value of [op] applied to [v1] and [v2]. *)
let apply_operator (op : bop) (v1 : value) (v2 : value) : value =
  match (op, v1, v2) with
  | Sum, VNum n1, VNum n2 -> VNum (n1 + n2)
  | Sub, VNum n1, VNum n2 -> VNum (n1 - n2)
  | Mul, VNum n1, VNum n2 -> VNum (n1 * n2)
  | Div, VNum _, VNum 0 -> runtime_error "division by zero"
  | Div, VNum n1, VNum n2 -> VNum (n1 / n2)
  | Eq, VNum n1, VNum n2 -> VBool (n1 = n2)
  | Lt, VNum n1, VNum n2 -> VBool (n1 < n2)
  | Gt, VNum n1, VNum n2 -> VBool (n1 > n2)
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
  | _ -> runtime_error "incompatible operands"

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
      ignore (lookup_location store l);
      (VLoc l, store)
  | Id x -> (lookup_variable environment x, store)
  | Not e1 ->
      let v1, store1 = evaluate store e1 in
      (VBool (not (expect_bool v1 "the operand of not")), store1)
  | Binop (op, e1, e2) ->
      let v1, store1 = evaluate store e1 in
      let v2, store2 = evaluate store1 e2 in
      (apply_operator op v1 v2, store2)
  | If (e1, e2, e3) ->
      let v1, store1 = evaluate store e1 in
      if expect_bool v1 "the condition of if" then evaluate store1 e2
      else evaluate store1 e3
  | Let (x, _, e1, e2) ->
      let v1, store1 = evaluate store e1 in
      evaluate_expression ((x, v1) :: environment) store1 e2
  | New e1 ->
      let v1, store1 = evaluate store e1 in
      let l = fresh_location store1 in
      (VLoc l, (l, v1) :: store1)
  | Deref e1 ->
      let v1, store1 = evaluate store e1 in
      let l = expect_location v1 "the operand of !" in
      (lookup_location store1 l, store1)
  | Asg (x, e1) ->
      let l =
        expect_location (lookup_variable environment x) ("the variable " ^ x)
      in
      let v1, store1 = evaluate store e1 in
      (VUnit, write store1 l v1)
  | Seq (e1, e2) ->
      let v1, store1 = evaluate store e1 in
      expect_unit v1 "the first expression of a sequence";
      evaluate store1 e2
  | Whi (condition, body) ->
      let v1, store1 = evaluate store condition in
      if expect_bool v1 "the condition of while" then (
        let v2, store2 = evaluate store1 body in
        expect_unit v2 "the body of while";
        evaluate store2 e)
      else (VUnit, store1)

(* Final value and store of a closed program. *)
let evaluate (e : ast) : value * store = evaluate_expression [] [] e

(* Final value and store, starting from a given store. *)
let evaluate_with_store (store : store) (e : ast) : value * store =
  evaluate_expression [] store e
