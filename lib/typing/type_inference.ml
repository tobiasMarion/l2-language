(* Type inference for L2, following the typing rules of the assignment:
   Γ ⊢ e : T. *)

open Ast
open Type_errors
open Type_printer
open Typing_env

(* The type both operands must have, and the type of the result. *)
let operator_type (op : bop) : tp * tp =
  match op with
  | Sum | Sub | Mul | Div -> (TInt, TInt)
  | Eq | Lt | Gt -> (TInt, TBool)
  | And | Or -> (TBool, TBool)

let rec infer_expression (context : context) (store_typing : store_typing)
    (e : ast) : tp =
  let infer = infer_expression context store_typing in
  match e with
  | Num _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Id x -> lookup_variable context x
  | Loc l -> TRef (lookup_location store_typing l)
  | Not e1 ->
      expect_type TBool (infer e1) "the operand of not";
      TBool
  | Binop (op, e1, e2) ->
      let operand_type, result_type = operator_type op in
      let symbol = string_of_bop op in
      expect_type operand_type (infer e1) ("the left operand of " ^ symbol);
      expect_type operand_type (infer e2) ("the right operand of " ^ symbol);
      result_type
  | Let (x, t, e1, e2) ->
      expect_type t (infer e1) ("the value bound to " ^ x);
      infer_expression ((x, t) :: context) store_typing e2
  | If (e1, e2, e3) ->
      expect_type TBool (infer e1) "the condition of if";
      let then_type = infer e2 in
      expect_type then_type (infer e3) "the else branch of if";
      then_type
  | New e1 -> TRef (infer e1)
  | Deref e1 -> expect_ref (infer e1) "the operand of !"
  | Asg (x, e1) ->
      let t = expect_ref (lookup_variable context x) ("the variable " ^ x) in
      expect_type t (infer e1) ("the value assigned to " ^ x);
      TUnit
  | Seq (e1, e2) ->
      expect_type TUnit (infer e1) "the first expression of a sequence";
      infer e2
  | Whi (e1, e2) ->
      expect_type TBool (infer e1) "the condition of while";
      expect_type TUnit (infer e2) "the body of while";
      TUnit

(* Type of a closed program. *)
let infer (e : ast) : tp = infer_expression [] [] e

(* Type of an expression that may refer to existing memory locations. *)
let infer_with_store_typing (store_typing : store_typing) (e : ast) : tp =
  infer_expression [] store_typing e
