(* Type inference for L2, following the typing rules of the assignment:
   Γ ⊢ e : T. *)

open Ast

(* Typing context Γ: maps each variable to its type. *)
type context = (string * tp) list

(* Store typing: maps each memory location to the type of the value it holds.
   Only needed to type locations, which appear during evaluation but are not
   written in source programs. *)
type store_typing = (int * tp) list

exception Type_error of string

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TRef t -> "ref " ^ string_of_type t

let rec infer_expression (context : context) (store_typing : store_typing)
    (e : ast) : tp =
  let infer = infer_expression context store_typing in
  match e with
  | Num _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Id x -> (
      match List.assoc_opt x context with
      | Some t -> t
      | None -> raise (Type_error ("undeclared variable: " ^ x)))
  | Not e1 ->
      if infer e1 = TBool then TBool
      else raise (Type_error "the operand of not must be a bool")
  | Binop (op, e1, e2) -> (
      let t1 = infer e1 in
      let t2 = infer e2 in
      match op with
      | Sum | Sub | Mul | Div ->
          if t1 = TInt && t2 = TInt then TInt
          else raise (Type_error "arithmetic operators require int operands")
      | Eq | Lt | Gt ->
          if t1 = TInt && t2 = TInt then TBool
          else raise (Type_error "relational operators require int operands")
      | Or | And ->
          if t1 = TBool && t2 = TBool then TBool
          else raise (Type_error "logical operators require bool operands"))
  | Let (x, t, e1, e2) ->
      let t1 = infer e1 in
      if t1 = t then infer_expression ((x, t) :: context) store_typing e2
      else
        raise
          (Type_error
             ("let " ^ x ^ " is declared as " ^ string_of_type t
            ^ " but bound to a " ^ string_of_type t1))
  | If (e1, e2, e3) ->
      if infer e1 <> TBool then
        raise (Type_error "the condition of if must be a bool")
      else
        let t2 = infer e2 in
        let t3 = infer e3 in
        if t2 = t3 then t2
        else raise (Type_error "both branches of if must have the same type")
  | New e1 -> TRef (infer e1)
  | Deref e1 -> (
      match infer e1 with
      | TRef t -> t
      | _ -> raise (Type_error "only a reference can be dereferenced"))
  | Asg (x, e1) -> (
      match infer (Id x) with
      | TRef t ->
          if infer e1 = t then TUnit
          else
            raise
              (Type_error
                 ("the value assigned to " ^ x ^ " must have type "
                ^ string_of_type t))
      | _ -> raise (Type_error ("cannot assign to " ^ x ^ ": not a reference")))
  | Seq (e1, e2) ->
      if infer e1 = TUnit then infer e2
      else raise (Type_error "the first expression of a sequence must be unit")
  | Whi (e1, e2) ->
      if infer e1 <> TBool then
        raise (Type_error "the condition of while must be a bool")
      else if infer e2 <> TUnit then
        raise (Type_error "the body of while must be unit")
      else TUnit
  | Loc l -> (
      match List.assoc_opt l store_typing with
      | Some t -> TRef t
      | None -> raise (Type_error ("unknown location " ^ string_of_int l)))

(* Type of a closed program. *)
let infer (e : ast) : tp = infer_expression [] [] e

(* Type of an expression that may refer to existing memory locations. *)
let infer_with_store_typing (store_typing : store_typing) (e : ast) : tp =
  infer_expression [] store_typing e
