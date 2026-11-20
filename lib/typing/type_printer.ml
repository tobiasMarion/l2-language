(* Readable names for types and operators, used in messages and output. *)

open Ast

let rec string_of_type (t : tp) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TRef t -> "ref " ^ string_of_type t

let string_of_bop (op : bop) : string =
  match op with
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"
  | And -> "&&"
  | Or -> "||"
