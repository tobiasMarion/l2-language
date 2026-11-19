open Ast
open Type_infer
open Big_step

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TRef t -> "ref " ^ string_of_type t

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VLoc loc -> "<loc " ^ string_of_int loc ^ ">"

(* Verifica os tipos antes de executar *)
let interpreter (e : ast) : string =
  try
    let t = typeInfer e in
    let (v, _s) = bigStepState e in
    string_of_value v ^ " : " ^ string_of_type t
  with
  | TypeError msg -> "Erro de tipo - " ^ msg
  | RunTimeError msg -> "Erro de execucao - " ^ msg;;


