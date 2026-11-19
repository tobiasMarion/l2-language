open Ast_datatypes
open Environment

type tyenv = (string * tp) list;;

type memenv = (int * tp) list;;

let empty_amb : tyenv = [];;
let empty_mem : memenv = [];;


exception TypeError of string;;

let rec infer (g : tyenv) (m : memenv) (e : ast) : tp =
  match e with 

  | Num _ -> TInt 
  | Bool _ -> TBool
  | Unit -> TUnit
  | Id x ->
      (match lookup g x with
       | Some t -> t
       | None ->
           raise (TypeError
                    ("Variavel nao declarada: " ^ x)))
  | Not e1 ->
      let t1 = infer g m e1 in
      if t1 = TBool then TBool 
      else raise (TypeError "Corpo deve ser um booleano") 
  | Binop (op, e1, e2) ->
      let t1 = infer g m e1 in
      let t2 = infer g m e2 in

      (match op with

       | Sum | Sub | Mul | Div ->
           
           if t1 = TInt && t2 = TInt then TInt 
           else raise (TypeError "Operadores aritméticos requerem operandos inteiros")
       | Eq | Lt | Gt ->
           
           if t1 = TInt && t2 = TInt then TBool
           else raise (TypeError "Operadores relacionais requerem operandos inteiros")
       | Or | And ->
                 
           if t1 = TBool && t2 = TBool then TBool
           else raise (TypeError "Operadores lógicos requerem operandos booleanos")
      )
  | Let (x, t, e1, e2) ->
      let t1 = infer g m e1 in
      if t1 = t then 
        let g' = update g x t in
        infer g' m e2
      else raise (TypeError "Declaração do tipo da variável e seu corpo são diferentes")
  | If (e1, e2, e3) ->
      let t1 = infer g m e1 in
      
      if t1 <> TBool then raise (TypeError "Condição deve ser booleana")
      else 
        let t2 = infer g m e2 in
        let t3 = infer g m e3 in 
        
        if t2 = t3 then t2
        else raise (TypeError "Ramo True e ramo False devem ter o mesmo tipo") 
  | New e1 ->
      let t1 = infer g m e1 in
      TRef t1
  | Deref e1 ->
      (match infer g m e1 with
       | TRef t -> t
       | _ ->
           raise (TypeError
                    "Deref exige uma referencia"))
  | Asg (x, e1) ->
      let tx = infer g m (Id x) in
      let te = infer g m e1 in

      (match tx with
       | TRef t ->
           if te = t then TUnit
           else raise (TypeError "Atribuicao deve conter o mesmo tipo que a expressao")

       | _ ->
           raise (TypeError
                    "Atribuicao exige uma variavel referencia"))
  | Seq (e1, e2) ->
      let t1 = infer g m e1 in
      if t1 = TUnit then
        infer g m e2
      else
        raise (TypeError "Primeira expressao da sequencia deve ser TUnit")
  | Whi (e1, e2) ->
      let tc = infer g m e1 in
      let tb = infer g m e2 in

      if tc <> TBool then raise (TypeError "Condição deve ser booleana") else 
      if tb <> TUnit then raise (TypeError "Corpo do laço deve ser um TUnit") else
        TUnit
  | Loc n ->
      (match lookup m n with
       | Some t -> TRef t
       | None ->
           raise (TypeError
                    "Localizacao de memoria desconhecida"));;


let typeInfer (e : ast) : tp =
  infer empty_amb empty_mem e;;

let typeInferMem (m : memenv) (e : ast) : tp =
  infer empty_amb m e;;


