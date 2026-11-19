open Ast
open Environment

(* Valores de execucao. Referencias sao enderecos de memoria. *)
type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VLoc of int;;

(* Ambiente de execucao: identificador -> valor *)
type runEnv = (string * value) list;;

(* Memoria de execucao: endereco -> valor *)
type store = (int * value) list;;

let empty_run : runEnv = [];;
let empty_store : store = [];;

exception RunTimeError of string;;


(* Escolhe uma localizacao nova que nao aparece na memoria. *)
let fresh_location (s : store) : int =
  1 + List.fold_left
    (fun maior (loc, _) -> max maior loc)
    (-1) s;;

(* Atualiza uma localizacao existente na memoria, sem modificar as demais. *)
let rec write_store (s : store) (loc : int) (v : value) : store =
  match s with
  | [] ->
      raise (RunTimeError "Localizacao de memoria inexistente")
  | (endereco, valor_antigo) :: resto ->
      if endereco = loc then
        (endereco, v) :: resto
      else
        (endereco, valor_antigo) :: write_store resto loc v;;

(* eval retorna (valor final, memoria final).
   As expressoes recebem a memoria resultante da expressao anterior. *)
let rec eval (r : runEnv) (s : store) (e : ast) : value * store =
  match e with

  | Num n -> (VNum n, s)

  | Bool b -> (VBool b, s)

  | Unit -> (VUnit, s)

  | Loc loc ->
      (match lookup s loc with
       | Some _ -> (VLoc loc, s)
       | None ->
           raise (RunTimeError "Localizacao de memoria inexistente"))

  | Id x ->
      (match lookup r x with
       | Some v -> (v, s)
       | None ->
           raise (RunTimeError ("Variavel nao declarada: " ^ x)))

  | Not e1 ->
      let (v1, s1) = eval r s e1 in
      (match v1 with
       | VBool b -> (VBool (not b), s1)
       | _ -> raise (RunTimeError "Not exige booleano"))

  | Binop (op, e1, e2) ->
      let (v1, s1) = eval r s e1 in
      let (v2, s2) = eval r s1 e2 in
      let resultado =
        match (op, v1, v2) with
        | (Sum, VNum n1, VNum n2) -> VNum (n1 + n2)
        | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
        | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
        | (Div, VNum _, VNum 0) ->
            raise (RunTimeError "Divisao por zero")
        | (Div, VNum n1, VNum n2) -> VNum (n1 / n2)
        | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
        | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
        | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
        | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
        | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
        | _ ->
            raise (RunTimeError "Operandos incompativeis no Binop")
      in
      (resultado, s2)

  | If (e1, e2, e3) ->
      let (vcond, s1) = eval r s e1 in
      (match vcond with
       | VBool true -> eval r s1 e2
       | VBool false -> eval r s1 e3
       | _ ->
           raise (RunTimeError "Condicao do If deve ser booleana"))

  | Let (x, _t, e1, e2) ->
      let (v1, s1) = eval r s e1 in
      let r' = update r x v1 in
      eval r' s1 e2

  | New e1 ->
      let (v1, s1) = eval r s e1 in
      let loc = fresh_location s1 in
      let s2 = update s1 loc v1 in
      (VLoc loc, s2)

  | Deref e1 ->
      let (v1, s1) = eval r s e1 in
      (match v1 with
       | VLoc loc ->
           (match lookup s1 loc with
            | Some v -> (v, s1)
            | None ->
                raise (RunTimeError "Referencia invalida"))
       | _ ->
           raise (RunTimeError "Deref exige uma referencia"))

  | Asg (x, e1) ->
      let vx =
        match lookup r x with
        | Some v -> v
        | None ->
            raise (RunTimeError ("Variavel nao declarada: " ^ x))
      in
      (match vx with
       | VLoc loc ->
           let (v1, s1) = eval r s e1 in
           let s2 = write_store s1 loc v1 in
           (VUnit, s2)
       | _ ->
           raise (RunTimeError "Atribuicao exige uma referencia"))

  | Seq (e1, e2) ->
      let (v1, s1) = eval r s e1 in
      (match v1 with
       | VUnit -> eval r s1 e2
       | _ ->
           raise (RunTimeError
                    "Primeira expressao da sequencia deve ser Unit"))

  | Whi (condicao, corpo) ->
      let (vcond, s1) = eval r s condicao in
      (match vcond with
       | VBool false -> (VUnit, s1)
       | VBool true ->
           let (vcorpo, s2) = eval r s1 corpo in
           (match vcorpo with
            | VUnit -> eval r s2 (Whi (condicao, corpo))
            | _ ->
                raise (RunTimeError "Corpo do While deve ser Unit"))
       | _ ->
           raise (RunTimeError "Condicao do While deve ser booleana"));;


(* Resultado da execucao, sem exibir a memoria. *)
let bigStep (e : ast) : value =
  fst (eval empty_run empty_store e);;

(* Resultado e memoria final: util para testar New, Asg e Whi. *)
let bigStepState (e : ast) : value * store =
  eval empty_run empty_store e;;

(* Permite avaliar uma expressao com uma memoria inicial fornecida. *)
let bigStepMem (s : store) (e : ast) : value * store =
  eval empty_run s e;;

