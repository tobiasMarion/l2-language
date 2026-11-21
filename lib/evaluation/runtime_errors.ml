(* Runtime errors, and the checks that raise them. *)

open Value

exception Runtime_error of string

let runtime_error (message : string) = raise (Runtime_error message)

(* Returns [b] when [v] is the boolean [b], and fails otherwise. [what]
   describes the evaluated expression, e.g. "the condition of if". *)
let expect_bool (v : value) (what : string) : bool =
  match v with VBool b -> b | _ -> runtime_error (what ^ " must be a bool")

(* Fails unless [v] is unit. *)
let expect_unit (v : value) (what : string) : unit =
  match v with VUnit -> () | _ -> runtime_error (what ^ " must be unit")

(* Returns [l] when [v] is the location [l], and fails otherwise. *)
let expect_location (v : value) (what : string) : int =
  match v with VLoc l -> l | _ -> runtime_error (what ^ " must be a reference")
