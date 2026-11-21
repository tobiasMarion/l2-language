(* Readable form of values, used in the interpreter's output. *)

open Value

let string_of_value (v : value) : string =
  match v with
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VLoc l -> "<location " ^ string_of_int l ^ ">"
