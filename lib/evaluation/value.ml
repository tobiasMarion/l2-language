(* Values an L2 program evaluates to. *)

(* Numbers, booleans, unit and memory locations (references). *)
type value = VNum of int | VBool of bool | VUnit | VLoc of int
