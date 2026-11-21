(* Small helpers shared by every test file. *)

open L2

(* True when running [f] raises a type error. [f] is a function so that the
   code only runs here, inside the [match], where the error can be caught. *)
let raises_type_error (f : unit -> 'a) : bool =
  match f () with _ -> false | exception Type_errors.Type_error _ -> true

(* The message of the type error raised by [f], or "no type error". *)
let type_error_message (f : unit -> 'a) : string =
  match f () with
  | _ -> "no type error"
  | exception Type_errors.Type_error message -> message

(* True when running [f] raises a runtime error. *)
let raises_runtime_error (f : unit -> 'a) : bool =
  match f () with
  | _ -> false
  | exception Runtime_errors.Runtime_error _ -> true

(* The message of the runtime error raised by [f], or "no runtime error". *)
let runtime_error_message (f : unit -> 'a) : string =
  match f () with
  | _ -> "no runtime error"
  | exception Runtime_errors.Runtime_error message -> message
