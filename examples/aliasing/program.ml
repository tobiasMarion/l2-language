(* AST of program.l2: r and s point to the same location, so writing through
   s changes what r reads. Result: 42 : int *)

open L2.Ast

let program =
  Let
    ( "r",
      TRef TInt,
      New (Num 0),
      Let ("s", TRef TInt, Id "r", Seq (Asg ("s", Num 42), Deref (Id "r"))) )
