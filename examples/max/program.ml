(* AST of program.l2: the larger of two numbers. Result: 7 : int *)

open L2.Ast

let program =
  Let
    ( "x",
      TInt,
      Num 7,
      Let ("y", TInt, Num 3, If (Binop (Gt, Id "x", Id "y"), Id "x", Id "y")) )
