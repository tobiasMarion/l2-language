(* AST of program.l2: 5! computed with a while loop. Result: 120 : int *)

open L2.Ast

let program =
  Let
    ( "n",
      TInt,
      Num 5,
      Let
        ( "acc",
          TRef TInt,
          New (Num 1),
          Let
            ( "i",
              TRef TInt,
              New (Num 1),
              Seq
                ( Whi
                    ( Binop (Lt, Deref (Id "i"), Binop (Sum, Id "n", Num 1)),
                      Seq
                        ( Asg
                            ( "acc",
                              Binop (Mul, Deref (Id "acc"), Deref (Id "i")) ),
                          Asg ("i", Binop (Sum, Deref (Id "i"), Num 1)) ) ),
                  Deref (Id "acc") ) ) ) )
