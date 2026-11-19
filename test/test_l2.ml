open L2
open Ast_datatypes

let () =
  let open Interpreter in
  assert (interpreter (Binop (Sum, Num 5, Num 3)) = "8 : int");
  assert (interpreter (New (Num 10)) = "<loc 0> : ref int");
  assert (interpreter (Deref (New (Num 5))) = "5 : int");
  let assignment =
    Let ("x", TRef TInt, New (Num 5),
         Seq (Asg ("x", Num 10), Deref (Id "x")))
  in
  assert (interpreter assignment = "10 : int");
  let loop =
    Let ("x", TRef TInt, New (Num 0),
         Whi (Binop (Lt, Deref (Id "x"), Num 3),
              Asg ("x", Binop (Sum, Deref (Id "x"), Num 1))))
  in
  assert (Big_step.bigStepState loop = (Big_step.VUnit, [(0, Big_step.VNum 3)]));
  assert (Type_infer.typeInferMem [(42, TBool)] (Loc 42) = TRef TBool);
  assert (Big_step.bigStepMem [(42, Big_step.VBool true)] (Loc 42)
          = (Big_step.VLoc 42, [(42, Big_step.VBool true)]));
  assert (match Type_infer.typeInfer (Binop (Sum, Num 1, Bool true)) with
          | _ -> false
          | exception Type_infer.TypeError _ -> true);
  assert (interpreter (Binop (Div, Num 6, Num 0)) =
          "Erro de execucao - Divisao por zero")
