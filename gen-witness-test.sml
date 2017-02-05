(* ------------------------------------------ GEN WITNESS TESTING ----------------------------------- *)

findWitness(Value(Fun(Var("x"),Int,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))));
(* 1 *)
	
findWitness(Value(Fun(Var("x"),Int,Condition(Variable(Var("x")),Value(Concrete(N(1))),Value(Concrete(N(2)))))));
(* 1 *)

findWitness(Value(Fun(Var("x"),Real,Condition(Value(Concrete(B(true))),Value(Concrete(N(1))),Variable(Var("x"))))));
(* 1.0 *)
   
findWitness(Value(Fun(Var("x"),TRecord([(Lab("a"),THole(TypeHole(TypeVar("b")))),(Lab("b"),Real)]),
	Case(Variable(Var("x")),
  [(PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVar(Var("x")))]),
	List([Variable(Var("x")),Value(Concrete(N(1)))]))]))));
(* {a=1,b=1.0} *)
 
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PVal(N(1)),ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(1))))),
	    (PVal(N(2)),ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(R(2.0))))),
		(PWildcard,Value(Concrete(N(3))))]))));
(* 1 *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(Variable(Var("x")),
		ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(R(2.0))))))));
(* fn x:int => v['a13] *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("c"))),
	App(Variable(Var("x")),
		BoolExpr(EQ,Value(Concrete(R(1.0))),Value(Concrete(N(1))))))));
(* fn x:bool => v['a18] *)
		
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PRecord([(Lab("1"),PVar(Var("f"))),(Lab("2"),PCons(PVar(Var("a")),PWildcard))]),
		 App(Variable(Var("f")),ArithExpr(TIMES,Variable(Var("a")),Value(Concrete(B(true))))))]))));
(* {1=fn x:'''a32 => v['22], 2=[v['''a32]]} *)
		
(* -------------------------------------------- FUZZ TESTING ------------------------------------- *)

fun prettyPrintFuzz(e,n) = (case fuzzExpr(e,n) of 

	  SOME (e,i) => prettyPrintExpression(Expression(e)) ^ "\n " ^ Int.toString(i)
	| NONE       => "FAIL");


prettyPrintFuzz(toCounterExpr(Value(Concrete(N(1)))),1); 		(* 1 => 2.0 *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(R(1.0)))),1); 		(* 1.0 => false *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(B(true)))),1);		(* true => 2 *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(EmptyList))),1);	(* [] => fn x:int => 1.0 *)

prettyPrintFuzz(toCounterExpr(Value(Concrete(N(1)))),0); 		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(R(1.0)))),2); 		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(B(true)))),3);		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(EmptyList))),4);	(* FAIL *)

(* --- (fn x:int => ((1,[1]) * (3,[2]), [3]), [4]) --- *)

prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),1); (* fn x:int => 2.0 * 3 *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),2); (* fn x:int => 1 * 2.0 *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),3); (* fn x:int => 1::3    *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),4); (* fn x:real => 1 * 3  *)



(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)