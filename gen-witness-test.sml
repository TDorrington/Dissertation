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
			
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)