(* ------------------------------------------ GEN WITNESS TESTING ----------------------------------- *)

findWitness(Value(Fun(Var("x"),Int,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))));
(* fn x:int => x/x
   Witness: 1 *)
	
findWitness(Value(Fun(Var("x"),Int,Condition(Variable(Var("x")),Value(Concrete(N(1))),Value(Concrete(N(2)))))));
(* fn x:int => if x then 1 else 2
   Witness: 1 *)

findWitness(Value(Fun(Var("x"),Real,Condition(Value(Concrete(B(true))),Value(Concrete(N(1))),Variable(Var("x"))))));
(* fn x:real => if true then 1 else x 
   Witness: 1.0 *)
   
findWitness(Value(Fun(Var("x"),TRecord([(Lab("a"),THole(TypeHole(TypeVar("b")))),(Lab("b"),Real)]),
	Case(Variable(Var("x")),
  [(PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVar(Var("x")))]),
	List([Variable(Var("x")),Value(Concrete(N(1)))]))]))));
(* fn x:{a:'b,b:real} => case x of {a=1,b=x} -> [x,1] 
   Witness: {a=1,b=1.0} *)
 
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PVal(N(1)),ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(1))))),
	    (PVal(N(2)),ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(R(2.0))))),
		(PWildcard,Value(Concrete(N(3))))]))));
(* fn x:'b => case x of 1 -> 1*1 | 2 -> 2*2.0 | _ -> 3 
   Witness: 1 *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(Variable(Var("x")),
		ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(R(2.0))))))));
(* fn x:'b => x (1*2.0)
   Witness: fn x:int => v['a13] *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("c"))),
	App(Variable(Var("x")),
		BoolExpr(EQ,Value(Concrete(R(1.0))),Value(Concrete(N(1))))))));
(* fn x:'c => x (1.0=1)
   Witness: fn x:bool => v['a18] *)
		
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PRecord([(Lab("1"),PVar(Var("f"))),(Lab("2"),PCons(PVar(Var("a")),PWildcard))]),
		 App(Variable(Var("f")),ArithExpr(TIMES,Variable(Var("a")),Value(Concrete(B(true))))))]))));
(* fn x:'b => case x of {1=f,2=a::_} -> f (a*true)
   Witness: {1=fn x:'''a32 => v['22], 2=[v['''a32]]} *)
			
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(App(Variable(Var("x")),Value(Concrete(N(1)))),
	    Value(Fun(Var("y"),Bool,Cons(Variable(Var("y")),Value(VList([Concrete(N(2))])))))))));
(* fn x:'b => (x 1) (fn y:bool => y::[2])
   Witness: fn x:int => fn x:(bool->bool list) => v['a37] *)
   
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(App(Variable(Var("x")),Value(Concrete(N(1)))),
	    Value(Fun(Var("y"),Bool,Cons(Variable(Var("x")),Value(Concrete(EmptyList)))))))));
(* fn x:'b => (x 1) (fn y:bool => x::[])
   Witness: fn x:int => v['a44] (occurs check) *)
  
prettyPrintExpression(Expression(toCounterExpr(App(Value(Fun(Var("x"),Int,Case(Variable(Var("x")),
[(PVal(N(0)),Value(Concrete(B(true)))),
 (PWildcard,Value(Concrete(B(false))))]))),Value(Concrete(N(1)))))));
	
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)