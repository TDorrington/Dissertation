(* ------------------------------------------------- TO-COUNTER-EXPR TEST ------------------------------------------------- *)

prettyPrintExpression(Expression(toCounterExpr(Value(Concrete(N(1)))))); 		(* (1,0) 	*)
prettyPrintExpression(Expression(toCounterExpr(Value(Concrete(R(1.0)))))); 		(* (1.0,0) 	*)
prettyPrintExpression(Expression(toCounterExpr(Value(Concrete(B(true)))))); 	(* (true,0) *)
prettyPrintExpression(Expression(toCounterExpr(Value(Concrete(EmptyList))))); 	(* ([],0) 	*)

prettyPrintExpression(Expression(toCounterExpr(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))))));
(* (fn x:int=>(10,0),1) *)

prettyPrintExpression(Expression(toCounterExpr(Value(VRecord([
	(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(EmptyList)),
	(Lab("c"),Fun(Var("x"),Int,Value(Concrete(B(false)))))])))));
(* ( {a=1,b=[],c=fn x:int=>(false,0)}, 1) *)

prettyPrintExpression(Expression(toCounterExpr(Value(VList([
	Concrete(N(1)),Concrete(EmptyList),VList([Concrete(R(1.0))])])))));
(* ([1,[],[1.0]],0) *)

prettyPrintExpression(Expression(toCounterExpr(Variable(Var("x"))))); (* (x,0) *)

prettyPrintExpression(Expression(toCounterExpr(ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(1)))))));
(* ((x,0)+(1,1),2) *)

prettyPrintExpression(Expression(toCounterExpr(ArithExpr(PLUS,
	ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(1)))),
	ArithExpr(TIMES,Value(Concrete(N(5))),Variable(Var("z")))))));
(* ( ( (x,0)+(1,1), 2) + ( (5,3) * (z,4), 5), 6) *)

prettyPrintExpression(Expression(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(EmptyList)),Value(VList([Concrete(N(1))]))),
  [(PWildcard,Value(Concrete(EmptyList))),
   (PVar(Var("x")),Value(Concrete(N(1)))),
   (PVal(B(true)),Value(Concrete(B(true))))]))));
(* ( case ( ([],0)=([1],1), 2) of _ -> ([],3) | x -> (1,4) | true -> (true,5), 6) *)

prettyPrintExpression(Expression(toCounterExpr(Record([
	(Lab("a"),Value(Concrete(N(1)))),
	(Lab("b"),Case(Variable(Var("x")),[(PWildcard,Case(Variable(Var("z")),[(PWildcard,Value(Concrete(EmptyList)))]))])),
	(Lab("c"),ArithExpr(PLUS,Value(Concrete(N(1))),
							 App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
								 Value(Concrete(N(2))))))]))));
(* ({a=(1,0), b=(case (x,1) of _ -> (case (z,2) of _ -> ([],3), 4), 5), 
     c= ( (1,6) + (((fn x:int=> ((x,7)*(x,8),9),10) (2,1)), 12),13)}, 14) *)
   
prettyPrintExpression(Expression(toCounterExpr(CounterExpr(Value(Concrete(N(1))),10))));
(* (1,10) *)
   
prettyPrintExpression(Expression(toCounterExpr(CounterExpr(
	ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(2)))),10))));
(* ( (1,0) * (2,1), 10 ) *)
  
prettyPrintExpression(Expression(toCounterExpr(CounterExpr(
	ArithExpr(TIMES,CounterExpr(Value(Concrete(N(1))),20),CounterExpr(Value(Concrete(N(2))),20)),30))));
(* ((1,20) * (2,20), 30) *)
   
(* ------------------------------------------------- FIND-WITNESS TEST ------------------------------------------------- *)
   
prettyPrintExpression(Expression(findWitness(
	Value(Fun(Var("x"),Int,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))))));
   
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)