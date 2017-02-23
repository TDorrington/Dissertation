(* -------------------------------------------- FUZZ TESTING ------------------------------------- *)

fun prettyPrintFuzz(e,n) = (case fuzzExpr(e,n) of 

	  SOME (e,i) => prettyPrintExpression(Expression(e)) ^ "\n " ^ Int.toString(i)
	| NONE       => "FAIL");


prettyPrintFuzz(toCounterExpr(Value(Concrete(N(1)))),1); 		(* 2.0   *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(R(1.0)))),1); 		(* false *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(B(true)))),1);		(*  2    *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(EmptyList))),1);	(* {}    *)

prettyPrintFuzz(toCounterExpr(Value(Concrete(N(1)))),0); 		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(R(1.0)))),2); 		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(B(true)))),3);		(* FAIL *)
prettyPrintFuzz(toCounterExpr(Value(Concrete(EmptyList))),4);	(* FAIL *)

(* --- (fn x:int => ((1,[1]) * (3,[2]), [3]), [4]) --- *)

prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),1); (* fn x:int => 2.0 * 3 *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),2); (* fn x:int => 1 * 2.0 *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),3); (* fn x:int => 1::3    *)
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))),4); (* fn x:real => 1 * 3  *)

(* --- ( [1,3,5], [1] ) --- *)

prettyPrintFuzz(toCounterExpr(Value(VList([Concrete(N(1)),Concrete(N(3)),Concrete(N(5))]))),1); (* {a0=1,a1=3,a2=5} *)
	
(* --- ([ fn x:int => (1,[1]), fn x:int => (2,[2]), fn y:int => (10,[3]) ], [4]) --- *)

prettyPrintFuzz(toCounterExpr(Value(VList([
	Fun(Var("x"),Int,Value(Concrete(N(1)))),
	Fun(Var("x"),Int,Value(Concrete(N(2)))),
	Fun(Var("y"),Int,Value(Concrete(N(10))))]))),1);
(* 	[ fn x:int => 2.0, fn x:int => 2, fn y:int => 10 ] *)
	
prettyPrintFuzz(toCounterExpr(Value(VList([
	Fun(Var("x"),Int,Value(Concrete(N(1)))),
	Fun(Var("x"),Int,Value(Concrete(N(2)))),
	Fun(Var("y"),Int,Value(Concrete(N(10))))]))),2);
(* 	[ fn x:int => 1, fn x:int => 2.0, fn y:int => 10 ] *)
	
prettyPrintFuzz(toCounterExpr(Value(VList([
	Fun(Var("x"),Int,Value(Concrete(N(1)))),
	Fun(Var("x"),Int,Value(Concrete(N(2)))),
	Fun(Var("y"),Int,Value(Concrete(N(10))))]))),3);
(* 	[ fn x:int => 1, fn x:int => 2, fn y:int => 2.0 ] *)
	
prettyPrintFuzz(toCounterExpr(Value(VList([
	Fun(Var("x"),Int,Value(Concrete(N(1)))),
	Fun(Var("x"),Int,Value(Concrete(N(2)))),
	Fun(Var("y"),Int,Value(Concrete(N(10))))]))),4);
(* 	{ a0=fn x:int => 1, a1=fn x:int => 2, a2=fn y:int => 10 ] *)

(* --- ( { a=1, b=true, c=2.0, d=[], e=fn x:int => ( (2,[1]) * (x,[2]), [3]), f={} }, [4]) --- *)

prettyPrintFuzz(toCounterExpr(Value(VRecord([
	(Lab("a"),Concrete(N(1))),
	(Lab("b"),Concrete(B(true))),
	(Lab("c"),Concrete(R(2.0))),
	(Lab("d"),Concrete(EmptyList)),
	(Lab("e"),Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))),
	(Lab("f"),VRecord([]))]))),1);
(* { a=1, b=true, c=2.0, d=[], e=fn x:int => 2.0*x, f={} } *)
	
prettyPrintFuzz(toCounterExpr(Value(VRecord([
	(Lab("a"),Concrete(N(1))),
	(Lab("b"),Concrete(B(true))),
	(Lab("c"),Concrete(R(2.0))),
	(Lab("d"),Concrete(EmptyList)),
	(Lab("e"),Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))),
	(Lab("f"),VRecord([]))]))),2);
(* { a=1, b=true, c=2.0, d=[], e=fn x:int => 2*3, f={} } *)
	
prettyPrintFuzz(toCounterExpr(Value(VRecord([
	(Lab("a"),Concrete(N(1))),
	(Lab("b"),Concrete(B(true))),
	(Lab("c"),Concrete(R(2.0))),
	(Lab("d"),Concrete(EmptyList)),
	(Lab("e"),Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))),
	(Lab("f"),VRecord([]))]))),3);
(* { a=1, b=true, c=2.0, d=[], e=fn x:int => 2::x, f={} } *)
	
prettyPrintFuzz(toCounterExpr(Value(VRecord([
	(Lab("a"),Concrete(N(1))),
	(Lab("b"),Concrete(B(true))),
	(Lab("c"),Concrete(R(2.0))),
	(Lab("d"),Concrete(EmptyList)),
	(Lab("e"),Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))),
	(Lab("f"),VRecord([]))]))),4);
(* {b=true, c=2.0, d=[], e=fn x:int => 2.0*x, f={} } *)

(* --- (fn x:'a => ( (x,[1]) * (y,[2]), [3] ), [4]) --- *)

prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),1);
(* fn x:'a => 3*y *)
	
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),2);
(* fn x:'a => x*3 *)
	
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),3);
(* fn x:'a => x::y *)
	
prettyPrintFuzz(toCounterExpr(Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),4);
(* fn x:'a => x::y, with expression 3 changed, not 4 *)
						
(* --- (fn x:'a => (fn y:'b => ( (x,[1]) * (y,[2]), [3]), [4]), [5]) --- *)

prettyPrintFuzz(toCounterExpr(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))))),1);
(* fn x:'a => fn y:'b => 3*y *)
						
prettyPrintFuzz(toCounterExpr(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))))),2);
(* fn x:'a => fn y:'b => x*3 *)			
			
prettyPrintFuzz(toCounterExpr(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))))),3);
(* fn x:'a => fn y:'b => x::y *)
					
prettyPrintFuzz(toCounterExpr(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))))),4);
(* fn x:'a => fn y:'b => x::y, with expression 3 changed, not 4 *)
						
prettyPrintFuzz(toCounterExpr(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))))),5);
(* fn x:'a => fn y:'b => x::y, with expression 3 changed, not 5 *)
	
(* --- ( ( (1,[1]) + (2,[2]), [3]) * ( (3,[4]) - (4,[5]), [6]), [7]) --- *)

prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),1);
(* (2.0+2) * (3-4) *)
	
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),2);
(* (1+2.0) * (3-4) *)
					
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),3);
(* (1::2) * (3-4) *)
					
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),4);
(* (1+2) * (2.0-4) *)
			
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),5);
(* (1+2) * (3-2.0) *)
					
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),6);
(* (1+2) * (3::4) *)
					
prettyPrintFuzz(toCounterExpr(
	ArithExpr(TIMES,ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
					ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))))),7);
(* (1+2) :: (3-4) *)				
					
(* ( case ( (1,[1]) = (2,[2]), [3]) of true -> (1,[4]) | false -> (2,[5]) |  x -> (3,[6]) | _ -> (4,[7]), [8]) --- *)  

prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),1);
(* case 2.0 = 2 of true -> 1 | false -> 2 | x -> 3 | _ -> 4 *)
   
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),2);
(* case 1 = 2.0 of true -> 1 | false -> 2 | x -> 3 | _ -> 4 *)

prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),3);
(* case (1 2) of true -> 1 | false -> 2 | x -> 3 | _ -> 4 *)
   
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),4);
(* case 1 = 2 of true -> 2.0 | false -> 2 | x -> 3 | _ -> 4 *)
   
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),5);
(* case 1 = 2 of true -> 1 | false -> 2.0 | x -> 3 | _ -> 4 *)
   
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),6);
(* case 1 = 2 of true -> 1 | false -> 2 | x -> 2.0 | _ -> 4 *)
   
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),7);
(* case 1 = 2 of true -> 1 | false -> 2 | x -> 3 | _ -> 2.0 *)
  
prettyPrintFuzz(toCounterExpr(Case(
	BoolExpr(EQ,Value(Concrete(N(1))),Value(Concrete(N(2)))),
  [(PVal(B(true)),Value(Concrete(N(1)))),
   (PVal(B(false)),Value(Concrete(N(2)))),
   (PVar(Var("x")),Value(Concrete(N(3)))),
   (PWildcard,Value(Concrete(N(4))))])),8); 
(* case (1 2) of true -> 1 | false -> 2 | x -> 3 | _ -> 4, expression 3 changed, not 8 *)

(* --- ( if ( (1,[1]) < (2,[2]), [3] ) 
		 then ( (fn x:int => (10,[4]), [5]) (20,[6]), [7])
		 else ( (fn y:int => (20,[8]), [9]) (30,[10]), [11]), [12]) --- *)
		 
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),1);
(* if 2.0<2 then (fn x:int=>10) 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),2);
(* if 1<2.0 then (fn x:int=>10) 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),3);
(* if (1 2) then (fn x:int=>10) 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),4);
(* if 1<2 then (fn x:int=>2.0) 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),5);
(* if 1<2 then (fn x:real=>10) 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),6);
(* if 1<2 then (fn x:int=>10) 2.0 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),7);
(* if 1<2 then (fn x:int=>10) + 20 else (fn y:int=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),8);
(* if 1<2 then (fn x:int=>10) 20 else (fn y:int=>2.0) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),9); 
(* if 1<2 then (fn x:int=>10) 20 else (fn y:real=>20) 30 *)
	
prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),10);
(* if 1<2 then (fn x:int=>10) 20 else (fn y:int=>20) 2.0 *)

prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),11);
(* if 1<2 then (fn x:int=>10) 20 else (fn y:int=>20) + 30 *)

prettyPrintFuzz(toCounterExpr(Condition(
	BoolExpr(LESS,Value(Concrete(N(1))),Value(Concrete(N(2)))),
	App(Value(Fun(Var("x"),Int,Value(Concrete(N(10))))),Value(Concrete(N(20)))),
	App(Value(Fun(Var("y"),Int,Value(Concrete(N(20))))),Value(Concrete(N(30)))))),12);
(* if 1<2 then (fn x:int=>10) + 20 else (fn y:int=>20) 30, changes expression 7, not 12 *)
 
(* --- ( {a = ( (1,[1]) :: ([],[2]), [3]),
		  b = ( (1,[4]) :: ([2,3],[5]), [6]),
		  c = ( (10,[7]) * (10,[8]), [9])}, [10]) --- *)
		  
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),1);
(* { a = 2.0::[], b = 1::[2,3], c = 10*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),2);
(* { a = 1::{}, b = 1::[2,3], c = 10*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),3);
(* { a = 1=[], b = 1::[2,3], c = 10*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),4);
(* { a = 1::[], b = 2.0::[2,3], c = 10*10 } *)	

prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),5);
(* { a = 1::[], b = 1::{a0=2,a1=3}, c = 10*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),6);
(* { a = 1::[], b = 1=[2,3], c = 10*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),7);
(* { a = 1::[], b = 1::[2,3], c = 2.0*10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),8);
(* { a = 1::[], b = 1::[2,3], c = 10*2.0 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),9);
(* { a = 1::[], b = 1::[2,3], c = 10::10 } *)
	
prettyPrintFuzz(toCounterExpr(Record([
	(Lab("a"),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
	(Lab("b"),Cons(Value(Concrete(N(1))),Value(VList([Concrete(N(2)),Concrete(N(3))])))),
	(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))])),10);
(* { b = 1::[2,3], c = 10*10 } *)

(* --- ([(let val x:int = (1,[1]) in ( (x,[2]) * (x,[3]), [4]) end, [5]),
         (let val x:'a = ([],[6]) in ( (1,[7])::(2,[8]), [9]), [10])], [11])  --- *)

prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),1);		 
(* [ let val x:int = 2.0 in x * x, let val x:'a = [] in 1::x end ] *)
	
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),2);
(* [ let val x:int = 1 in 3 * x, let val x:'a = [] in 1::x end ] *)
	 
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),3);
(* [ let val x:int = 1 in x * 3, let val x:'a = [] in 1::x end ] *)
	 
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),4);
(* [ let val x:int = 1 in x :: x, let val x:'a = [] in 1::x end ] *)
	 
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),5);
(* [ let val x:real = 1 in x * x, let val x:'a = [] in 1::x end ] *)
	 
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),6);
(* [ let val x:int = 1 in x * x, let val x:'a = {} in 1::x end ] *)
	 
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),7);
(* [ let val x:int = 1 in x * x, let val x:'a = [] in 2.0::x end ] *)	 
	
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),8);
(* [ let val x:int = 1 in x * x, let val x:'a = [] in 1::3 end ] *)	 
	
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),9);
(* [ let val x:int = 1 in x * x, let val x:'a = [] in 1=x end ] *)	
	
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),10);
(* [ let val x:int = 1 in x * x, let val x:'a = {} in 1::x end ], expression 6 changed, not 10 *)
	
prettyPrintFuzz(toCounterExpr(List(
	[Let(Var("x"),Int,Value(Concrete(N(1))),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 Let(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Concrete(EmptyList)),Cons(Value(Concrete(N(1))),Variable(Var("x"))))])),11);
(* { a0 = let val x:int = 1 in x * x, a1 = let val x:'a = [] in 1::x end } *)
	 
(* --- (let val rec:(int->int) = ( fn x:int => ( (x,[1]) * (10,[2]), [3] ), [4] ) in 
	    ( [ ( (x,[5]) (1,[6]), [7]), ( (x,[8]) ( (x,[9]) + (y,[10]), [11] ), [12] )], [13]) end, [14]) --- *)

prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),1);
(* let val rec x:(int->int) = (fn x:int => 3*10) in [(x 1), (x (x+y))] end *)

prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),2);
(* let val rec x:(int->int) = (fn x:int => x*2.0) in [(x 1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),3);
(* let val rec x:(int->int) = (fn x:int => x::10) in [(x 1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),4);
(* let val rec x:(int->int) = (fn x:real => x*10) in [(x 1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),5);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(3 1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),6);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 2.0), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),7);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x+1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),8);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 1), (3 (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),9);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 1), (x (3+y))] end *)

prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),10);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 1), (x (x+3))] end *)
		
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),11);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 1), (x (x::y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),12);
(* let val rec x:(int->int) = (fn x:int => x*10) in [(x 1), (x (x+y))] end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),13);
(* let val rec x:(int->int) = (fn x:int => x*10) in {a0=(x 1), a1=(x + (x+y))} end *)
		  
prettyPrintFuzz(toCounterExpr(LetRec(Var("x"),TFun(Int,Int),
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
	List([App(Variable(Var("x")),Value(Concrete(N(1)))),
	      App(Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),14);
(* let val rec x:{a:int} = (fn x:int => x*10) in [(x 1), (x (x+y))] end *)

fun prettyPrintFuzzList(e) = (case fuzz(e) of 

	  NONE   => "FAIL"
	  
	| SOME l => 
		
		let fun iterPrintList(l) = (case l of 
			
				  []          => ""
				| [(e,_)]     => "[" ^ prettyPrintExpression(Expression(e)) ^ "]"
				| (e,_)::rest => "[" ^ prettyPrintExpression(Expression(e)) ^ "], " ^ iterPrintList(rest))
				
		in iterPrintList(iterDropCounterExpr(l)) end);
				
prettyPrintFuzzList(toCounterExpr(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(3))))))));
(* [fn x:real => 1 * 3], [fn x:int => 1 :: 3], [fn x:int => 1 * 2.0], [fn x:int => 2.0 * 3] *)

prettyPrintFuzzList(toCounterExpr(Value(Fun(Var("x"),Int,
	  Case(ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))),
	     [(PVal(N(1)),Cons(Value(Concrete(N(1))),Value(Concrete(EmptyList)))),
		  (PVal(N(10)),Value(VList([Concrete(N(10))]))),
		  (PWildcard,List([ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(20))))]))])))));
(* [fn x:real => case x * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 20]], 
   [fn x:int => case x :: x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 20]], 
   [fn x:int => case x * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> {a0=10 * 20}], 
   [fn x:int => case x * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 :: 20]], 
   [fn x:int => case x * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 2.0]], 
   [fn x:int => case x * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [2.0 * 20]], 
   [fn x:int => case x * x of 1 -> 1 :: [ ] | 10 -> {a0=10} | _ -> [10 * 20]], 
   [fn x:int => case x * x of 1 -> 1 = [ ] | 10 -> [10] | _ -> [10 * 20]],
   [fn x:int => case x * x of 1 -> 1 :: {} | 10 -> [10] | _ -> [10 * 20]],
   [fn x:int => case x * x of 1 -> 2.0 :: [ ] | 10 -> [10] | _ -> [10 * 20]],
   [fn x:int => case x :: x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 20]], 
   [fn x:int => case x * 3 of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 20]], 
   [fn x:int => case 3 * x of 1 -> 1 :: [ ] | 10 -> [10] | _ -> [10 * 20]] *)
   
 
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)