(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR NARROW *)
(* will use unify in its implementation, so also further tests for this *)

val va' = VHole(SimpleHole(ValueHole(TypeVar("a"))));
val vb' = VHole(SimpleHole(ValueHole(TypeVar("b"))));
val vc' = VHole(SimpleHole(ValueHole(TypeVar("c"))));
val vd' = VHole(SimpleHole(ValueHole(TypeVar("d"))));
val ve' = VHole(SimpleHole(ValueHole(TypeVar("e"))));
val vf' = VHole(SimpleHole(ValueHole(TypeVar("f"))));

val va'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("a"))));
val vb'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("b"))));

val va''' = VHole(SimpleHole(ValueHole(ArithTypeVar("a"))));
val vb''' = VHole(SimpleHole(ValueHole(ArithTypeVar("b"))));

val a' = THole(TypeHole(TypeVar("a")));
val a'1 = TypeHole(TypeVar("a"));
val b' = THole(TypeHole(TypeVar("b")));
val b'1 = TypeHole(TypeVar("b"));
val c' = THole(TypeHole(TypeVar("c")));
val c'1 = TypeHole(TypeVar("c"));
val d' = THole(TypeHole(TypeVar("d")));

val a'' = THole(TypeHole(EqualityTypeVar("a")));
val b'' = THole(TypeHole(EqualityTypeVar("b")));
val b''1 = TypeHole(EqualityTypeVar("b"));
val c'' = THole(TypeHole(EqualityTypeVar("c")));
val d'' = THole(TypeHole(EqualityTypeVar("d")));

val a''' = THole(TypeHole(ArithTypeVar("a")));
val b''' = THole(TypeHole(ArithTypeVar("b")));
val c''' = THole(TypeHole(ArithTypeVar("c")));
val d''' = THole(TypeHole(ArithTypeVar("d")));

prettyPrintConfig(narrow(N(3),Int,[],[]));		(* 3, [], []    *)
prettyPrintConfig(narrow(R(3.0),Real,[],[]));	(* 3.0, [], []  *)
prettyPrintConfig(narrow(N(3),Real,[],[]));		(* Stuck *)
prettyPrintConfig(narrow(B(true),Bool,[],[]));	(* true, [], [] *)
prettyPrintConfig(narrow(B(false),Int,[],[]));	(* Stuck *)

prettyPrintConfig(narrow(ValuePair(N(3),N(4)),Pair(Int,Int),[],[])); 			(* (3,4), [], []	  *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Real,Bool),[],[])); 	(* (3.0,true), [], [] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Bool,Real),[],[])); 	(* Stuck	  *)

prettyPrintConfig(narrow(N(3),a',[],[]));		(* 3, [], ['a->Int] 	 *)
prettyPrintConfig(narrow(R(3.0),a',[],[]));		(* 3.0, [], ['a->Real]   *)
prettyPrintConfig(narrow(R(3.0),a'',[],[]));	(* Stuck *)
prettyPrintConfig(narrow(N(3),a''',[],[]));		(* 3, [], ['''a->Int] 	 *)
prettyPrintConfig(narrow(B(true),a'',[],[]));	(* true, [], [''a->Bool] *)
prettyPrintConfig(narrow(B(false),a''',[],[]));	(* Stuck *)

prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),a',[],[])); 	(* (3.0,true), [], ['a1 -> bool, 'a0 -> real, 'a -> ('a0 * 'a1)] *)
prettyPrintConfig(narrow(ValuePair(N(4),N(4)),a'',[],[]));	 	(* (4,4), [], [''a1 -> int, ''a0 -> int, ''a -> (''a0 * ''a1)] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),R(5.0)),b'',[],[]));	(* Stuck, [], [''b -> (''a0 * ''a1)] *)
prettyPrintConfig(narrow(ValuePair(B(true),N(3)),a''',[],[]));	(* Stuck *)
prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),N(3)),ValuePair(R(2.0),R(1.0))),a''',[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(R(2.0),R(1.0))),a''',[],[]));	(* Stuck, [], [] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(N(1),N(5))),a'',[],[]));		
(* ((3,true),(1,5)), [], [''a5 -> int, ''a4 -> int, ''a1 -> (''a4 * ''a5), ''a3 -> bool, ''a2 -> int, ''a0 -> (''a2 * ''a3), ''a -> (''a0 * ''a1)] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(B(false),N(1))),a'',[],[]));	
(* ((3,true),(false,1)), [], [''a2 -> int, ''a3 -> bool, ''a4 -> bool, ''a5 -> int, ''a1 -> (''a4 * ''a5), ''a0 -> (''a2 * ''a3), ''a -> (''a0 * ''a1)] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(R(2.0),R(1.0))),a',[],[]));
(* ((3,true),(2.0,1.0)), [], ['a5 -> real, 'a4 -> real, 'a1 -> ('a4 * 'a5), 'a3 -> bool, 'a2 -> int, 'a0 -> ('a2 * 'a3), 'a -> ('a0 * 'a1)] *)

prettyPrintConfig(narrow(va',Int,[],[])); 			(* 1, [v['a]->1],   ['a->Int]   *)
prettyPrintConfig(narrow(va'',Int,[],[]));			(* 1, [v[''a]->1],  [''a->Int]  *)
prettyPrintConfig(narrow(va''',Int,[],[]));			(* 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrow(va''',Bool,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrow(va',Pair(Int,Int),[],[])); (* (1,1), [v['a]->(1,1)], ['a->Int*Int] *)
prettyPrintConfig(narrow(va',Pair(b',c'),[],[]));	(* (v['b],v['c]), [v['a]->(v['b],v['c])], ['a->'a0*'a1, 'a0->'b, 'a1->'c] *)
prettyPrintConfig(narrow(va',Pair(a',b'),[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrow(va''',Pair(c',b'),[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrow(va''',b'',[],[]));			(* 1, [v['''a]->1], ['''a->Int,''b->Int] *)

(* FOR TESTING USE NARROWEXPR FIlE *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),Real,[],[])); 	(* 3.0/5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),Int,[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(N(3)),Value(R(5.0))),Real,[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Real,[],[]));	(* Stuck  *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),Real,[],[]));		(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va'')),Real,[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),Real,[],[]));		(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va''')),Real,[],[]));		(* 3.0/1.0, [ v['''a]->1.0], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),Real,[],[]));		(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),a',[],[])); 		(* 3.0/5.0, [], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),a'',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(5.0)),Value(R(5.0))),a''',[],[]));	(* 5.0/5.0, [], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),a',[],[]));			(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),b',[],[]));			(* 1.0/5.0, [v['a]->1.0], ['a->Real, 'b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va'')),a',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),a''',[],[]));		(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real, '''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),a''',[],[]));		(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(R(3.0)),Value(R(5.0))),Real,[],[])); 		(* 3.0+5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(R(3.0)),Value(R(5.0))),Int,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(N(3)),Value(R(5.0))),Real,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Real,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),Real,[],[]));		(* 1.0 - 5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(R(3.0)),Value(va'')),Real,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),Real,[],[]));		(* 1.0 - 1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Real,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Int,[],[]));			(* 5 * 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Bool,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),Real,[],[]));		(* 1.0 * 1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(R(3.0)),Value(R(5.0))),a',[],[]));		(* 3.0*5.0, [], ['a->'''a0, '''a0->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),a''',[],[]));		(* 1.0 - 5.0, [v['a]->1.0], ['a->Real,'''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),a'',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),a',[],[]));		(* v['''a0]-v['''a0], [v['''b]->v['''a0],v['a]->v['''a0]], ['''b->'''a0,'a->'''a0] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),a''',[],[]));		(* 5*1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a'',[],[]));		(* 1*1, [ v['''a]->1.0], ['''a->Int, ''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a''',[],[]));		(* v['''a] * v['''a], [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'),Value(va')),a''',[],[]));			(* v['''a] * v['''a], [v['a]->v['''a]], ['a->'''a] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va''),Value(va'')),a''',[],[]));			(* 1*1, [v[''a]->1], [''a->Int, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),Bool,[],[])); 		(* 3.0 < 5.0, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),Int,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(N(3)),Value(R(5.0))),Bool,[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Bool,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),Bool,[],[]));			(* 1 >= 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(R(3.0)),Value(va'')),Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a',[],[])); 		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a'',[],[]));		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a''',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),a',[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),b',[],[]));			(* 1>=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),a',[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(R(3.0)),Value(va'')),a'',[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(va'),Value(vb''')),Bool,[],[]));		
(* v['''b] <= v['''b], [v['a]->v['''a0]], ['a->'''b] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(N(5)),Value(va''')),Bool,[],[]));			
(* 5 < 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(va'''),Value(va''')),Bool,[],[]));			
(* v['''a] > v['''a], [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(R(5.0))),Bool,[],[])); 	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(R(5.0))),Int,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(R(5.0))),Bool,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Bool,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),Bool,[],[]));			(* 1 = 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(va'')),Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a',[],[])); 		(* 3=5, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a'',[],[]));		(* 3=5, [], [''a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a''',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),a',[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),b',[],[]));		(* 1=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(va''')),b'',[],[]));		(* 3=1, [v['''a]->1.0], [''b->Bool, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),Bool,[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),c',[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int, 'c->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb'')),a',[],[]));		
(* true = true, [ v['a]->v[true], v[''b]->true ], ['a->Bool, ''b->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(5)),Value(va''')),Bool,[],[]));		
(* 5 = 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'''),Value(va''')),Bool,[],[]));			
(* 1 = 1, [v['''a]->1], ['''a->int, ''a0->int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(N(5),B(true))),Value(ValuePair(N(5),B(true)))),Bool,[],[]));
(* (5,true) = (5,true) *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(N(5),B(true))),Value(ValuePair(N(5),B(true)))),a',[],[]));
(* (5,true) = (5,true), [], ['a->bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(R(5.0),B(true)))),Bool,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(R(5.0),B(true)))),a',[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(N(5),B(true)))),Bool,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(ValuePair(N(5),B(true)),ValuePair(N(5),B(true)))),
										 Value(ValuePair(ValuePair(N(5),B(true)),ValuePair(N(5),B(true))))),Bool,[],[]));
(* ((5,true),(5,true)) = ((5,true),(5,true)), [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,ExpressionPair(Value(ValuePair(N(5),B(true))),Condition(Value(va'),Value(N(4)),Value(N(5)))),
										 ExpressionPair(Value(ValuePair(vb',B(true))),Value(N(3)))),Bool,[],[]));
(* ((5,true), if true then 4 else 5) = ((1,true),3), [v['a]->true, v['b]->1.0], ['a->bool,'b->real] *)

prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							 Pair(Int,Bool),[],[]));
(* (if true then 1 else 1, if true then true else true), [v['e]->true,v['d]->true,v['c]->1,v['b]->1,v['a]->true],
   ['a->bool,'b->int,'c->int,'d->bool,'e->bool] *)
	
prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							a'',[],[]));
(* (if true then v[''a0] else v[''a0], if true then v[''a1] else v[''a1]), 
   [v['e] -> v[''a1], v['d] -> v[''a1], v['c] -> v[''a0], v['b] -> v[''a0], v['a] -> true],
   [''a->(''a0,''a1), 'a->Bool, ''a0->''a2, ''a1->''a3, 'b->''a2, 'c->''a2, 'd->''a3, ''e->''a3] *)   	

prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							a''',[],[]));
(* Stuck *)  
   

prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(B(true)),Value(B(false))),
	Condition(Condition(Value(B(true)),Value(B(true)),Value(vb')),Value(vc'),Value(N(3))),
	Condition(Value(B(true)),Value(N(5)),Value(vd'))),Int,[],[]));
(*
if (if v['a] then true else false)
then if (if true then true else v['b]) then v['c] else 3
else if true then 5 else v['d] 
=>
if if true then true else false 
then if if true then true else true then 1 else 3
else if true then 5 else 1,
[v['a]->true,v['b]->true,v['c]->1,v['d]->1],
[a'->bool,'b->bool,'c->int,'d->int] 
*)

prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(B(true)),Value(B(false))),
	Condition(Condition(Value(B(true)),Value(B(true)),Value(vb')),Value(vc'),Value(N(3))),
	Condition(Value(B(true)),Value(N(5)),Value(vd'))),a''',[],[]));
(*
if (if v['a] then true else false)
then if (if true then true else v['b]) then v['c] else 3
else if true then 5 else v['d] 
=>
if if true then true else false 
then if if true then true else true then v['''a] else 3
else if true then 5 else 1,
[v['d] -> 1, v['c] -> v['''a], v['b] -> true, v['a] -> true], 
['d -> int, '''a -> int, 'c -> '''a, 'b -> bool, 'a -> bool] *)

prettyPrintConfig(narrowExpr(
	Case(ExpressionPair(Value(va'),Value(vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* case (1,1) of (x,y) -> x+y, 
  [v['a40] -> 1, v['a39] -> 1, v['b] -> v['a40], v['a] -> v['a39]]
  ['a40 -> int, 'a39 -> int, 'b -> 'a40, 'a -> 'a39]
  [x -> 1, y -> 1] *)	

prettyPrintConfig(narrowExpr(
	Case(ExpressionPair(Value(va'),Value(vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),c',[],[]));
(* case (v['''43],v['''43]) of (x,y) -> x+y, 
  [v['a42] -> v['''a43], v['a41] -> v['''a43], v['b] -> v['a42], v['a] -> v['a41]]
  ['a42 -> '''a43, 'a41 -> '''a43, 'c -> '''a43, 'b -> 'a42, 'a -> 'a41]
  [x -> v['''a43], y -> v['''a43]] *)
	
prettyPrintConfig(narrowExpr(
	Case(ExpressionPair(Value(va'),Value(vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Real,[],[]));
(* case (1.0,1.0) of (x,y) -> x+y, 
  [v['a45] -> 1.0, v['a44] -> 1.0, v['b] -> v['a45], v['a] -> v['a44]]
  ['a45 -> real, 'a44 -> real, 'b -> 'a45, 'a -> 'a44]
  [x -> 1.0, y -> 1.0] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* case (1,1) of (x,y) -> x+y,
  [v['a47] -> 1, v['a46] -> 1, v['b] -> v['a47], v['a] -> v['a46]]
  ['a47 -> int, 'a46 -> int, 'b -> 'a47, 'a -> 'a46]
  [x -> 1, y -> 1] *)
	
prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Real,[],[]));
(* case (1.0,1.0) of (x,y) -> x/y, 
  [v['a49] -> 1.0, v['a48] -> 1.0, v['b] -> v['a49], v['a] -> v['a48]]
  ['a49 -> real, 'a48 -> real, 'b -> 'a49, 'a -> 'a48]
  [x -> 1.0, y -> 1.0] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),a''',[],[]));
(* case (1.0,1.0) of (x,y) -> x/y, 
  [v['a51] -> 1.0, v['a50] -> 1.0, v['b] -> v['a51], v['a] -> v['a50]]
  ['a51 -> real, 'a50 -> real, '''a -> real, 'b -> 'a51, 'a -> 'a50]
  [x -> 1.0, y -> 1.0] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(
	Case(Value(vc'),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Real,[],[]));
(* case (1.0,1.0) of ( x , y ) -> x / y
  [v['a55] -> 1.0, v['a54] -> 1.0, v['c] -> (v['a54],v['a55])]
  ['a55 -> real, 'a54 -> real, 'a56 -> 'a54, 'a57 -> 'a55, 'c -> ('a56 * 'a57)]
  [x -> 1.0, y -> 1.0] *)

prettyPrintConfig(narrowExpr(
	Case(Value(va'),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* case (1,1) of ( x , y ) -> x + y
  [v['a59] -> 1, v['a58] -> 1, v['a] -> (v['a58],v['a59])]
  ['a59 -> int, 'a58 -> int, 'a60 -> 'a58, 'a61 -> 'a59, 'a -> ('a60 * 'a61)]
  [x -> 1, y -> 1] *)

 prettyPrintConfig(narrowExpr(
	Case(Value(va'),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),a''',[],[]));
(* case (v['''a],v['''a]) of ( x , y ) -> x + y
  [v['a63] -> v['''a], v['a62] -> v['''a], v['a] -> (v['a62],v['a63])]
  ['a63 -> '''a, 'a62 -> '''a, 'a64 -> 'a62, 'a65 -> 'a63, 'a -> ('a64 * 'a65)]
  [x -> v['''a], y -> v['''a]] *)
  
 
 prettyPrintConfig(narrowExpr(
	Case(Value(va'''),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),b''',[],[]));
(* Stuck *)
  
 prettyPrintConfig(narrowExpr(
	Case(Value(va''),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),b'',[],[]));
(* case (1,1) of ( x , y ) -> x + y
  v[''a71] -> 1, v[''a70] -> 1, v[''a] -> (v[''a70],v[''a71])]
  [''a71 -> int, ''a70 -> int, ''b -> int, 'a68 -> ''a70, 'a69 -> ''a71, ''a -> (''a70 * ''a71)]
  [x -> 1, y -> 1] *)
   
 prettyPrintConfig(narrowExpr(
	Case(Value(va''),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),b''',[],[]));
(* case (1,1) of ( x , y ) -> x + y
  [v[''a75] -> 1, v[''a74] -> 1, v[''a] -> (v[''a74],v[''a75])]
  [''a75 -> int, ''a74 -> int, '''b -> int, 'a72 -> ''a74, 'a73 -> ''a75, ''a -> (''a74 * ''a75)]
  [x -> 1, y -> 1] *)
  
prettyPrintConfig(narrow(
	Func(Var("x"),Int,ArithExpr(TIMES,Value(N(2)),Variable(Var("x")))),
	Fun(Int,Int),
	[], []));
(* fn x:int => 2*x, [], [] *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,ArithExpr(TIMES,Value(N(2)),Variable(Var("x")))),
	a',
	[], []));
(* fn x:int => 2*x, [], 
  ['''a80 -> int, 'a77 -> '''a80, 'a78 -> int, 'a79 -> 'a77, 'a -> ('a78 -> 'a79)] *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,ArithExpr(TIMES,Value(N(2)),Variable(Var("x")))),
	Fun(Int,Real),
	[], []));
(* Stuck *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,ArithExpr(TIMES,Value(N(2)),Variable(Var("x")))),
	Fun(Real,Int),
	[], []));
(* Stuck *)

prettyPrintConfig(narrow(
	Func(Var("x"),Real,ArithExpr(TIMES,Value(R(2.0)),Variable(Var("x")))),
	Fun(Real,Real),
	[], []));
(* fn x:real => 2.0 * x, [], [] *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,
		 Value(Func(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))),
	Fun(Int,Fun(Int,Int)),
	[],[]));
(* fn x:int => fn x0:int => x0+x0, [], [] *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,
		 Value(Func(Var("y"),Int,
			   Case(ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			        VariablePair(Var("x"),Var("y")),
					ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))))),Fun(Int,Fun(Int,Int)),[],[]));
(* fn x:int => fn y:int => case (x,y) of (x0,y0) -> x0+y0, [], ['a0->int, 'a1->int] *)

prettyPrintConfig(narrow(
	Func(Var("x"),Int,
		 Value(Func(Var("x"),Int,
			   Value(Func(Var("x"),Int,
					 Value(Func(Var("x"),Int,Value(va''')))))))),
	Fun(Int,Fun(Int,Fun(Int,Fun(Int,Real)))),[],[]));
(* fn x:int => fn x0:int => fn x01:int => fn x012:int => 1.0,
   [v['''a]->1.0], ['''a->real] *)
   
prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),Fun(Int,Fun(Int,Int)),[],[]));
(* fn x:int => fn y:int => if true then 1 else 1
  [v['b]->1, v['a]->1, v[''a]->true],
  [''a->bool, 'b->int,'a->int] *)
  
prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),Fun(Int,Fun(Int,Real)),[],[]));
(* Stuck *)

prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),Fun(Int,Fun(Int,a''')),[],[]));
(* fn x:int => fn y:int => if true then 1 else 1
  [v['b]->1, v['a]->1, v[''a]->true],
  [''a->bool, 'b->int,'a->int,'''a->int] *)

 prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),Fun(Int,Fun(b''',a''')),[],[]));
(* fn x:int => fn y:int => if true then 1 else 1
  [v['b]->1, v['a]->1, v[''a]->true],
  [''a->bool, ,'a->int,'''a->int,'''b->int,'b->'''b] *)
  
  prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),Fun(c''',Fun(b''',a''')),[],[]));
(* fn x:'''a => fn y:'''a => if true then v['''a] else v['''a]
  [v['b]->v['''a], v['a]->v['''a], v[''a]->true],
  ['''c->'''a, ,''a->bool,'''b->'''a,'b->'''b,'a->'''c] *)
  
  prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),a',[],[]));
(* Stuck *)

  prettyPrintConfig(narrow(
	Func(Var("x"),a',
		 Value(Func(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),c',[],[]));
(* fn x:'a88 => fn y:'a88 => if true then v['a88] else v['a88] 
  [v['b]->v['a88], v['a]->v['a88], v[''a]->true],
  ['b -> 'a88, 'a -> 'a88, ''a -> bool, 'a89 -> 'b, 'a90 -> 'a88, 'a85 -> ('a89 -> 'a90), 'a86 -> 'a, 'a87 -> 'a85, 'c -> ('a86 -> 'a87)]
*)

prettyPrintConfig(narrow(
	Func(Var("x"),Pair(Int,Int),
	BoolExpr(EQ,Variable(Var("x")),Value(ValuePair(N(2),N(2))))),
	Fun(Pair(Int,Int),Bool),[],[]));
(* fn x : (int * int) => x=(2,2), [], [] *)

 prettyPrintConfig(narrowExpr(App(
	Value(Func(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
	Value(N(2))),Int,[],[]));
(* (fn x:int => x+x) 2, [], [] *)
	
prettyPrintConfig(narrowExpr(App(App(
	Value(Func(Var("x"),Real,
		Value(Func(Var("y"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))))),
	Value(R(2.0))),Value(R(3.0))),Real,[],[]));
(* ( fn x : real => fn y : real => x/y) 2.0 3.0, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(B(true)),
			  Value(Func(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Func(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(N(4))),Int,[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(B(true)),
			  Value(Func(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Func(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(N(4))),a',[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4 , [], ['a->int] *)
	
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Func(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Func(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(N(4))),Int,[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 4 , 
  [v['c]->1,v['b]->1,v['a]->true],
  ['c->int,'b->int,'a->bool] *)
  
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Func(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Func(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(va''')),Int,[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 1 , 
  [v['c]->1,v['b]->1,v['a]->true],v['''a]-.1,
  ['c->int,'b->int,'a->bool,'''a->int] *) 
	
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)