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

(* Ignore 3rd substitution, gamma, of variables -> expressions *)

prettyPrintConfig(narrow(N(3),Int,[],[],[]));		(* 3, [], []    *)
prettyPrintConfig(narrow(R(3.0),Real,[],[],[]));	(* 3.0, [], []  *)
prettyPrintConfig(narrow(N(3),Real,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrow(B(true),Bool,[],[],[]));	(* true, [], [] *)
prettyPrintConfig(narrow(B(false),Int,[],[],[]));	(* Stuck *)

prettyPrintConfig(narrow(ValuePair(N(3),N(4)),Pair(Int,Int),[],[],[])); 		(* (3,4), [], []	  *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Real,Bool),[],[],[])); 	(* (3.0,true), [], [] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Bool,Real),[],[],[])); 	(* Stuck	  *)

prettyPrintConfig(narrow(N(3),a',[],[],[]));		(* 3, [], ['a->Int] 	 *)
prettyPrintConfig(narrow(R(3.0),a',[],[],[]));		(* 3.0, [], ['a->Real]   *)
prettyPrintConfig(narrow(R(3.0),a'',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrow(N(3),a''',[],[],[]));		(* 3, [], ['''a->Int] 	 *)
prettyPrintConfig(narrow(B(true),a'',[],[],[]));	(* true, [], [''a->Bool] *)
prettyPrintConfig(narrow(B(false),a''',[],[],[]));	(* Stuck *)

prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),a',[],[],[])); 	(* (3.0,true), [], ['a1 -> bool, 'a0 -> real, 'a -> ('a0 * 'a1)] *)
prettyPrintConfig(narrow(ValuePair(N(4),N(4)),a'',[],[],[]));	 	(* (4,4), [], [''a1 -> int, ''a0 -> int, ''a -> (''a0 * ''a1)] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),R(5.0)),b'',[],[],[]));	(* Stuck, [], [''b -> (''a0 * ''a1)] *)
prettyPrintConfig(narrow(ValuePair(B(true),N(3)),a''',[],[],[]));	(* Stuck *)
prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),N(3)),ValuePair(R(2.0),R(1.0))),a''',[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(R(2.0),R(1.0))),a''',[],[],[]));	(* Stuck, [], [] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(N(1),N(5))),a'',[],[],[]));		
(* ((3,true),(1,5)), [], [''a5 -> int, ''a4 -> int, ''a1 -> (''a4 * ''a5), ''a3 -> bool, ''a2 -> int, ''a0 -> (''a2 * ''a3), ''a -> (''a0 * ''a1)] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(B(false),N(1))),a'',[],[],[]));	
(* ((3,true),(false,1)), [], [''a2 -> int, ''a3 -> bool, ''a4 -> bool, ''a5 -> int, ''a1 -> (''a4 * ''a5), ''a0 -> (''a2 * ''a3), ''a -> (''a0 * ''a1)] *)

prettyPrintConfig(narrow(ValuePair(ValuePair(N(3),B(true)),ValuePair(R(2.0),R(1.0))),a',[],[],[]));
(* ((3,true),(2.0,1.0)), [], ['a5 -> real, 'a4 -> real, 'a1 -> ('a4 * 'a5), 'a3 -> bool, 'a2 -> int, 'a0 -> ('a2 * 'a3), 'a -> ('a0 * 'a1)] *)

prettyPrintConfig(narrow(va',Int,[],[],[])); 			(* 1, [v['a]->1],   ['a->Int]   *)
prettyPrintConfig(narrow(va'',Int,[],[],[]));			(* 1, [v[''a]->1],  [''a->Int]  *)
prettyPrintConfig(narrow(va''',Int,[],[],[]));			(* 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrow(va''',Bool,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrow(va',Pair(Int,Int),[],[],[]));  (* (1,1), [v['a]->(1,1)], ['a->Int*Int] *)
prettyPrintConfig(narrow(va',Pair(b',c'),[],[],[]));	(* (v['b],v['c]), [v['a]->(v['b],v['c])], ['a -> 'b * 'c] *)
prettyPrintConfig(narrow(va',Pair(a',b'),[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrow(va''',Pair(c',b'),[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrow(va''',b'',[],[],[]));			(* 1, [v['''a]->1], ['''a->Int,''b->Int] *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),Real,[],[],[])); 	(* 3.0/5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),Int,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(N(3)),Value(R(5.0))),Real,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Real,[],[],[]));	(* Stuck  *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),Real,[],[],[]));		(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va'')),Real,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),Real,[],[],[]));			(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va''')),Real,[],[],[]));		(* 3.0/1.0, [ v['''a]->1.0], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),Real,[],[],[]));		(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),a',[],[],[])); 		(* 3.0/5.0, [], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),a'',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(5.0)),Value(R(5.0))),a''',[],[],[]));		(* 5.0/5.0, [], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),a',[],[],[]));			(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(R(5.0))),b',[],[],[]));			(* 1.0/5.0, [v['a]->1.0], ['a->Real, 'b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(va'')),a',[],[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),a''',[],[],[]));			(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real, '''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),a''',[],[],[]));		(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(R(3.0)),Value(R(5.0))),Real,[],[],[])); 		(* 3.0+5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(R(3.0)),Value(R(5.0))),Int,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(N(3)),Value(R(5.0))),Real,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Real,[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),Real,[],[],[]));		(* 1.0 - 5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(R(3.0)),Value(va'')),Real,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),Real,[],[],[]));		(* 1.0 - 1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Real,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Int,[],[],[]));			(* 5 * 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),Bool,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),Real,[],[],[]));		(* 1.0 * 1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(R(3.0)),Value(R(5.0))),a',[],[],[]));		(* 3.0*5.0, [], ['a->'''a0, '''a0->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),a''',[],[],[]));		(* 1.0 - 5.0, [v['a]->1.0], ['a->Real,'''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(R(5.0))),a'',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),a',[],[],[]));			(* v['''a0]-v['''b], [ v['a]->v['''a0] ], ['''a0->'''b, 'a->'''a0] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(N(5)),Value(va''')),a''',[],[],[]));			(* 5*1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a'',[],[],[]));			(* 1*1, [ v['''a]->1.0], ['''a->Int, ''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a''',[],[],[]));		(* v['''a] * v['''a], [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'),Value(va')),a''',[],[],[]));			(* v['''a] * v['''a], [v['a]->v['''a]], ['a->'''a] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va''),Value(va'')),a''',[],[],[]));			(* 1*1, [v[''a]->1], [''a->Int, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),Bool,[],[],[])); 		(* 3.0 < 5.0, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),Int,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(N(3)),Value(R(5.0))),Bool,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Bool,[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),Bool,[],[],[]));			(* 1 >= 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(R(3.0)),Value(va'')),Bool,[],[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a',[],[],[])); 		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a'',[],[],[]));		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(R(3.0)),Value(R(5.0))),a''',[],[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),a',[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),b',[],[],[]));		(* 1>=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(N(2))),a',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(R(3.0)),Value(va'')),a'',[],[],[]));	(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(va'),Value(vb''')),Bool,[],[],[]));		
(* v['''a0] <= v['''b], [ v['a]->v['''a0] ], ['a->'''a0, '''a0->'''b] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(N(5)),Value(va''')),Bool,[],[],[]));			
(* 5 < 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(va'''),Value(va''')),Bool,[],[],[]));			
(* v['''a0] > v['''a0], [v['''a]->v['''a0]], ['''a->'''a0] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(R(5.0))),Bool,[],[],[])); 		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(R(5.0))),Int,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(R(5.0))),Bool,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(ValuePair(R(5.0),B(true)))),Bool,[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),Bool,[],[],[]));			(* 1 = 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(R(3.0)),Value(va'')),Bool,[],[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a',[],[],[])); 		(* 3=5, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a'',[],[],[]));		(* 3=5, [], [''a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),a''',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),a',[],[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(N(2))),b',[],[],[]));			(* 1=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(3)),Value(va''')),b'',[],[],[]));		(* 3=1, [v['''a]->1.0], [''b->Bool, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),Bool,[],[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),c',[],[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int, 'c->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb'')),a',[],[],[]));		
(* true = true, [ v['a]->v[true], v[''b]->true ], ['a->Bool, ''b->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(N(5)),Value(va''')),Bool,[],[],[]));		
(* 5 = 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'''),Value(va''')),Bool,[],[],[]));			
(* 1 = 1, [v['''a]->1], ['''a->int, ''a0->int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(R(5.0),B(true)))),Bool,[],[],[]));
(* (5.0,true) = (5.0,true), [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(R(5.0),B(true)))),a',[],[],[]));
(* (5.0,true) = (5.0,true), [], ['a->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(R(5.0),B(true))),Value(ValuePair(N(5),B(true)))),Bool,[],[],[]));
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(ValuePair(ValuePair(R(5.0),B(true)),ValuePair(R(5.0),B(true)))),
										 Value(ValuePair(ValuePair(R(5.0),B(true)),ValuePair(R(5.0),B(true))))),Bool,[],[],[]));
(* ((5.0,true),(5.0,true)) = ((5.0,true),(5.0,true)), [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,ExpressionPair(Value(ValuePair(R(5.0),B(true))),Condition(Value(va'),Value(N(4)),Value(N(5)))),
										 ExpressionPair(Value(ValuePair(vb',B(true))),Value(N(3)))),Bool,[],[],[]));
(* ((5.0,true), if true then 4 else 5) = ((1.0,true),3), [v['a]->true, v['b]->1.0], ['a->bool,'b->real] *)

prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							 Pair(Int,Bool),[],[],[]));
(* (if true then 1 else 1, if true then true else true), [v['e]->true,v['d]->true,v['c]->1,v['b]->1,v['a]->true],
   ['a->bool,'b->int,'c->int,'d->bool,'e->bool] *)
	
prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							a'',[],[],[]));
(* (if true then v[''a0] else v[''a0], if true then v[''a1] else v[''a1]), 
   [v['e] -> v[''a1], v['d] -> v[''a1], v['c] -> v[''a0], v['b] -> v[''a0], v['a] -> true],
   [''a->(''a0,''a1), 'a->Bool, ''a0->''a2, ''a1->''a3, 'b->''a2, 'c->''a2, 'd->''a3, ''e->''a3] *)   	

prettyPrintConfig(narrowExpr(ExpressionPair(Condition(Value(va'),Value(vb'),Value(vc')),Condition(Value(B(true)),Value(vd'),Value(ve'))),
							a''',[],[],[]));
(* Stuck *)  
   
prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(B(true)),Value(B(false))),
	Condition(Condition(Value(B(true)),Value(B(true)),Value(vb')),Value(vc'),Value(N(3))),
	Condition(Value(B(true)),Value(N(5)),Value(vd'))),Int,[],[],[]));
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
	Condition(Value(B(true)),Value(N(5)),Value(vd'))),a''',[],[],[]));
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
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[],[]));
(* case (1,1) of (x,y) -> x+y, [ v['a]->1,v['b]->1 ], ['a->Int,'b->Int] *)				
	
prettyPrintConfig(narrowExpr(
	Case(ExpressionPair(Value(va'),Value(vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),c',[],[],[]));
(* case (v['''a0],v['''a0]) of (x,y) -> x+y, 
[ v['a]->v['''a0],v['b]->v['''a0] ], 
['a->'''a0,'b->'''a0, 'c->'''a0] *)	
	
prettyPrintConfig(narrowExpr(
	Case(ExpressionPair(Value(va'),Value(vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Real,[],[],[]));
(* case (1.0,1.0) of (x,y) -> x+y, [v['a]->1.0,v['b]->1.0], ['a->Real,'b->Real] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[],[]));
(* case (1.0,1.0) of (x,y) -> x+y, [v['a]->1.0,v['b]->1.0], ['a->Real,'b->Real] *)
	
prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Real,[],[],[]));
(* case (1.0,1.0) of (x,y) -> x/y, [v['a]->1.0,v['b]->1.0], ['a->Real,'b->Real] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),a''',[],[],[]));
(* case (1.0,1.0) of (x,y) -> x/y, 
[v['a]->1.0,v['b]->1.0], 
['a->Real,'b->Real,'''a->Real] *)

prettyPrintConfig(narrowExpr(
	Case(Value(ValuePair(va',vb')),VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Int,[],[],[]));
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(
	Case(Value(vc'),
		 VariablePair(Var("x"),Var("y")),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Real,[],[],[]));
(* Stuck, [], [] *)