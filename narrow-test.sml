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
val vc'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("c"))));

val va''' = VHole(SimpleHole(ValueHole(ArithTypeVar("a"))));
val vb''' = VHole(SimpleHole(ValueHole(ArithTypeVar("b"))));

val a' = THole(TypeHole(TypeVar("a")));
val a'1 = TypeHole(TypeVar("a"));
val b' = THole(TypeHole(TypeVar("b")));
val b'1 = TypeHole(TypeVar("b"));
val c' = THole(TypeHole(TypeVar("c")));
val c'1 = TypeHole(TypeVar("c"));
val d' = THole(TypeHole(TypeVar("d")));
val f' = THole(TypeHole(TypeVar("f")));

val a'' = THole(TypeHole(EqualityTypeVar("a")));
val b'' = THole(TypeHole(EqualityTypeVar("b")));
val b''1 = TypeHole(EqualityTypeVar("b"));
val c'' = THole(TypeHole(EqualityTypeVar("c")));
val d'' = THole(TypeHole(EqualityTypeVar("d")));

val a''' = THole(TypeHole(ArithTypeVar("a")));
val b''' = THole(TypeHole(ArithTypeVar("b")));
val c''' = THole(TypeHole(ArithTypeVar("c")));
val d''' = THole(TypeHole(ArithTypeVar("d")));

prettyPrintConfig(narrow(Concrete(N(3)),Int,[],[],[]));			(* 3, [], []    *)
prettyPrintConfig(narrow(Concrete(R(3.0)),Real,[],[],[]));		(* 3.0, [], []  *)
prettyPrintConfig(narrow(Concrete(N(3)),Real,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrow(Concrete(B(true)),Bool,[],[],[]));		(* true, [], [] *)
prettyPrintConfig(narrow(Concrete(B(false)),Int,[],[],[]));		(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),
	TRecord([(Lab("a"),Int),(Lab("b"),Int)]),[],[],[])); 			
(* {a=3,b=4}, [], [] *)
	
prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Bool)]),[],[],[])); 	
(* {a=3.0,b=true} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	TRecord([(Lab("a"),TFun(Int,Int))]),[],[],[])); 	
(* {a=fn x:int=>x+x} *)

prettyPrintConfig(narrow(VRecord([]),TRecord([]),[],[],[])); 	
(* { } *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Bool),(Lab("c"),Int)]),[],[],[])); 	
(* {a=3.0,b=true,c=2} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("c"),Int),(Lab("b"),Bool)]),[],[],[])); 	
(* {a=3.0,b=true,c=2} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("c"),Int),(Lab("d"),Bool)]),[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(Concrete(N(3)),a',[],[],[]));			(* 3, [], ['a->Int] 	 *)
prettyPrintConfig(narrow(Concrete(R(3.0)),a',[],[],[]));		(* 3.0, [], ['a->Real]   *)
prettyPrintConfig(narrow(Concrete(R(3.0)),a'',[],[],[]));		(* Stuck *)
prettyPrintConfig(narrow(Concrete(N(3)),a''',[],[],[]));		(* 3, [], ['''a->Int] 	 *)
prettyPrintConfig(narrow(Concrete(B(true)),a'',[],[],[]));		(* true, [], [''a->Bool] *)
prettyPrintConfig(narrow(Concrete(B(false)),a''',[],[],[]));	(* Stuck *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a',[],[],[])); 			
(* {a=3,b=4}, [], ['a->{a:'a0,b:'a1}, 'a0->int, 'a1->int] *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a',[],[],[])); 	
(* {a=3.0,b=true}, [], ['a3 -> bool, 'a2 -> real, 'a -> {a:'a2, b:'a3}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a',[],[],[])); 	
(* {a=fn x:int=>x+x}, [], 
   ['''a8 -> int, 'a5 -> '''a8, 'a6 -> int, 'a7 -> 'a5, 'a4 -> ('a6 -> 'a7), 'a -> {a:'a4}] *)

prettyPrintConfig(narrow(VRecord([]),a',[],[],[])); 	
(* { }, [], ['a->{}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a',[],[],[])); 	
(* {a=3.0,b=true,c=2}, [], ['a11 -> int, 'a10 -> bool, 'a9 -> real, 'a -> {a:'a9, b:'a10, c:'a11}] *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a'',[],[],[])); 			
(* {a=3,b=4}, [], [''a->{a:''a0,b:''a1}, ''a0->int, ''a1->int] *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a'',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a'',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(VRecord([]),a'',[],[],[])); 	
(* { }, [], [''a->{}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(N(4))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a'',[],[],[])); 	
(* {a=4,b=true,c=2}, [], [''a11 -> int, 'a'10 -> bool, ''a9 -> real, ''a -> {a:''a9, b:''a10, c:''a11}] *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a''',[],[],[])); 			
(* Stuck *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a''',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a''',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(VRecord([]),a''',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a''',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(2.0))),
			 (Lab("c"),Concrete(B(true))),
			 (Lab("b"),Concrete(N(2))),
			 (Lab("d"),VRecord([(Lab("1"),Concrete(N(2))),
								(Lab("2"),Fun(Var("x"),Int,BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(2))))))])),
			 (Lab("e"),va'),
			 (Lab("f"),VHole(CaseHole(vb',[(PVar(Var("x")),BoolExpr(EQ,Variable(Var("x")),Value(Concrete(B(true)))))]))),
			 (Lab("g"),VHole(CaseHole(VRecord([(Lab("one"),vc''),(Lab("two"),va''')]),
								   	  [(PRecord([(Lab("two"),PVar(Var("x"))),(Lab("one"),PVal(N(2)))]),
									    ArithExpr(PLUS,Variable(Var("x")),Value(vb''')))])))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Int),(Lab("c"),Bool),
			 (Lab("d"),TRecord([(Lab("1"),Int),(Lab("2"),TFun(Int,Bool))])),
			 (Lab("e"),a'),
			 (Lab("f"),Bool),
			 (Lab("g"),Int)]),
	[],[],[]));
(* {a=2.0,b=2,c=true,d={1=2,2=fn x:int=>x=2},e=v['a],
    f=case v['a20] of x->x=true, g=case {one=v[''c],two=1.0} of {two=x,one=2}->x+1},
	[v['''b] -> 1.0, v['''a] -> 1.0, v[''c] -> 2, v['a21] -> true, v['b] -> v['a21]]
	['''b -> real, '''a -> real, ''c -> int, 'a23 -> '''a, 'a24 -> ''c, 'a21 -> bool, 'b -> 'a21] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	TRecord([(Lab("a"),TRecord([(Lab("1"),Real),(Lab("2"),Int)])),
			 (Lab("b"),TRecord([(Lab("3"),TFun(Int,Int))])),
			 (Lab("c"),TRecord([(Lab("4"),TRecord([(Lab("one"),TRecord([]))]))]))]),[],[],[])); 	
(* {a={1=3.0,2=3}, b={3=fn x:int=>3}, c={4={one={}}}}, [], [] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a',[],[],[])); 	
(* {a={1=3.0,2=3}, b={3=fn x:int=>3}, c={4={one={}}}}, [],
   ['a35 -> {}, 'a34 -> {one:'a35}, 'a27 -> {4:'a34}, 'a31 -> int, 'a32 -> int, 'a33 -> 'a31, 
    'a30 -> ('a32 -> 'a33), 'a26 -> {3:'a30}, 'a29 -> int, 'a28 -> real, 'a25 -> {1:'a28, 2:'a29}, 'a -> {a:'a25, b:'a26, c:'a27}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a'',[],[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(B(true))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),va')])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a'',[],[],[])); 	
(* {a={1=true,2=3}, b={3=v[''a46]}, c={4={one={}}}}, [v['a]->v[''a46]],
   [''a48 -> {}, ''a47 -> {one:''a48}, ''a43 -> {4:''a47}, 'a -> ''a46, ''a42 -> {3:''a46},
    ''a45 -> int, ''a44 -> bool, ''a41 -> {1:''a44, 2:''a45}, ''a -> {a:''a41, b:''a42, c:''a43}] *)

prettyPrintConfig(narrow(va',Int,[],[],[])); 			(* 1, [v['a]->1],   ['a->Int]   *)
prettyPrintConfig(narrow(va'',Int,[],[],[]));			(* 1, [v[''a]->1],  [''a->Int]  *)
prettyPrintConfig(narrow(va''',Int,[],[],[]));			(* 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrow(va''',Bool,[],[],[]));			(* Stuck, [], [] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),Int),(Lab("b"),Int)]),[],[],[])); 
(* {a=1,b=1}, [v['a]->{a=1,b=1}], ['a49 -> int, 'a50 -> int, 'a -> {a:'a49, b:'a50}] *)

prettyPrintConfig(narrow(va',TRecord([]),[],[],[])); 
(* {}, [v['a]->{}], ['a -> {}] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),b'),(Lab("b"),c'),(Lab("c"),d')]),[],[],[])); 
(* {a=v['b],b=v['c],c=v['d]}, [v['a]->{a=v['b],b=v['c],c=v['d]}], 
   ['a51 -> 'b, 'a52 -> 'c, 'a53 -> 'd, 'a -> {a:'a51, b:'a52, c:'a53}] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),a'),(Lab("b"),c'),(Lab("c"),d')]),[],[],[])); 
(* Stuck *)

prettyPrintConfig(narrow(va''',TRecord([(Lab("a"),b'),(Lab("b"),c'),(Lab("c"),d')]),[],[],[])); 
(* Stuck *)

prettyPrintConfig(narrow(va''',b'',[],[],[]));		(* 1, [v['''a]->1], ['''a->Int,''b->Int] *)

(* FOR TESTING USE NARROWEXPR FIlE *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Real,[],[],[])); 	(* 3.0/5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Real,[],[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0)))]))),Real,[],[],[]));	
(* Stuck  *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),Real,[],[],[]));		(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va'')),Real,[],[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),Real,[],[],[]));					(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va''')),Real,[],[],[]));	(* 3.0/1.0, [ v['''a]->1.0], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),Real,[],[],[]));				(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)


prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[],[])); 	(* 3.0/5.0, [], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a'',[],[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(5.0))),Value(Concrete(R(5.0)))),a''',[],[],[]));	(* 5.0/5.0, [], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),a',[],[],[]));				(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),b',[],[],[]));				(* 1.0/5.0, [v['a]->1.0], ['a->Real, 'b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va'')),a',[],[],[]));				(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),a''',[],[],[]));							(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real, '''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),a''',[],[],[]));						(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Real,[],[],[])); 	(* 3.0+5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Real,[],[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Real,[],[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),Real,[],[],[]));	(* 1.0 - 5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(va'')),Real,[],[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),Real,[],[],[]));				(* 1.0 - 1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Real,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Int,[],[],[]));		(* 5 * 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Bool,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),Real,[],[],[]));				(* 1.0 * 1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[],[]));	(* 3.0*5.0, [], ['a->'''a0, '''a0->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),a''',[],[],[]));			(* 1.0 - 5.0, [v['a]->1.0], ['a->Real,'''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),a'',[],[],[]));				(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),a',[],[],[]));							(* v['''a0]-v['''a0], [v['''b]->v['''a0],v['a]->v['''a0]], ['''b->'''a0,'a->'''a0] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),a''',[],[],[]));				(* 5*1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a'',[],[],[]));							(* 1*1, [ v['''a]->1.0], ['''a->Int, ''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a''',[],[],[]));						(* v['''a] * v['''a], [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'),Value(va')),a''',[],[],[]));							(* v['''a] * v['''a], [v['a]->v['''a]], ['a->'''a] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va''),Value(va'')),a''',[],[],[]));							(* 1*1, [v[''a]->1], [''a->Int, '''a->Int] *)
		
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Bool,[],[],[])); 		(* 3.0 < 5.0, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[],[]));			(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Bool,[],[],[]));			(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),Bool,[],[],[]));				(* 1 >= 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(Concrete(R(3.0))),Value(va'')),Bool,[],[],[]));				(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[],[])); 		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a'',[],[],[]));		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a''',[],[],[]));	(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),a',[],[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),b',[],[],[]));			(* 1>=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),a',[],[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(Concrete(R(3.0))),Value(va'')),a'',[],[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(va'),Value(vb''')),Bool,[],[],[]));		
(* v['''b] <= v['''b], [v['a]->v['''b]], ['a->'''b] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(N(5))),Value(va''')),Bool,[],[],[]));			
(* 5 < 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(va'''),Value(va''')),Bool,[],[],[]));			
(* v['''a] > v['''a], [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Bool,[],[],[])); 		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Bool,[],[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),Bool,[],[],[]));			(* 1 = 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(va'')),Bool,[],[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a',[],[],[])); 	(* 3=5, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a'',[],[],[]));	(* 3=5, [], [''a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a''',[],[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),a',[],[],[]));				(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),b',[],[],[]));				(* 1=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(va''')),b'',[],[],[]));			(* 3=1, [v['''a]->1.0], [''b->Bool, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),Bool,[],[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),c',[],[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int, 'c->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb'')),a',[],[],[]));		
(* true = true, [ v['a]->v[true], v[''b]->true ], ['a->Bool, ''b->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(5))),Value(va''')),Bool,[],[],[]));		
(* 5 = 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'''),Value(va''')),Bool,[],[],[]));			
(* 1 = 1, [v['''a]->1], ['''a->int, ''a0->int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[],[]));
(* {a=5,b=true} = {a=5,b=true} *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))]))),a',[],[],[]));
(*  {a=5,b=true} = {a=5,b=true}, [], ['a->bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),a',[],[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))]))),Bool,[],[],[]));
(* {a={1=6,2=true},b={1=5,2=true}} = {a={1=6,2=true},b={1=5,2=true}}, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("3"),Concrete(B(true)))]))]))),Bool,[],[],[]));
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("2"),Concrete(B(true))),(Lab("1"),Concrete(N(5)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))]))),Bool,[],[],[]));
(* {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}} = {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}}, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("2"),Concrete(B(true))),(Lab("1"),Concrete(N(5)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("f"),VRecord([]))]))]))]))),Bool,[],[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(va')),Bool,[],[],[]));
(* {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}} = {a={1=1, 2=true}, b={1=1, 2=true}, c={c={d={}}}},
  [v['a] -> {a={1=1, 2=true}, b={1=1, 2=true}, c={c={d={}}}}],
  ['a193 -> int, 'a194 -> bool, 'a195 -> int, 'a196 -> bool, 'a198 -> {}, 'a197 -> {d:'a198}, 'a192 -> {c:'a197},
   'a191 -> {1:'a195, 2:'a196}, 'a190 -> {1:'a193, 2:'a194}, 'a -> {a:'a190, b:'a191, c:'a192}] *) 

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Record([(Lab("a"),Value(VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))),
			(Lab("b"),Condition(Value(va'),Value(Concrete(N(4))),Value(Concrete(N(5)))))]),
	Record([(Lab("a"),Value(VRecord([(Lab("1"),vb'),(Lab("2"),Concrete(B(true)))]))),
			(Lab("b"),Value(Concrete(N(3))))])),Bool,[],[],[]));
(* {a={1=5,2=true}, b=if true then 4 else 5} = {a={1=1,2=true},3}, [v['a]->true, v['b]->1], ['a->bool,'b->int] *)

prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	TRecord([(Lab("a"),TFun(Int,Int)),(Lab("b"),Bool)]),[],[],[]));
(* {a=if true then fn x:int => 1 else fn x:int=>1, 
    b=if true then true else true}, 
   [v['e]->true,v['d]->true,v['c]->fn x:int=>1,v['b]->fn x:int=>1,v['a]->true],
   ['e -> bool, 'd -> bool, 'a71 -> int, 'a72 -> int, 'c -> ('a71 -> 'a72), 
    'a69 -> int, 'a70 -> int, 'b -> ('a69 -> 'a70), 'a -> bool] *)
	
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	f',[],[],[]));
(* {a=if true then  v['a73] else v['a73], b=if true then  v['a74] else v['a74]}, 
   [v['e] -> v[''a76], v['d] -> v[''a76], v['c] -> v[''a75], v['b] -> v[''a75], v['a] -> true]
   ['e -> 'a74, 'd -> 'a74, 'c -> 'a73, 'b -> 'a73, 'a -> bool, 'f -> {a:'a73, b:'a74}] *)
	
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	a'',[],[],[]));
(* {a=if true then  v[''a75] else v[''a75], b=if true then  v[''a76] else v[''a76]}
   [v['e] -> v[''a76], v['d] -> v[''a76], v['c] -> v[''a75], v['b] -> v[''a75], v['a] -> true]
   ['e -> ''a76, 'd -> ''a76, 'c -> ''a75, 'b -> ''a75, 'a -> bool, ''a -> {a:''a75, b:''a76}] *)
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	a''',[],[],[]));	
(* Stuck *)

prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(Concrete(B(true))),Value(Concrete(B(false)))),
	Condition(Condition(Value(Concrete(B(true))),Value(Concrete(B(true))),Value(vb')),Value(vc'),Value(Concrete(N(3)))),
	Condition(Value(Concrete(B(true))),Value(Concrete(N(5))),Value(vd'))),Int,[],[],[]));
(*
if if true then true else false 
then if if true then true else true then 1 else 3
else if true then 5 else 1,
[v['a]->true,v['b]->true,v['c]->1,v['d]->1],
[a'->bool,'b->bool,'c->int,'d->int] 
*)

prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(Concrete(B(true))),Value(Concrete(B(false)))),
	Condition(Condition(Value(Concrete(B(true))),Value(Concrete(B(true))),Value(vb')),Value(vc'),Value(Concrete(N(3)))),
	Condition(Value(Concrete(B(true))),Value(Concrete(N(5))),Value(vd'))),a''',[],[],[]));
(*
if if true then true else false 
then if if true then true else true then v['''a] else 3
else if true then 5 else 1,
[v['d] -> 1, v['c] -> v['''a], v['b] -> true, v['a] -> true], 
['d -> int, '''a -> int, 'c -> '''a, 'b -> bool, 'a -> bool] *)

prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PWildcard,ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(3)))))]),Int,[],[],[]));
(* case 2 of _ => 2+3, [], ['a77->int] *)

prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),VRecord([]))])),
	[(PWildcard,ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))))]),Real,[],[],[]));
(* case {a=3,b={}} of _ -> 3.0/5.0, [], ['a80 -> {}, 'a79 -> int, 'a78 -> {a:'a79, b:'a80}] *)

prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),VRecord([]))])),
	[(PVar(Var("x")),BoolExpr(EQ,Variable(Var("x")),
								Value(VRecord([(Lab("a"),Concrete(N(5))),
											   (Lab("b"),VRecord([]))]))))]),Bool,[],[],[]));
(* case {a=3,b={}} of x -> x = {a=5,b={}}, [],
   ['a83 -> {}, 'a82 -> int, 'a81 -> {a:'a82, b:'a83}] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
	[(PVar(Var("x")),App(Variable(Var("x")),Value(Concrete(N(10)))))]),Int,[],[],[]));
(* case (fn x:int => x*x) of x -> (x) (10), [],
  ['''a87 -> int, 'a84 -> '''a87, 'a85 -> int, 'a86 -> 'a84, 'a83 -> ('a85 -> 'a86)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),Concrete(N(10)))])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	  App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0))))))]),Real,[],[],[]));
(* case {a=fn x:real => x/x, b=fn x:real => x*x, c=10} of {a=x, b=y, c=10} -> (x) ((y) (10.0)), []
   ['a91 -> int, '''a98 -> real, 'a95 -> '''a98, 'a96 -> real, 'a97 -> 'a95, 
    'a90 -> ('a96 -> 'a97), 'a92 -> real, 'a93 -> real, 'a94 -> 'a92, 'a89 -> ('a93 -> 'a94)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
	[(PVar(Var("x")),
	  Case(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
		   [(PVar(Var("y")),
			 App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(N(10))))))]))]),Int,[],[],[]));
(* case (fn x:int => x+x) of x -> case (fn x127:int => x127*x127) of y -> (x) ((y) (10)), [],
  ['''a111 -> int, 'a107 -> '''a111, 'a108 -> int, 'a109 -> 'a107, 'a106 -> ('a108 -> 'a109),
   '''a105 -> int, 'a102 -> '''a105, 'a103 -> int, 'a104 -> 'a102, 'a101 -> ('a103 -> 'a104)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(va')))),
	[(PVar(Var("x")),
	  Case(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(vb')))),
		   [(PVar(Var("y")),
			 App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(N(10))))))]))]),Int,[],[],[]));
(* case (fn x:int => x+1) of x -> case (fn x140:int => x140*1) of y -> (x) ((y) (10)),
  [v['b] -> 1, v['a] -> 1], 
  ['b -> int, '''a123 -> int, 'a119 -> '''a123, 'a120 -> int, 'a121 -> 'a119, 'a118 -> ('a120 -> 'a121),
   'a -> int, '''a117 -> int, 'a114 -> '''a117, 'a115 -> int, 'a116 -> 'a114, 'a113 -> ('a115 -> 'a116)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Value(va''')))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Value(vb'''),Variable(Var("x"))))),
				   (Lab("c"),va'')])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	  App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0))))))]),Real,[],[],[]));
(* case {a=fn x:real => x/1.0, b=fn x:real => 1.0*x, c=v[''a]} of {a=x, b=y, c=10} -> (x) ((y) (10.0)), []
   ['a91 -> int, '''a98 -> real, 'a95 -> '''a98, 'a96 -> real, 'a97 -> 'a95, 
    'a90 -> ('a96 -> 'a97), 'a92 -> real, 'a93 -> real, 'a94 -> 'a92, 'a89 -> ('a93 -> 'a94)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Value(va''')))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Value(vb'''),Variable(Var("x"))))),
				   (Lab("c"),va'')])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	  App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0))))))]),a''',[],[],[]));
(* case {a=fn x:real => x/1.0, b=fn x:real => 1.0*x, c=v[''a]} of {a=x, b=y, c=10} -> (x) ((y) (10.0)),
  [v[''a] -> 10, v['''b] -> 1.0, v['''a] -> 1.0],
  [''a -> int, 'a139 -> ''a, '''b -> real, '''a146 -> real, 'a143 -> '''a146, 'a144 -> real, 'a145 -> 'a143,
  'a138 -> ('a144 -> 'a145), '''a -> real, 'a140 -> real, 'a141 -> real, 'a142 -> 'a140, 'a137 -> ('a141 -> 'a142)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]),Int,[],[],[]));
(* case {a=v['a168], b=v['a169]} of {a=x, b=y} -> x + y,
  [v['a169] -> 1, v['a168] -> 1, v['b] -> v['a169], v['a] -> v['a168]],
  ['a169 -> int, 'a168 -> int, 'b -> 'a169, 'a -> 'a168] *)
  
prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]),c',[],[],[]));
(* case {a=v['a170], b=v['a171]} of {a=x, b=y} -> x + y,
  [v['a171] -> v['''a172], v['a170] -> v['''a172], v['b] -> v['a171], v['a] -> v['a170]],
  ['a171 -> '''a172, 'a170 -> '''a172, 'c -> '''a172, 'b -> 'a171, 'a -> 'a170] *)

prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb')),(Lab("c"),Value(vb'''))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
	  ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]),Real,[],[],[]));
(* case {a=v['a173], b=v['a174], c=v['''b]} of {a=x, b=y, c=z} -> x + y + z,
  [v['''b] -> 1.0, v['a174] -> 1.0, v['a173] -> 1.0, v['b] -> v['a174], v['a] -> v['a173]],
  ['''b -> real, 'a174 -> real, 'a173 -> real, 'a175 -> '''b, 'b -> 'a174, 'a -> 'a173] *)

prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))]),Int,[],[],[]));
(* Stuck *)
	
prettyPrintConfig(narrowExpr(Case(
	Value(vc'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))]),Real,[],[],[]));
(* case {a=v['a178], b=v['a179]} of {a=x, b=y} -> x / y,
  [v['a179] -> 1.0, v['a178] -> 1.0, v['c] -> {a=v['a178], b=v['a179]}],
  ['a179 -> real, 'a178 -> real, 'a180 -> 'a178, 'a181 -> 'a179, 'c -> {a:'a180, b:'a181}] *)

prettyPrintConfig(narrowExpr(Case(
	Value(vc'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  App(Variable(Var("x")),
		  App(Value(Fun(Var("x"),a''',ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2)))))),
			  ArithExpr(PLUS,Variable(Var("y")),Value(Concrete(N(5)))))))]),b',[],[],[]));
(* case {a=v['a182], b=v['a183]} of {a=x, b=y} -> (x) ((fn x190:int => x190 * 2) (y + 5)),
  [v['a183] -> 1, v['a182] -> fn x:int => v['b], v['c] -> {a=v['a182], b=v['a183]}],
  ['a187 -> int, 'a188 -> 'b, 'a182 -> ('a187 -> 'a188), 'a183 -> int, '''a -> int, 'a184 -> 'a182, 'a185 -> 'a183, 'c -> {a:'a184, b:'a185}] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PWildcard),(Lab("c"),PVal(N(1))),
			   (Lab("d"),PRecord([(Lab("1"),PVal(B(true))),(Lab("2"),PVar(Var("y")))]))]),
	  Condition(Variable(Var("x")),
				Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(2)))))),
				App(Variable(Var("y")),Value(Concrete(R(1.0))))))]),
	TFun(Int,Int),[],[],[]));
(* case {a=v['a191], b=v['a192], c=v['a193], d={1=v['a194], 2=v['a195]}} of {a=x, b=_, c=1, d={1=true, 2=y}} -> if x then  fn x202:int => x202 + 2 else (y) (1.0),
  [v['a195] -> fn x:real => fn x:int => 1, v['a191] -> true, v['a] -> {a=v['a191], b=v['a192], c=v['a193], d={1=v['a194], 2=v['a195]}}],
  ['a203 -> real, 'a205 -> int, 'a206 -> int, 'a204 -> ('a205 -> 'a206), 'a195 -> ('a203 -> 'a204), 'a191 -> bool, 'a194 -> bool, 'a193 -> int, 'a196 -> 'a191, 'a197 -> 'a192, 'a198 -> 'a193, 'a200 -> 'a194, 'a201 -> 'a195, 'a199 -> {1:'a200, 2:'a201}, 'a -> {a:'a196, b:'a197, c:'a198, d:'a199}] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Int,Int),
	[], [],[]));
(* fn x:int => 2*x, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	a',
	[], [],[]));
(* fn x:int => 2*x, [], 
  ['''a80 -> int, 'a77 -> '''a80, 'a78 -> int, 'a79 -> 'a77, 'a -> ('a78 -> 'a79)] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Int,Real),
	[], [],[]));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Real,Int),
	[], [],[]));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Real,ArithExpr(TIMES,Value(Concrete(R(2.0))),Variable(Var("x")))),
	TFun(Real,Real),
	[], [],[]));
(* fn x:real => 2.0 * x, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))),
	TFun(Int,TFun(Int,Int)),
	[],[],[]));
(* fn x:int => fn x0:int => x0+x0, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,
			   Value(Fun(Var("x"),Int,
					 Value(Fun(Var("x"),Int,Value(va''')))))))),
	TFun(Int,TFun(Int,TFun(Int,TFun(Int,Real)))),[],[],[]));
(* fn x:int => fn x211:int => fn x211212:int => fn x211212213:int => 1.0,
   [v['''a] -> 1.0], 
   ['''a -> real] *)
   
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,Int)),[],[],[]));
(* fn x:int => fn y:int => if true then  1 else 1,
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   [''a -> bool, 'b -> int, 'a -> int] *)
  
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,Real)),[],[],[]));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,a''')),[],[],[]));
(* fn x:int => fn y:int => if true then  1 else 1,
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   ['''a -> int, ''a -> bool, 'b -> int, 'a -> int] *)

 prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(b''',a''')),[],[],[]));
(* fn x:int => fn y:int => if true then  1 else 1, 
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   ['''b -> int, '''a -> int, ''a -> bool, 'b -> '''b, 'a -> int] *)
  
  prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(c''',TFun(b''',a''')),[],[],[]));
(* fn x:'''a => fn y:'''a => if true then  v['''a] else v['''a], 
   [v['b] -> v['''a], v['a] -> v['''a], v[''a] -> true], 
   ['''b -> '''a, '''c -> '''a, ''a -> bool, 'b -> '''b, 'a -> '''c] *)
   
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),a',[],[],[]));
(* Stuck *)

  prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),c',[],[],[]));
(* fn x:'a218 => fn y:'a218 => if true then  v['a218] else v['a218],
   [v['b] -> v['a218], v['a] -> v['a218], v[''a] -> true],
   ['b -> 'a218, 'a -> 'a218, ''a -> bool, 'a219 -> 'b, 'a220 -> 'a218,
    'a215 -> ('a219 -> 'a220), 'a216 -> 'a, 'a217 -> 'a215, 'c -> ('a216 -> 'a217)] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int),(Lab("c"),Bool)]),
	    BoolExpr(EQ,Variable(Var("x")),Value(VRecord([(Lab("b"),Concrete(N(2))),
													  (Lab("a"),Concrete(N(2))),
													  (Lab("c"),Concrete(B(true)))])))),
	TFun(TRecord([(Lab("a"),Int),(Lab("b"),Int),(Lab("c"),Bool)]),Bool),[],[],[]));
(* fn x:{a:int, b:int, c:bool} => x = {a=2, b=2, c=true} *)

 prettyPrintConfig(narrowExpr(App(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
	Value(Concrete(N(2)))),Int,[],[],[]));
(* (fn x:int => x+x) 2, [], [] *)
	
prettyPrintConfig(narrowExpr(App(App(
	Value(Fun(Var("x"),Real,
		Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))))),
	Value(Concrete(R(2.0)))),Value(Concrete(R(3.0)))),Real,[],[],[]));
(* ( fn x : real => fn y : real => x/y) 2.0 3.0, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(Concrete(B(true))),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(Concrete(N(4)))),Int,[],[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(Concrete(B(true))),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(Concrete(N(4)))),a',[],[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4 , [], ['a->int] *)
	
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(Concrete(N(4)))),Int,[],[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 4 , 
  [v['c] -> 1, v['b] -> 1, v['a] -> true],
  ['c -> int, 'b -> int, 'a -> bool] *)
  
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(va''')),Int,[],[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 1 , 
  [v['''a] -> 1, v['c] -> 1, v['b] -> 1, v['a] -> true],
  ['''a -> int, 'c -> int, 'b -> int, 'a -> bool] *)

prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PWildcard),(Lab("c"),PVal(N(2))),
			 (Lab("b"),PVar(Var("x"))),(Lab("d"),PVar(Var("z"))),(Lab("e"),PVar(Var("y")))]),
	  App(App(Variable(Var("x")),Variable(Var("y"))),Variable(Var("z"))))]),b',[],[],[]));
(* case {a=v['a222], c=v['a223], b=v['a224], d=v['a225], e=v['a226]} of {a=_, c=2, b=x, d=z, e=y} -> ((x) (y)) (z),
  [v['a225] -> v['a235], v['a224] -> fn x:'a226 => fn x:'a235 => v['a236], v['a] -> {a=v['a222], c=v['a223], b=v['a224], d=v['a225], e=v['a226]}],
  ['a225 -> 'a235, 'b -> 'a236, 'a232 -> ('a235 -> 'a236), 'a233 -> 'a226, 'a234 -> 'a232, 'a224 -> ('a233 -> 'a234), 'a223 -> int, 'a227 -> 'a222,
  'a229 -> 'a224, 'a228 -> 'a223, 'a230 -> 'a225, 'a231 -> 'a226, 'a -> {a:'a227, c:'a228, b:'a229, d:'a230, e:'a231}] *)
   
prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PWildcard),(Lab("c"),PVal(N(2))),
			  (Lab("b"),PVar(Var("x"))),(Lab("d"),PVar(Var("z"))),(Lab("e"),PVar(Var("y")))]),
	  App(Variable(Var("x")),App(Variable(Var("y")),Variable(Var("z")))))]),b',[],[],[]));
(* case {a=v['a237], c=v['a238], b=v['a239], d=v['a240], e=v['a241]} of {a=_, c=2, b=x, d=z, e=y} -> (x) ((y) (z)),
  [v['a241] -> fn x:'a240 => v['a247], v['a239] -> fn x:'a247 => v['b], v['a] -> {a=v['a237], c=v['a238], b=v['a239], d=v['a240], e=v['a241]}],
  ['a250 -> 'a247, 'a251 -> 'b, 'a239 -> ('a250 -> 'a251), 'a248 -> 'a240, 'a249 -> 'a247, 'a241 -> ('a248 -> 'a249), 'a238 -> int, 'a242 -> 'a237,
  'a244 -> 'a239, 'a243 -> 'a238, 'a245 -> 'a240, 'a246 -> 'a241, 'a -> {a:'a242, c:'a243, b:'a244, d:'a245, e:'a246}] *)
  
 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(N(2)),Value(Concrete(R(2.0)))),
	 (PWildcard,Value(Concrete(R(0.0))))]),Real,[],[],[]));
(* case 2 of 1 -> 1.0 | 2 -> 2.0 | _ -> 0.0 *)

 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(R(2.0)),Value(Concrete(R(2.0)))),
	 (PWildcard,Value(Concrete(R(0.0))))]),Real,[],[],[]));
(* Stuck *)

 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(N(2)),Value(Concrete(R(2.0)))),
	 (PWildcard,Value(Concrete(N(1))))]),Real,[],[],[]));
(* Stuck *)

 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(N(2)),Value(Concrete(R(2.0)))),
	 (PVar(Var("x")),Value(Concrete(R(3.0)))),
	 (PWildcard,Value(Concrete(R(0.0))))]),Real,[],[],[]));
(* case 2 of 1 -> 1.0 | 2 -> 2.0 | x -> 3.0 | _ -> 0.0 *)

 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(N(2)),Value(Concrete(R(2.0)))),
	 (PVar(Var("x")),Value(Concrete(R(3.0)))),
	 (PWildcard,Value(va'))]),Real,[],[],[]));
(* case 2 of 1 -> 1.0 | 2 -> 2.0 | x -> 3.0 | _ -> 1.0, [v['a]->1.0], ['a->real] *)

 prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	[(PVal(N(1)),Value(Concrete(R(1.0)))),
	 (PVal(N(2)),Value(Concrete(R(2.0)))),
	 (PVar(Var("x")),Value(Concrete(R(3.0)))),
	 (PRecord([(Lab("i"),PVal(N(2)))]),Value(Concrete(R(5.0)))),
	 (PWildcard,Value(va'))]),Real,[],[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(Value(Fun(Var("x"),Int,Case(
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
	[(PVar(Var("x")),App(Variable(Var("x")),ArithExpr(TIMES,Value(Concrete(N(10))),Value(Concrete(N(10)))))),
	 (PVar(Var("y")),App(Variable(Var("y")),ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(20)))))),
	 (PWildcard,Variable(Var("x")))]))),TFun(Int,Int),[],[],[]));
(* fn x:int => case (fn x:263:int => x263*x263) of x258 -> x258 (10*10)
												 | y    -> y (x*20)
												 | _    -> x *)

prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	[(PVal(B(false)),Value(Fun(Var("x"),Int,Value(Concrete(N(10)))))),
	 (PVal(B(true)), Value(Fun(Var("x"),Int,Value(Concrete(N(20)))))),
	 (PWildcard,     Value(Fun(Var("x"),b', Value(vc'))))]),TFun(d',Int),[],[],[]));
(* case v['a267] of false -> fn x:int => 10 
				  | true  -> fn x:int => 20 
				  | _     -> fn x:int => 1,
  [v['c] -> 1, v['a] -> v['a267]],
  ['c -> int, 'b -> int, 'd -> int, 'a267 -> bool, 'a -> 'a267] *)
  
prettyPrintConfig(narrowExpr(
Value(Fun(Var("x"),Int,
	  Value(Fun(Var("y"),Int,
			Case(Record([(Lab("i"),Variable(Var("x"))),(Lab("j"),Variable(Var("y")))]),
				 [(PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVal(N(1)))]),Value(Concrete(N(10)))),
				  (PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVar(Var("x")))]),
				   Case(Variable(Var("x")),
				        [(PVal(N(2)),Value(Concrete(N(20)))),
						 (PVar(Var("x")),Value(Concrete(N(0))))])),
				  (PVar(Var("x")),
				   Case(Variable(Var("y")),
					    [(PVal(N(2)),Value(Concrete(N(0)))),
						 (PWildcard,Value(Concrete(N(20))))])),
				  (PWildcard,Value(va'))]))))),TFun(Int,TFun(Int,Int)),[],[],[]));			   
(* fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10
				   | {i=1,j=x} -> case x of 2 -> 20 | x -> 0
				   | x -> case y of 2 -> 0 | _ -> 20
				   | _ -> v['a]
   =>
   fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10 
				   | {i=1,j=x268} -> case x268 of 2 -> 20 | x268272 -> 0
				   | x269 -> case y of 2 -> 0 | _ -> 20
				   | _ -> 1,
   [v['a]->1], ['a->int] *)
				   
prettyPrintConfig(narrowExpr(
Value(Fun(Var("x"),Int,
	  Value(Fun(Var("y"),Int,
			Case(Record([(Lab("i"),Variable(Var("x"))),(Lab("j"),Variable(Var("y")))]),
				 [(PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVal(N(1)))]),Value(Concrete(N(10)))),
				  (PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVar(Var("x")))]),
				   Case(Variable(Var("x")),
				        [(PVal(N(2)),Value(Concrete(N(20)))),
						 (PVar(Var("x")),Value(Concrete(N(0))))])),
				  (PVar(Var("x")),
				   Case(Variable(Var("y")),
					    [(PVal(N(2)),Value(Concrete(N(0)))),
						 (PWildcard,Value(Concrete(N(20))))])),
				  (PWildcard,Value(va'))]))))),b',[],[],[]));			   
(* fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10
				   | {i=1,j=x} -> case x of 2 -> 20 | x -> 0
				   | x -> case y of 2 -> 0 | _ -> 20
				   | _ -> v['a]
   =>
   fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10 
				   | {i=1,j=x268} -> case x268 of 2 -> 20 | x268272 -> 0
				   | x269 -> case y of 2 -> 0 | _ -> 20
				   | _ -> 1,
   [v['a]->1], ['a->int, 'b->(int->(int->int))] *)
   
prettyPrintConfig(narrowExpr(
Value(Fun(Var("x"),Int,
	  Value(Fun(Var("y"),Int,
			Case(Record([(Lab("i"),Variable(Var("x"))),(Lab("j"),Variable(Var("y")))]),
				 [(PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVal(N(1)))]),Value(Concrete(N(10)))),
				  (PRecord([(Lab("i"),PVal(N(1))),(Lab("j"),PVar(Var("x")))]),
				   Case(Variable(Var("x")),
				        [(PVal(N(2)),Value(Concrete(N(20)))),
						 (PVar(Var("x")),Value(Concrete(N(0))))])),
				  (PVar(Var("x")),
				   Case(Variable(Var("y")),
					    [(PVal(N(2)),Value(Concrete(N(0)))),
						 (PWildcard,Value(Concrete(N(20))))])),
				  (PWildcard,Value(va'))]))))),b',[],[],[]));			   
(* fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10
				   | {i=1,j=x} -> case x of 2 -> 20 | x -> 0
				   | x -> case y of 2 -> 0 | _ -> 20
				   | _ -> v['a]
   =>
   fn x:int => fn y:int =>
   case {i=x,j=y} of {i=1,j=1} -> 10 
				   | {i=1,j=x268} -> case x268 of 2 -> 20 | x268272 -> 0
				   | x269 -> case y of 2 -> 0 | _ -> 20
				   | _ -> 1,
   [v['a]->1], ['a->int, 'b->(int->(int->int))] *)
												 
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)  
