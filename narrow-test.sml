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

prettyPrintConfig(narrow(Concrete(N(3)),Int,[],[]));		(* 3, [], []    *)
prettyPrintConfig(narrow(Concrete(R(3.0)),Real,[],[]));		(* 3.0, [], []  *)
prettyPrintConfig(narrow(Concrete(N(3)),Real,[],[]));		(* Stuck *)
prettyPrintConfig(narrow(Concrete(B(true)),Bool,[],[]));	(* true, [], [] *)
prettyPrintConfig(narrow(Concrete(B(false)),Int,[],[]));	(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),
	TRecord([(Lab("a"),Int),(Lab("b"),Int)]),[],[])); 			
(* {a=3,b=4}, [], [] *)
	
prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Bool)]),[],[])); 	
(* {a=3.0,b=true} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	TRecord([(Lab("a"),TFun(Int,Int))]),[],[])); 	
(* {a=fn x:int=>x+x} *)

prettyPrintConfig(narrow(VRecord([]),TRecord([]),[],[])); 	
(* { } *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Bool),(Lab("c"),Int)]),[],[])); 	
(* {a=3.0,b=true,c=2} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("c"),Int),(Lab("b"),Bool)]),[],[])); 	
(* {a=3.0,b=true,c=2} *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	TRecord([(Lab("a"),Real),(Lab("c"),Int),(Lab("d"),Bool)]),[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(Concrete(N(3)),a',[],[]));			(* 3, [], ['a->Int] 	 *)
prettyPrintConfig(narrow(Concrete(R(3.0)),a',[],[]));		(* 3.0, [], ['a->Real]   *)
prettyPrintConfig(narrow(Concrete(R(3.0)),a'',[],[]));		(* Stuck *)
prettyPrintConfig(narrow(Concrete(N(3)),a''',[],[]));		(* 3, [], ['''a->Int] 	 *)
prettyPrintConfig(narrow(Concrete(B(true)),a'',[],[]));		(* true, [], [''a->Bool] *)
prettyPrintConfig(narrow(Concrete(B(false)),a''',[],[]));	(* Stuck *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a',[],[])); 			
(* {a=3,b=4}, [], ['a->{a:'a0,b:'a1}, 'a0->int, 'a1->int] *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a',[],[])); 	
(* {a=3.0,b=true}, [], ['a3 -> bool, 'a2 -> real, 'a -> {a:'a2, b:'a3}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a',[],[])); 	
(* {a=fn x:int=>x+x}, [], 
   ['''a8 -> int, 'a5 -> '''a8, 'a6 -> int, 'a7 -> 'a5, 'a4 -> ('a6 -> 'a7), 'a -> {a:'a4}] *)

prettyPrintConfig(narrow(VRecord([]),a',[],[])); 	
(* { }, [], ['a->{}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a',[],[])); 	
(* {a=3.0,b=true,c=2}, [], ['a11 -> int, 'a10 -> bool, 'a9 -> real, 'a -> {a:'a9, b:'a10, c:'a11}] *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a'',[],[])); 			
(* {a=3,b=4}, [], [''a->{a:''a0,b:''a1}, ''a0->int, ''a1->int] *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a'',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a'',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(VRecord([]),a'',[],[])); 	
(* { }, [], [''a->{}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(N(4))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a'',[],[])); 	
(* {a=4,b=true,c=2}, [], [''a11 -> int, 'a'10 -> bool, ''a9 -> real, ''a -> {a:''a9, b:''a10, c:''a11}] *)

prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]),a''',[],[])); 			
(* Stuck *)
	
prettyPrintConfig(narrow(VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true)))]),a''',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))]),
	a''',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(VRecord([]),a''',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(3.0))),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(2)))]),
	a''',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),Concrete(R(2.0))),
			 (Lab("c"),Concrete(B(true))),
			 (Lab("b"),Concrete(N(2))),
			 (Lab("d"),VRecord([(Lab("1"),Concrete(N(2))),
								(Lab("2"),Fun(Var("x"),Int,BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(2))))))])),
			 (Lab("e"),va'),
			 (Lab("f"),VHole(CaseHole(vb',PVar(Var("x")),BoolExpr(EQ,Variable(Var("x")),Value(Concrete(B(true))))))),
			 (Lab("g"),VHole(CaseHole(VRecord([(Lab("one"),vc''),(Lab("two"),va''')]),
								   	  PRecord([(Lab("two"),PVar(Var("x"))),(Lab("one"),PVal(N(2)))]),
									  ArithExpr(PLUS,Variable(Var("x")),Value(vb''')))))]),
	TRecord([(Lab("a"),Real),(Lab("b"),Int),(Lab("c"),Bool),
			 (Lab("d"),TRecord([(Lab("1"),Int),(Lab("2"),TFun(Int,Bool))])),
			 (Lab("e"),a'),
			 (Lab("f"),Bool),
			 (Lab("g"),Int)]),
	[],[]));
(* {a=2.0,b=2,c=true,d={1=2,2=fn x:int=>x=2},e=v['a],
    f=case true of x20->x20=true, g=case {one=v[''c],two=1.0} of {two=x22,one=2}->s22+1.0},
	[v['''b] -> 1.0, v['''a] -> 1.0, v[''c] -> 2, v['a21] -> true, v['b] -> v['a21]]
	['''b -> real, '''a -> real, ''c -> int, 'a23 -> '''a, 'a24 -> ''c, 'a21 -> bool, 'b -> 'a21] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	TRecord([(Lab("a"),TRecord([(Lab("1"),Real),(Lab("2"),Int)])),
			 (Lab("b"),TRecord([(Lab("3"),TFun(Int,Int))])),
			 (Lab("c"),TRecord([(Lab("4"),TRecord([(Lab("one"),TRecord([]))]))]))]),[],[])); 	
(* {a={1=3.0,2=3}, b={3=fn x:int=>3}, c={4={one={}}}}, [], [] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a',[],[])); 	
(* {a={1=3.0,2=3}, b={3=fn x:int=>3}, c={4={one={}}}}, [],
   ['a35 -> {}, 'a34 -> {one:'a35}, 'a27 -> {4:'a34}, 'a31 -> int, 'a32 -> int, 'a33 -> 'a31, 
    'a30 -> ('a32 -> 'a33), 'a26 -> {3:'a30}, 'a29 -> int, 'a28 -> real, 'a25 -> {1:'a28, 2:'a29}, 'a -> {a:'a25, b:'a26, c:'a27}] *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(R(3.0))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),Fun(Var("x"),Int,Value(Concrete(N(3)))))])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a'',[],[])); 	
(* Stuck *)

prettyPrintConfig(narrow(
	VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(B(true))),(Lab("2"),Concrete(N(3)))])),
	         (Lab("b"),VRecord([(Lab("3"),va')])),
			 (Lab("c"),VRecord([(Lab("4"),VRecord([(Lab("one"),VRecord([]))]))]))]),
	a'',[],[])); 	
(* {a={1=true,2=3}, b={3=v[''a46]}, c={4={one={}}}}, [v['a]->v[''a46]],
   [''a48 -> {}, ''a47 -> {one:''a48}, ''a43 -> {4:''a47}, 'a -> ''a46, ''a42 -> {3:''a46},
    ''a45 -> int, ''a44 -> bool, ''a41 -> {1:''a44, 2:''a45}, ''a -> {a:''a41, b:''a42, c:''a43}] *)

prettyPrintConfig(narrow(va',Int,[],[])); 			(* 1, [v['a]->1],   ['a->Int]   *)
prettyPrintConfig(narrow(va'',Int,[],[]));			(* 1, [v[''a]->1],  [''a->Int]  *)
prettyPrintConfig(narrow(va''',Int,[],[]));			(* 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrow(va''',Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),Int),(Lab("b"),Int)]),[],[])); 
(* {a=1,b=1}, [v['a]->{a=1,b=1}], ['a49 -> int, 'a50 -> int, 'a -> {a:'a49, b:'a50}] *)

prettyPrintConfig(narrow(va',TRecord([]),[],[])); 
(* {}, [v['a]->{}], ['a -> {}] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),b'),(Lab("b"),c'),(Lab("c"),d')]),[],[])); 
(* {a=v['b],b=v['c],c=v['d]}, [v['a]->{a=v['b],b=v['c],c=v['d]}], 
   ['a51 -> 'b, 'a52 -> 'c, 'a53 -> 'd, 'a -> {a:'a51, b:'a52, c:'a53}] *)

prettyPrintConfig(narrow(va',TRecord([(Lab("a"),a'),(Lab("b"),c'),(Lab("c"),d')]),[],[])); 
(* Stuck *)

prettyPrintConfig(narrow(va''',TRecord([(Lab("a"),b'),(Lab("b"),c'),(Lab("c"),d')]),[],[])); 
(* Stuck *)

prettyPrintConfig(narrow(va''',b'',[],[]));		(* 1, [v['''a]->1], ['''a->Int,''b->Int] *)

(* FOR TESTING USE NARROWEXPR FIlE *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Real,[],[])); 	(* 3.0/5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[]));		(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Real,[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0)))]))),Real,[],[]));	
(* Stuck  *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),Real,[],[]));		(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va'')),Real,[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),Real,[],[]));				(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va''')),Real,[],[]));	(* 3.0/1.0, [ v['''a]->1.0], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),Real,[],[]));				(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[])); 	(* 3.0/5.0, [], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a'',[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(5.0))),Value(Concrete(R(5.0)))),a''',[],[]));(* 5.0/5.0, [], ['''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),a',[],[]));				(* 1.0/5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(Concrete(R(5.0)))),b',[],[]));				(* 1.0/5.0, [v['a]->1.0], ['a->Real, 'b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(va'')),a',[],[]));				(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'),Value(vb''')),a''',[],[]));						(* 1.0/1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real, '''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(DIVIDE,Value(va'''),Value(va''')),a''',[],[]));						(* 1.0/1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Real,[],[])); 	(* 3.0+5.0, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Real,[],[]));	(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Real,[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),Real,[],[]));	(* 1.0 - 5.0, [v['a]->1.0], ['a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(va'')),Real,[],[]));	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),Real,[],[]));				(* 1.0 - 1.0, [ v['a]->1.0, v['''b]->1.0 ], ['a->Real, '''b->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Real,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Int,[],[]));		(* 5 * 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),Bool,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),Real,[],[]));				(* 1.0 * 1.0, [ v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[]));	(* 3.0*5.0, [], ['a->'''a0, '''a0->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),a''',[],[]));			(* 1.0 - 5.0, [v['a]->1.0], ['a->Real,'''a->Real] *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(Concrete(R(5.0)))),a'',[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(ArithExpr(SUBTRACT,Value(va'),Value(vb''')),a',[],[]));						(* v['''a0]-v['''a0], [v['''b]->v['''a0],v['a]->v['''a0]], ['''b->'''a0,'a->'''a0] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(Concrete(N(5))),Value(va''')),a''',[],[]));				(* 5*1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a'',[],[]));						(* 1*1, [ v['''a]->1.0], ['''a->Int, ''a->Int] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'''),Value(va''')),a''',[],[]));						(* v['''a] * v['''a], [], [] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va'),Value(va')),a''',[],[]));							(* v['''a] * v['''a], [v['a]->v['''a]], ['a->'''a] *)
prettyPrintConfig(narrowExpr(ArithExpr(TIMES,Value(va''),Value(va'')),a''',[],[]));							(* 1*1, [v[''a]->1], [''a->Int, '''a->Int] *)
		
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Bool,[],[])); 		(* 3.0 < 5.0, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Bool,[],[]));			(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),Bool,[],[]));			(* 1 >= 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(Concrete(R(3.0))),Value(va'')),Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a',[],[])); 		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a'',[],[]));		(* 3.0 < 5.0, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),a''',[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),a',[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),b',[],[]));			(* 1>=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(MORE_EQ,Value(va'),Value(Concrete(N(2)))),a',[],[]));			(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(Concrete(R(3.0))),Value(va'')),a'',[],[]));		(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS_EQ,Value(va'),Value(vb''')),Bool,[],[]));		
(* v['''b] <= v['''b], [v['a]->v['''a0]], ['a->'''b] *)

prettyPrintConfig(narrowExpr(BoolExpr(LESS,Value(Concrete(N(5))),Value(va''')),Bool,[],[]));			
(* 5 < 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(MORE,Value(va'''),Value(va''')),Bool,[],[]));			
(* v['''a] > v['''a], [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Bool,[],[])); 	(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),Int,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(R(5.0)))),Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[]));	
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),Bool,[],[]));		(* 1 = 2, [v['a]->1], ['a->Int] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(va'')),Bool,[],[]));		(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a',[],[])); 	(* 3=5, [], ['a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a'',[],[]));	(* 3=5, [], [''a->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),a''',[],[]));	(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),a',[],[]));				(* Stuck *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))),b',[],[]));				(* 1=2, [v['a]->1], ['a->Int,'b->Bool] *)
prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(va''')),b'',[],[]));			(* 3=1, [v['''a]->1.0], [''b->Bool, '''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),Bool,[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb''')),c',[],[]));		
(* v[''a0] = 1, [ v['a]->v[''a0], v['''b]->1 ], ['a->''a0, '''b->Int, ''a0->Int, 'c->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'),Value(vb'')),a',[],[]));		
(* true = true, [ v['a]->v[true], v[''b]->true ], ['a->Bool, ''b->Bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(Concrete(N(5))),Value(va''')),Bool,[],[]));		
(* 5 = 1, [ v['''a]->1 ], ['''a->Int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,Value(va'''),Value(va''')),Bool,[],[]));			
(* 1 = 1, [v['''a]->1], ['''a->int, ''a0->int] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[]));
(* {a=5,b=true} = {a=5,b=true} *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))]))),a',[],[]));
(*  {a=5,b=true} = {a=5,b=true}, [], ['a->bool] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),a',[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(true)))])),
	Value(VRecord([(Lab("a"),Concrete(R(5.0))),(Lab("b"),Concrete(B(true)))]))),Bool,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))]))),Bool,[],[]));
(* {a={1=6,2=true},b={1=5,2=true}} = {a={1=6,2=true},b={1=5,2=true}}, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("3"),Concrete(B(true)))]))]))),Bool,[],[]));
(* Stuck, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("2"),Concrete(B(true))),(Lab("1"),Concrete(N(5)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))]))),Bool,[],[]));
(* {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}} = {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}}, [], [] *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("2"),Concrete(B(true))),(Lab("1"),Concrete(N(5)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("f"),VRecord([]))]))]))]))),Bool,[],[]));
(* Stuck *)

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(6))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))])),
				   (Lab("c"),VRecord([(Lab("c"),VRecord([(Lab("d"),VRecord([]))]))]))])),
	Value(va')),Bool,[],[]));
(* {a={1=6, 2=true}, b={1=5, 2=true}, c={c={d={}}}} = {a={1=1, 2=true}, b={1=1, 2=true}, c={c={d={}}}},
  [v['a] -> {a={1=1, 2=true}, b={1=1, 2=true}, c={c={d={}}}}],
  ['a193 -> int, 'a194 -> bool, 'a195 -> int, 'a196 -> bool, 'a198 -> {}, 'a197 -> {d:'a198}, 'a192 -> {c:'a197},
   'a191 -> {1:'a195, 2:'a196}, 'a190 -> {1:'a193, 2:'a194}, 'a -> {a:'a190, b:'a191, c:'a192}] *) 

prettyPrintConfig(narrowExpr(BoolExpr(EQ,
	Record([(Lab("a"),Value(VRecord([(Lab("1"),Concrete(N(5))),(Lab("2"),Concrete(B(true)))]))),
			(Lab("b"),Condition(Value(va'),Value(Concrete(N(4))),Value(Concrete(N(5)))))]),
	Record([(Lab("a"),Value(VRecord([(Lab("1"),vb'),(Lab("2"),Concrete(B(true)))]))),
			(Lab("b"),Value(Concrete(N(3))))])),Bool,[],[]));
(* {a={1=5,2=true}, b=if true then 4 else 5} = {a={1=1,2=true},3}, [v['a]->true, v['b]->1], ['a->bool,'b->int] *)

prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	TRecord([(Lab("a"),TFun(Int,Int)),(Lab("b"),Bool)]),[],[]));
(* {a=if true then fn x:int => 1 else fn x:int=>1, 
    b=if true then true else true}, 
   [v['e]->true,v['d]->true,v['c]->fn x:int=>1,v['b]->fn x:int=>1,v['a]->true],
   ['e -> bool, 'd -> bool, 'a71 -> int, 'a72 -> int, 'c -> ('a71 -> 'a72), 
    'a69 -> int, 'a70 -> int, 'b -> ('a69 -> 'a70), 'a -> bool] *)
	
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	f',[],[]));
(* {a=if true then  v['a73] else v['a73], b=if true then  v['a74] else v['a74]}, 
   [v['e] -> v[''a76], v['d] -> v[''a76], v['c] -> v[''a75], v['b] -> v[''a75], v['a] -> true]
   ['e -> 'a74, 'd -> 'a74, 'c -> 'a73, 'b -> 'a73, 'a -> bool, 'f -> {a:'a73, b:'a74}] *)
	
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	a'',[],[]));
(* {a=if true then  v[''a75] else v[''a75], b=if true then  v[''a76] else v[''a76]}
   [v['e] -> v[''a76], v['d] -> v[''a76], v['c] -> v[''a75], v['b] -> v[''a75], v['a] -> true]
   ['e -> ''a76, 'd -> ''a76, 'c -> ''a75, 'b -> ''a75, 'a -> bool, ''a -> {a:''a75, b:''a76}] *)
prettyPrintConfig(narrowExpr(
	Record([(Lab("a"),Condition(Value(va'),Value(vb'),Value(vc'))),(Lab("b"),Condition(Value(Concrete(B(true))),Value(vd'),Value(ve')))]),
	a''',[],[]));	
(* Stuck *)

prettyPrintConfig(narrowExpr(Condition(
	Condition(Value(va'),Value(Concrete(B(true))),Value(Concrete(B(false)))),
	Condition(Condition(Value(Concrete(B(true))),Value(Concrete(B(true))),Value(vb')),Value(vc'),Value(Concrete(N(3)))),
	Condition(Value(Concrete(B(true))),Value(Concrete(N(5))),Value(vd'))),Int,[],[]));
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
	Condition(Value(Concrete(B(true))),Value(Concrete(N(5))),Value(vd'))),a''',[],[]));
(*
if if true then true else false 
then if if true then true else true then v['''a] else 3
else if true then 5 else 1,
[v['d] -> 1, v['c] -> v['''a], v['b] -> true, v['a] -> true], 
['d -> int, '''a -> int, 'c -> '''a, 'b -> bool, 'a -> bool] *)

prettyPrintConfig(narrowExpr(Case(
	Value(Concrete(N(2))),
	PWildcard,
	ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(3))))),Int,[],[]));
(* case 2 of _ => 2+3, [], ['a77->int] *)

prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),VRecord([]))])),
	PWildcard,
	ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0))))),Real,[],[]));
(* case {a=3,b={}} of _ -> 3.0/5.0, [], ['a80 -> {}, 'a79 -> int, 'a78 -> {a:'a79, b:'a80}] *)

prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),VRecord([]))])),
	PVar(Var("x")),
	BoolExpr(EQ,Variable(Var("x")),
				Value(VRecord([(Lab("a"),Concrete(N(5))),
							   (Lab("b"),VRecord([]))])))),Bool,[],[]));
(* case {a=3,b={}} of x -> x = {a=5,b={}}, [],
   ['a83 -> {}, 'a82 -> int, 'a81 -> {a:'a82, b:'a83}] *)
   
prettyPrintConfig(narrowExpr(Case(
	Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
	PVar(Var("x")),
	App(Variable(Var("x")),Value(Concrete(N(10))))),Int,[],[]));
(* case fn x88:int => x88 * x88 of x -> (x) (10), [],
  ['''a87 -> int, 'a84 -> '''a87, 'a85 -> int, 'a86 -> 'a84, 'a83 -> ('a85 -> 'a86)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),Concrete(N(10)))])),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0)))))),Real,[],[]));
(* case {a=fn x99:real => x99 / x99, b=fn x100:real => x100 * x100, c=10} of {a=x, b=y, c=10} -> (x) ((y) (10.0)), []
   ['a91 -> int, '''a98 -> real, 'a95 -> '''a98, 'a96 -> real, 'a97 -> 'a95, 
    'a90 -> ('a96 -> 'a97), 'a92 -> real, 'a93 -> real, 'a94 -> 'a92, 'a89 -> ('a93 -> 'a94)] *)
  
 prettyPrintConfig(narrowExpr(
	Case(Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
		 PVar(Var("x")),
		 Case(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
			  PVar(Var("y")),
			  App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(N(10))))))),Int,[],[]));
(* case fn x112:int => x112 + x112 of x -> case fn x110:int => x110 * x110 of y -> (x) ((y) (10)), [],
  ['''a111 -> int, 'a107 -> '''a111, 'a108 -> int, 'a109 -> 'a107, 'a106 -> ('a108 -> 'a109),
   '''a105 -> int, 'a102 -> '''a105, 'a103 -> int, 'a104 -> 'a102, 'a101 -> ('a103 -> 'a104)] *)
  
  prettyPrintConfig(narrowExpr(
	Case(Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(va')))),
		 PVar(Var("x")),
		 Case(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(vb')))),
			  PVar(Var("y")),
			  App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(N(10))))))),Int,[],[]));
(* case (fn x124:int => x124 + 1) of x -> case (fn x122:int => x122 * 1) of y -> (x) ((y) (10)),
  [v['b] -> 1, v['a] -> 1], 
  ['b -> int, '''a123 -> int, 'a119 -> '''a123, 'a120 -> int, 'a121 -> 'a119, 'a118 -> ('a120 -> 'a121),
   'a -> int, '''a117 -> int, 'a114 -> '''a117, 'a115 -> int, 'a116 -> 'a114, 'a113 -> ('a115 -> 'a116)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Value(va''')))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Value(vb'''),Variable(Var("x"))))),
				   (Lab("c"),va'')])),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0)))))),Real,[],[]));
(* case {a=fn x99:real => x99 / x99, b=fn x100:real => x100 * x100, c=10} of {a=x, b=y, c=10} -> (x) ((y) (10.0)), []
   ['a91 -> int, '''a98 -> real, 'a95 -> '''a98, 'a96 -> real, 'a97 -> 'a95, 
    'a90 -> ('a96 -> 'a97), 'a92 -> real, 'a93 -> real, 'a94 -> 'a92, 'a89 -> ('a93 -> 'a94)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Value(VRecord([(Lab("a"),Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Value(va''')))),
				   (Lab("b"),Fun(Var("x"),Real, ArithExpr(TIMES,Value(vb'''),Variable(Var("x"))))),
				   (Lab("c"),va'')])),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(N(10)))]),
	App(Variable(Var("x")),App(Variable(Var("y")),Value(Concrete(R(10.0)))))),a''',[],[]));
(* case {a=fn x147:real => x147 / 1.0, b=fn x148:real => 1.0 * x148, c=v[''a]} of {a=x, b=y, c=10} -> (x) ((y) (10.0)),
  [v[''a] -> 10, v['''b] -> 1.0, v['''a] -> 1.0],
  [''a -> int, 'a139 -> ''a, '''b -> real, '''a146 -> real, 'a143 -> '''a146, 'a144 -> real, 'a145 -> 'a143,
  'a138 -> ('a144 -> 'a145), '''a -> real, 'a140 -> real, 'a141 -> real, 'a142 -> 'a140, 'a137 -> ('a141 -> 'a142)] *)
  
prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* case {a=1, b=1} of {a=x, b=y} -> x + y,
  [v['a150] -> 1, v['a149] -> 1, v['b] -> v['a150], v['a] -> v['a149]],
  ['a150 -> int, 'a149 -> int, 'b -> 'a150, 'a -> 'a149] *)
  
prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),c',[],[]));
(* {a=v['''a153], b=v['''a153]} of {a=x, b=y} -> x + y,
  [v['a152] -> v['''a153], v['a151] -> v['''a153], v['b] -> v['a152], v['a] -> v['a151]],
  ['a152 -> '''a153, 'a151 -> '''a153, 'c -> '''a153, 'b -> 'a152, 'a -> 'a151] *)

prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb')),(Lab("c"),Value(vb'''))]),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
	ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z"))))),Real,[],[]));
(* case {a=1.0, b=1.0, c=1.0} of {a=x, b=y, c=z} -> x + y + z,
  [v['''b] -> 1.0, v['a155] -> 1.0, v['a154] -> 1.0, v['b] -> v['a155], v['a] -> v['a154]],
  ['''b -> real, 'a155 -> real, 'a154 -> real, 'a156 -> '''b, 'b -> 'a155, 'a -> 'a154] *)

prettyPrintConfig(narrowExpr(Case(
	Record([(Lab("a"),Value(va')),(Lab("b"),Value(vb'))]),
	PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Int,[],[]));
(* Stuck *)
	
prettyPrintConfig(narrowExpr(
	Case(Value(vc'),
		 PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
		 ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),Real,[],[]));
(* case {a=1.0, b=1.0} of {a=x, b=y} -> x / y,
  [v['a160] -> 1.0, v['a159] -> 1.0, v['c] -> {a=v['a159], b=v['a160]}],
  ['a160 -> real, 'a159 -> real, 'a161 -> 'a159, 'a162 -> 'a160, 'c -> {a:'a161, b:'a162}] *)

prettyPrintConfig(narrowExpr(
	Case(Value(vc'),
		 PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
		 App(Variable(Var("x")),
			 App(Value(Fun(Var("x"),a''',ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2)))))),
				 ArithExpr(PLUS,Variable(Var("y")),Value(Concrete(N(5))))))),b',[],[]));
(* case {a=fn x:int=>v['b],b=1} of {a=x,b=y} -> (x) ((fn x171:int => x171 * 2) (y + 5)),
  [v['a164] -> 1, v['a163] -> fn x:int => v['b], v['c] -> {a=v['a163], b=v['a164]}],
  ['a168 -> int, 'a169 -> 'b, 'a163 -> ('a168 -> 'a169), 'a164 -> int, '''a -> int, 'a165 -> 'a163,
   'a166 -> 'a164, 'c -> {a:'a165, b:'a166}] *)
  
prettyPrintConfig(narrowExpr(
	Case(Value(va'),
		 PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PWildcard),(Lab("c"),PVal(N(1))),
				  (Lab("d"),PRecord([(Lab("1"),PVal(B(true))),(Lab("2"),PVar(Var("y")))]))]),
		 Condition(Variable(Var("x")),
				   Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(2)))))),
				   App(Variable(Var("y")),Value(Concrete(R(1.0)))))),
	TFun(Int,Int),[],[]));
(* case {a=true, b=v['a191], c=v['a192], d={1=v['a193], 2=fn x:real => fn x:int => 1}} of {a=x, b=_, c=1, d={1=true, 2=y}} -> if x then  fn x201:int => x201 + 2 else (y) (1.0), 
 [v['a194] -> fn x:real => fn x:int => 1, v['a190] -> true, v['a193] -> true, v['a192] -> 1, v['a] -> {a=v['a190], b=v['a191], c=v['a192], d={1=v['a193], 2=v['a194]}}],
 ['a202 -> real, 'a204 -> int, 'a205 -> int, 'a203 -> ('a204 -> 'a205), 'a194 -> ('a202 -> 'a203), 'a190 -> bool, 'a193 -> bool, 'a192 -> int,
  'a195 -> 'a190, 'a196 -> 'a191, 'a197 -> 'a192, 'a199 -> 'a193, 'a200 -> 'a194, 'a198 -> {1:'a199, 2:'a200}, 'a -> {a:'a195, b:'a196, c:'a197, d:'a198}] *)
  
  
prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Int,Int),
	[], []));
(* fn x:int => 2*x, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	a',
	[], []));
(* fn x:int => 2*x, [], 
  ['''a80 -> int, 'a77 -> '''a80, 'a78 -> int, 'a79 -> 'a77, 'a -> ('a78 -> 'a79)] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Int,Real),
	[], []));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))),
	TFun(Real,Int),
	[], []));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Real,ArithExpr(TIMES,Value(Concrete(R(2.0))),Variable(Var("x")))),
	TFun(Real,Real),
	[], []));
(* fn x:real => 2.0 * x, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))),
	TFun(Int,TFun(Int,Int)),
	[],[]));
(* fn x:int => fn x0:int => x0+x0, [], [] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,
			   Value(Fun(Var("x"),Int,
					 Value(Fun(Var("x"),Int,Value(va''')))))))),
	TFun(Int,TFun(Int,TFun(Int,TFun(Int,Real)))),[],[]));
(* fn x:int => fn x211:int => fn x211212:int => fn x211212213:int => 1.0,
   [v['''a] -> 1.0], 
   ['''a -> real] *)
   
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,Int)),[],[]));
(* fn x:int => fn y:int => if true then  1 else 1,
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   [''a -> bool, 'b -> int, 'a -> int] *)
  
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,Real)),[],[]));
(* Stuck *)

prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(Int,a''')),[],[]));
(* fn x:int => fn y:int => if true then  1 else 1,
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   ['''a -> int, ''a -> bool, 'b -> int, 'a -> int] *)

 prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(Int,TFun(b''',a''')),[],[]));
(* fn x:int => fn y:int => if true then  1 else 1, 
   [v['b] -> 1, v['a] -> 1, v[''a] -> true], 
   ['''b -> int, '''a -> int, ''a -> bool, 'b -> '''b, 'a -> int] *)
  
  prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),TFun(c''',TFun(b''',a''')),[],[]));
(* fn x:'''a => fn y:'''a => if true then  v['''a] else v['''a], 
   [v['b] -> v['''a], v['a] -> v['''a], v[''a] -> true], 
   ['''b -> '''a, '''c -> '''a, ''a -> bool, 'b -> '''b, 'a -> '''c] *)
   
prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),a',[],[]));
(* Stuck *)

  prettyPrintConfig(narrow(
	Fun(Var("x"),a',
		 Value(Fun(Var("y"),b',
			   Condition(Value(va''),
						 Value(va'),
						 Value(vb'))))),c',[],[]));
(* fn x:'a218 => fn y:'a218 => if true then  v['a218] else v['a218],
   [v['b] -> v['a218], v['a] -> v['a218], v[''a] -> true],
   ['b -> 'a218, 'a -> 'a218, ''a -> bool, 'a219 -> 'b, 'a220 -> 'a218,
    'a215 -> ('a219 -> 'a220), 'a216 -> 'a, 'a217 -> 'a215, 'c -> ('a216 -> 'a217)] *)

prettyPrintConfig(narrow(
	Fun(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int),(Lab("c"),Bool)]),
	    BoolExpr(EQ,Variable(Var("x")),Value(VRecord([(Lab("b"),Concrete(N(2))),
													  (Lab("a"),Concrete(N(2))),
													  (Lab("c"),Concrete(B(true)))])))),
	TFun(TRecord([(Lab("a"),Int),(Lab("b"),Int),(Lab("c"),Bool)]),Bool),[],[]));
(* fn x:{a:int, b:int, c:bool} => x = {a=2, b=2, c=true} *)

 prettyPrintConfig(narrowExpr(App(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
	Value(Concrete(N(2)))),Int,[],[]));
(* (fn x:int => x+x) 2, [], [] *)
	
prettyPrintConfig(narrowExpr(App(App(
	Value(Fun(Var("x"),Real,
		Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))))),
	Value(Concrete(R(2.0)))),Value(Concrete(R(3.0)))),Real,[],[]));
(* ( fn x : real => fn y : real => x/y) 2.0 3.0, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(Concrete(B(true))),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(Concrete(N(4)))),Int,[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4, [], [] *)

prettyPrintConfig(narrowExpr(App(
	Condition(Value(Concrete(B(true))),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y")))))),
	Value(Concrete(N(4)))),a',[],[]));
(* (if true then (fx:int => x+x) else (fn y:int => y*y)) 4 , [], ['a->int] *)
	
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(Concrete(N(4)))),Int,[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 4 , 
  [v['c] -> 1, v['b] -> 1, v['a] -> true],
  ['c -> int, 'b -> int, 'a -> bool] *)
  
prettyPrintConfig(narrowExpr(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Int,ArithExpr(PLUS,Value(vb'),Variable(Var("x"))))),
			  Value(Fun(Var("y"),Int,ArithExpr(TIMES,Value(vc'),Variable(Var("y")))))),
	Value(va''')),Int,[],[]));
(* (if true then (fx:int => 1+x) else (fn y:int => 1*y)) 1 , 
  [v['''a] -> 1, v['c] -> 1, v['b] -> 1, v['a] -> true],
  ['''a -> int, 'c -> int, 'b -> int, 'a -> bool] *)

prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	PRecord([(Lab("a"),PWildcard),(Lab("c"),PVal(N(2))),
			 (Lab("b"),PVar(Var("x"))),(Lab("d"),PVar(Var("z"))),(Lab("e"),PVar(Var("y")))]),
	App(App(Variable(Var("x")),Variable(Var("y"))),Variable(Var("z")))),b',[],[]));
(* case {a=v['a221], b=fn x:'a225 => fn x:'a234 => v['a235], c=v['a222], d=v['a234], e=v['a225]} of {a=_, c=2, b=x, d=z, e=y} -> ((x) (y)) (z)\,
  [v['a224] -> v['a234], v['a223] -> fn x:'a225 => fn x:'a234 => v['a235], v['a222] -> 2, v['a] -> {a=v['a221], c=v['a222], b=v['a223], d=v['a224], e=v['a225]}],
  ['a224 -> 'a234, 'b -> 'a235, 'a231 -> ('a234 -> 'a235), 'a232 -> 'a225, 'a233 -> 'a231, 'a223 -> ('a232 -> 'a233), 'a222 -> int, 'a226 -> 'a221,
   'a228 -> 'a223, 'a227 -> 'a222, 'a229 -> 'a224, 'a230 -> 'a225, 'a -> {a:'a226, c:'a227, b:'a228, d:'a229, e:'a230}] *)
   
prettyPrintConfig(narrowExpr(Case(
	Value(va'),
	PRecord([(Lab("a"),PWildcard),(Lab("c"),PVal(N(2))),
			 (Lab("b"),PVar(Var("x"))),(Lab("d"),PVar(Var("z"))),(Lab("e"),PVar(Var("y")))]),
	App(Variable(Var("x")),App(Variable(Var("y")),Variable(Var("z"))))),b',[],[]));
(* case {a=v['a236], b=fn x:'a246 => v['b], c=v['a237], d=v['a239], e=fn x:'a239 => v['a246]} of {a=_, c=2, b=x, d=z, e=y} -> (x) ((y) (z)),
  [v['a240] -> fn x:'a239 => v['a246], v['a238] -> fn x:'a246 => v['b], v['a237] -> 2, v['a] -> {a=v['a236], c=v['a237], b=v['a238], d=v['a239], e=v['a240]}],
  ['a249 -> 'a246, 'a250 -> 'b, 'a238 -> ('a249 -> 'a250), 'a247 -> 'a239, 'a248 -> 'a246, 'a240 -> ('a247 -> 'a248),
   'a237 -> int, 'a241 -> 'a236, 'a243 -> 'a238, 'a242 -> 'a237, 'a244 -> 'a239, 'a245 -> 'a240, 'a -> {a:'a241, c:'a242, b:'a243, d:'a244, e:'a245}] *)