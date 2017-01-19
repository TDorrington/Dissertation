(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR TYPEOF *)

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
val e' = THole(TypeHole(TypeVar("e")));

val a'' = THole(TypeHole(EqualityTypeVar("a")));
val b'' = THole(TypeHole(EqualityTypeVar("b")));
val b''1 = TypeHole(EqualityTypeVar("b"));
val c'' = THole(TypeHole(EqualityTypeVar("c")));
val d'' = THole(TypeHole(EqualityTypeVar("d")));

val a''' = THole(TypeHole(ArithTypeVar("a")));
val b''' = THole(TypeHole(ArithTypeVar("b")));
val c''' = THole(TypeHole(ArithTypeVar("c")));
val d''' = THole(TypeHole(ArithTypeVar("d")));
val e''' = THole(TypeHole(ArithTypeVar("e")));

fun prettyPrintTypeOf(NONE,_) = "FAIL"
|	prettyPrintTypeOf(SOME t,theta) = 
	prettyPrintType(t) ^ ", theta = [ " ^ prettyPrintTheta(theta) ^ " ]";

prettyPrintTypeOf(typeof(Concrete(N(3)),[]));		(* Int *)
prettyPrintTypeOf(typeof(Concrete(B(true)),[]));	(* Bool *)
prettyPrintTypeOf(typeof(Concrete(R(3.0)),[]));		(* Real *)

prettyPrintTypeOf(typeof(VRecord([(Lab("i"),Concrete(N(3)))]),[]));			
(* {i:int} *)

prettyPrintTypeOf(typeof(VRecord(
	[(Lab("i"),Concrete(N(3))),(Lab("j"),Concrete(R(2.0))),(Lab("k"),Concrete(B(true)))]),[]));	
(* {i:int,j:real,k:bool} *)

prettyPrintTypeOf(typeof(VRecord([]),[])); (* {} *)

prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(TypeVar("a")))),[]));			(* 'a   *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(EqualityTypeVar("a")))),[]));	(* ''a  *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(ArithTypeVar("a")))),[]));		(* '''a *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(PLUS),va',vb')),[]));	 		(* '''a0, [  'a->'''a0,   'b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(TIMES),va''',vb''')),[]));	(* '''a0, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(SUBTRACT),va''',vb'')),[]));	(* Int,   ['''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),va',vb')),[])); 		(* Real,  [  'a->Real,    'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),va'',vb')),[]));		(* FAIL 							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),va''',vb')),[]));		(* Real,  [ '''a->Real,   'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),va''',vb'')),[]));	(* FAIL  							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(PLUS),va''',va''')),[]));		(* '''a,  [ ]						  *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),va',vb')),[]));			(* Bool, [  'a->''a0,    'b->''a0 ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),va''',vb''')),[]));		(* Bool, ['''a->Int,   '''b->Int  ]	 *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),va'',vb''')),[]));			(* Bool, ['''a->Int,   '''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(LESS),va',vb')),[]));			(* Bool, [  'a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(MORE),va'',vb''')),[]));		(* Bool, [ ''a->Int,  '''b->Int   ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(LESS_EQ),va''',vb''')),[]));	(* Bool, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(MORE_EQ),va'',vb'')),[]));		(* Bool, [ ''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(MORE_EQ),va'',va'')),[]));		(* Bool, [ ''a->Int ]  				 *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(LESS),
	VHole(BinaryOpHole(ArithOper(PLUS),va',vb')),
	VHole(BinaryOpHole(ArithOper(SUBTRACT),va',vb')))),[])); 
(* 
   v[ ('a+'b) < ('a-'b) ]
   =>
   Bool, ['a->'''a0, 'b->'''a0] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(TIMES),
	VHole(BinaryOpHole(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOpHole(ArithOper(PLUS),va',vb')))),[]));
(* 
   v[ ('a/'b) * ('a+'b) ]
   =>
   Real, ['a->Real, 'b->Real] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(LESS),
	VHole(BinaryOpHole(ArithOper(PLUS),va',vb')),
	VHole(BinaryOpHole(ArithOper(SUBTRACT),vc',vd')))),[])); 
(* 
   v[ ('a+'b) < ('c-'d) ]
   =>
   Bool, ['a->'''a0, 'b->'''a0, 'c->'''a1, 'd->'''a1, '''a0->'''a2, '''a1->'''a2] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(TIMES),
	VHole(BinaryOpHole(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOpHole(ArithOper(PLUS),vc',vd')))),[]));
(* 
   v[ ('a/'b) * ('c+'d) ]
   =>
   Real, ['a->Real, 'b->Real, 'c->a0, 'd->a0, 'a0->Real] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(TIMES),
	VHole(BinaryOpHole(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOpHole(BoolOper(EQ),va'',vb'')))),[]));
(* 
   v[ ('a/'b) * (''a=''b) ]
   =>
   FAIL 
*)	

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),
	VHole(BinaryOpHole(ArithOper(PLUS),va''',vb'')),
	VHole(BinaryOpHole(ArithOper(TIMES),va',vb')))),[]));
(* 
   v[ ('''a+''b) / ('a*'b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(DIVIDE),
	VHole(BinaryOpHole(ArithOper(PLUS),va''',vb'')),
	VHole(BinaryOpHole(ArithOper(TIMES),va''',vb'')))),[]));
(* 
   v[ ('''a+''b) / ('''a*''b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(ArithOper(PLUS),
	VHole(BinaryOpHole(ArithOper(PLUS),
		VHole(BinaryOpHole(ArithOper(PLUS),va',va')),
		VHole(BinaryOpHole(ArithOper(PLUS),va',va')))),
	VHole(BinaryOpHole(ArithOper(PLUS),
		VHole(BinaryOpHole(ArithOper(PLUS),va',va')),
		VHole(BinaryOpHole(ArithOper(DIVIDE),vb',vb')))))),[]));
(*
   v[ ( ('a+'a) + ('a+'a) ) + ( ('a+'a) + ('b/'b) ) ]
   =>
   Real, ['a->'''a0, '''a0->Real, 'b->Real]
*)

prettyPrintTypeOf(typeof(VHole(RecordHole([(Lab("i"),va'),(Lab("j"),vb')])),[]));
(* {i:'a, j:'b} *)

prettyPrintTypeOf(typeof(VHole(RecordHole([
	(Lab("a"),VHole(RecordHole([(Lab("i"),va'),(Lab("j"),vb')]))),
	(Lab("b"),VHole(RecordHole([(Lab("i"),va'),(Lab("j"),vb')])))])),[]));
(* {a:{i:'a,j:'b}, b:{i:'a,j:'b}} *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),Concrete(N(2))),(Lab("j"),vb')])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("j"),Concrete(B(false)))])))),[]));
(* Bool, ['b->Bool, 'a->Int] *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),Concrete(N(2))),(Lab("j"),vb')])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("k"),Concrete(B(false)))])))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),Concrete(N(2))),(Lab("j"),vb'),(Lab("k"),vc')])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("j"),Concrete(B(false)))])))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("j"),Concrete(B(false)))])))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),Concrete(N(2))),(Lab("j"),vb')])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("j"),Concrete(R(2.0)))])))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),va'''),(Lab("j"),vb'')])),
	VHole(RecordHole([(Lab("i"),va''), (Lab("j"),vb''')])))),[]));
(* Bool, ['''a->Int, '''b->Int, ''b->Int, ''a->Int] *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	VHole(RecordHole([(Lab("i"),Concrete(N(2))),(Lab("j"),vb'),(Lab("k"),vc')])),
	VHole(RecordHole([(Lab("i"),va'),(Lab("j"),Concrete(B(false))),(Lab("k"),vc'')])))),[]));
(* Bool, ['a->int,'b->bool, 'c->''a13, ''c->''a13] *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va',
	VHole(RecordHole([(Lab("i"),vb'),(Lab("j"),Concrete(B(false))),(Lab("k"),vc'')])))),[]));
(* Bool, 
  [ ''c -> ''a18, 'a16 -> ''a18, 'a15 -> bool, 'b -> ''a17, 'a14 -> ''a17, 'a -> {i:'a14, j:'a15, k:'a16} ] *)
  
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va',
	VHole(RecordHole([])))),[]));
(* Bool, ['a->{}] *)
	
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va',
	VHole(RecordHole([(Lab("i"),vb''')])))),[]));
(* Bool, ['a->{i:'a19}, 'a19->int, '''b->int] *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va'',
	VHole(RecordHole([(Lab("i"),vb''')])))),[]));
(* Bool, [ '''b -> int, ''a20 -> int, ''a -> {i:''a20} ] *)
	
prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va'',
	VHole(RecordHole([(Lab("i"),vb'),(Lab("j"),Concrete(B(false))),(Lab("k"),vc'')])))),[]));
(* Bool,
  [ ''c -> ''a25, ''a23 -> ''a25, ''a22 -> bool, 'b -> ''a24, ''a21 -> ''a24, ''a -> {i:''a21, j:''a22, k:''a23} ] *)

prettyPrintTypeOf(typeof(VHole(BinaryOpHole(BoolOper(EQ),
	va''',
	VHole(RecordHole([(Lab("i"),vb''')])))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(Concrete(N(3))),Value(Concrete(N(4))))),[]));
(* Int, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(Concrete(N(3))),Value(Concrete(R(4.0))))),[]));
(* 'a0, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(BinaryOpHole(BoolOper(LESS),va',vb')),
	Value(Concrete(N(3))),Value(Concrete(N(4))))),[]));
(* Int, ['a->'''a0, 'b->'''a0] *)
	
prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(RecordHole([(Lab("i"),va'''),(Lab("j"),vb''')])),
	Value(Concrete(N(3))),Value(Concrete(N(4))))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va'',Value(Concrete(N(3))),Value(Concrete(N(4))))),[]));
(* Int, [''a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va''',Value(Concrete(N(3))),Value(Concrete(N(4))))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va'',
	Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(R(2.0)))])),
	Value(VRecord([(Lab("i"),Concrete(N(3))),(Lab("j"),Concrete(R(3.0)))])))),[]));
(* {i:int, j:real}, [''a->bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va'',
	Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(R(2.0)))])),
	Value(VRecord([(Lab("i"),Concrete(N(3))),(Lab("j"),Concrete(B(true)))])))),[]));
(* ''a0, [''a->bool] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(N(2))),[(PWildcard,Value(va'))]),[]));
(* case 2 of _ -> v['a]
   =>
   'a *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(N(2))),[(PVar(Var("x")),Variable(Var("x")))]),[]));
(* case 2 of x -> x
   =>
   Int *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(N(2))),[(PVar(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))]),[]));
(* case 2 of x -> x+x
   =>
   Int *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(B(true))),
		 [(PVar(Var("x")),
		   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(B(false))),
		 [(PVar(Var("x")),
		   Condition(Variable(Var("x")),Value(Concrete(N(2))),Value(Concrete(N(3)))))]),[]));
(* case false of x -> if x then 2 else 3
   =>
   Int *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Value(va''),
		 [(PVar(Var("x")),
		   Condition(Variable(Var("x")),Value(Concrete(N(2))),Value(Concrete(N(3)))))]),[]));
(* case v[''a] of x -> if x then 2 else 3
   =>
   Int, [''a->bool] *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Condition(Value(Concrete(B(true))),
				   Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(3)))])),
				   Value(VRecord([(Lab("i"),Concrete(N(4))),(Lab("j"),Concrete(N(5)))]))),
		 [(PVar(Var("x")),
		   BoolExpr(EQ,Variable(Var("x")),Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(3)))]))))]),[]));
(* case (if true then {i=2,j=3} then {i=4,j=5}) of x -> x = {i=2,j=3}
   =>
   Bool*)
   
prettyPrintTypeOf(typeofexpr(
	Case(Condition(Value(Concrete(B(true))),
				   Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(3)))])),
				   Value(VRecord([(Lab("i"),Concrete(N(4))),(Lab("j"),Concrete(N(5)))]))),
		 [(PVar(Var("x")),
		   BoolExpr(EQ,Variable(Var("x")),Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("k"),Concrete(N(3)))]))))]),[]));
(* FAIL *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Condition(Value(Concrete(B(true))),
				   Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(3)))])),
				   Value(VRecord([(Lab("i"),Concrete(N(4))),(Lab("k"),Concrete(N(5)))]))),
		 [(PVar(Var("x")),
		   BoolExpr(EQ,Variable(Var("x")),Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(3)))]))))]),[]));
(* Bool, 
  [ 'a32 -> int, 'a31 -> int, 'a30 -> {i:'a31, j:'a32} ] *)
   
prettyPrintTypeOf(typeofexpr(
	Case(Value(va'''),
		 [(PVar(Var("x")),
		   Condition(Variable(Var("x")),Value(Concrete(N(2))),Value(Concrete(N(3)))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(N(2))),
		 [(PVal(N(2)),
		   Value(Concrete(N(3))))]),[]));
(* Int *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(R(3.0))),
		 [(PVal(N(2)),
		   Value(Concrete(N(3))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(R(3.0))),
		 [(PVal(R(3.0)),
		   Value(Concrete(N(3))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(B(true))),
		 [(PVal(B(true)),
		   Value(Concrete(N(3))))]),[]));
(* Int *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(Concrete(B(true))),
		 [(PVal(B(false)),
		   Value(Concrete(N(3))))]),[]));
(* Int *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va'),
		 [(PVal(B(false)),
		   Value(Concrete(N(3))))]),[]));
(* Int, ['a->bool] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va''),
		 [(PVal(B(false)),
		   Value(Concrete(R(3.0))))]),[]));
(* Real, [''a->bool] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va'''),
		 [(PVal(B(false)),
		   Value(Concrete(R(3.0))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va'''),
		 [(PVal(N(3)),
		   Value(Concrete(R(3.0))))]),[]));
(* Real, ['''a->int] *)

prettyPrintTypeOf(typeofexpr(
	Case(Case(Value(va'),[(PVar(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))]),
		 [(PVal(N(3)),
		   Value(Concrete(R(3.0))))]),[]));
(* Real, ['a->'''a32, '''a32->int] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(4)))])),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   ArithExpr(PLUS,Variable(Var("x")), Variable(Var("y"))))]),[]));
(* Int *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(R(4.0)))])),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   ArithExpr(PLUS,Variable(Var("x")), Variable(Var("y"))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(N(4))),(Lab("k"),Concrete(N(6)))])),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y"))),(Lab("k"),PVar(Var("z")))]),
		   ArithExpr(PLUS,Variable(Var("x")), ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]),[]));
(* Int *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),vb'''),(Lab("k"),va')])),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y"))),(Lab("k"),PVar(Var("z")))]),
		   ArithExpr(PLUS,Variable(Var("x")), ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]),[]));
(* Int, ['''b->'''a33, 'a->'''a33, '''a33->int] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([])),
		 [(PRecord([]),
		   Value(Concrete(B(false))))]),[]));
(* Bool *)

prettyPrintTypeOf(typeofexpr(
	Case(Record([(Lab("i"),ArithExpr(PLUS,Value(Concrete(N(2))),Value(va''))),
				 (Lab("j"),Condition(Value(vb''),Value(Concrete(N(2))),Value(Concrete(N(3))))),
				 (Lab("k"),Value(Concrete(B(true))))]),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y"))),(Lab("k"),PVal(B(true)))]),
		   BoolExpr(LESS,Variable(Var("x")),Variable(Var("y"))))]),[]));
(* Bool, [''b->bool, ''a->int] *)

prettyPrintTypeOf(typeofexpr(
	Case(Record([(Lab("i"),ArithExpr(PLUS,Value(Concrete(N(2))),Value(va''))),
				 (Lab("j"),Condition(Value(vb''),Value(Concrete(N(2))),Value(Concrete(N(3))))),
				 (Lab("k"),Value(Concrete(B(true))))]),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   BoolExpr(LESS,Variable(Var("x")),Variable(Var("y"))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va'),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   BoolExpr(LESS,Variable(Var("x")),Variable(Var("y"))))]),[]));
(* Bool, [ 'a35 -> '''a36, 'a34 -> '''a36, 'a -> {i:'a34, j:'a35} ] *)
		 
prettyPrintTypeOf(typeofexpr(
	Case(Value(va'),
		 [(PRecord([]),
		   Value(Concrete(N(2))))]),[]));
(* Int, ['a->{}] *)
		 
prettyPrintTypeOf(typeofexpr(
	Case(Value(va''),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))]),[]));
(* Bool, [ ''a38 -> ''a39, ''a37 -> ''a39, ''a -> {i:''a37, j:''a38} ] *)
		 
prettyPrintTypeOf(typeofexpr(
	Case(Value(va'''),
		 [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		   BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(va'),
		 [(PRecord([(Lab("i"),PWildcard)]),
		   Value(Concrete(N(2))))]),[]));
(* Int, ['a->{i:'a40} ] *)

prettyPrintTypeOf(typeofexpr(
	Case(Value(VRecord([(Lab("i"),Concrete(N(2))),(Lab("j"),Concrete(B(false))),(Lab("a"),Concrete(N(3))),(Lab("k"),Concrete(N(5)))])),
		 [(PRecord([(Lab("a"),PVal(N(3))),(Lab("k"),PVar(Var("y"))),(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("z")))]),
		   Condition(Variable(Var("z")),Variable(Var("x")),Variable(Var("y"))))]),[]));
(* Int *)


prettyPrintTypeOf(typeofexpr(ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(4)))),[])); 			(* Int *) 
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),[])); 	(* Real *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(N(3)))),[])); 		(* FAIL *)
prettyPrintTypeOf(typeofexpr(ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),[])); 	(* Real *)
prettyPrintTypeOf(typeofexpr(ArithExpr(SUBTRACT,Value(Concrete(B(true))),Value(Concrete(R(3.0)))),[])); (* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(N(5)))),[])); 		(* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(MORE,Value(Concrete(R(3.0))),Value(Concrete(R(6.0)))),[])); 	(* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(5)))),[])); 		(* Bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	Value(VRecord([(Lab("j"),Concrete(B(true))),(Lab("i"),Concrete(N(5)))])),
	Value(VRecord([(Lab("i"),Concrete(N(6))),(Lab("j"),Concrete(B(false)))]))),[]));
(* Bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(5.0)))),[])); 		
(* FAIL *)

prettyPrintTypeOf(typeofexpr(ArithExpr(PLUS,Value(Concrete(N(3))),Value(va')),[])); 	(* Int, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(vb'),Value(Concrete(R(5.0)))),[]));	(* Real, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(TIMES,Value(va'''),Value(vb'')),[])); 			(* Int, ['''a->Int, ''b->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(vb''),Value(va''')),[]));  			(* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(LESS,Value(Concrete(N(3))),Value(va')),[])); 		(* Bool, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(LESS_EQ,Value(vb'),Value(Concrete(R(5.0)))),[])); (* Bool, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(va'''),Value(vb'')),[])); 				(* Bool, [''b->Int,'''a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(vb'''),Value(va''')),[])); 				(* Bool, ['''a->Int,'''b->Int]  *)

prettyPrintTypeOf(typeofexpr(Record([(Lab("1"),Value(Concrete(N(3)))),(Lab("2"),Value(va'))]),[])); 	
(* {1:int, 2:'a} *)

prettyPrintTypeOf(typeofexpr(Record([
	(Lab("a"),ArithExpr(TIMES,Value(va'''),Value(vb''))),
	(Lab("b"),BoolExpr(LESS_EQ,Value(vb'),Value(Concrete(R(5.0)))))]),[])); 
(* {a:int,b: Bool}, ['b->Real, ''b->Int, '''a -> Int] *)
	

prettyPrintTypeOf(typeofexpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  Case(
		  Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4)))])),
		  [(PRecord([(Lab("2"),PVar(Var("x"))),(Lab("1"),PVar(Var("y")))]),
		    ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))]),[]));
(* Int, ['a->{a:'a0,b:'a1}] *)


prettyPrintTypeOf(typeofexpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),
		  Case(Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4)))])),
			   [(PRecord([(Lab("2"),PVar(Var("x"))),(Lab("1"),PVar(Var("y")))]),
			     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])))]),[]));
(* Int, ['a->{a:'a0,b:'a1}, 'a0->int] *)

prettyPrintTypeOf(typeofexpr(Case(
	Value(va'),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  BoolExpr(EQ,
		  BoolExpr(EQ,
			  ArithExpr(PLUS,Variable(Var("x")),
				  Case(Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4))),(Lab("3"),Concrete(B(false)))])),
					   [(PRecord([(Lab("3"),PVal(B(false))),(Lab("2"),PVar(Var("x"))),(Lab("1"),PVar(Var("y")))]),
					     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
		Value(Concrete(N(3)))),
	Variable(Var("y"))))]),[]));
(* Bool, ['a1->bool, 'a0->int, 'a->{a:'a0,b:'a1}] *)
			

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,Variable(Var("x"))),[])); 
(* Int -> Int *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))),[]));
(* Int -> Int *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),Real,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x")))),[]));
(* Real -> Real *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),Int,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x")))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,
	Value(Fun(Var("y"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[]));
(* Int -> Int -> Int *)

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))),[]));
(* Int -> Int -> Int *)

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,
	Value(Fun(Var("y"),Bool,BoolExpr(EQ,BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(2)))),Variable(Var("y")))))),[]));
(* Int -> Bool -> Bool *)

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,
	Value(Fun(Var("y"),Int,BoolExpr(EQ,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2)))),Variable(Var("y")))))),[]));
(* Int -> Int -> Bool *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
		 Case(Variable(Var("x")),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
			    ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),[]));
(* {a:int,b:int} -> int *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
		 Case(Variable(Var("x")),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("c"),PVar(Var("y")))]),
			    ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),[]));
(* FAIL*)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),TRecord([(Lab("1"),TRecord([(Lab("a"),Int),(Lab("b"),Int)])),
						  (Lab("2"),TRecord([(Lab("a"),Int),(Lab("b"),Int)]))]),
		 Case(Variable(Var("x")),
		      [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
			    BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))])),[]));
(* {1:{a:int, b:int}, 2:{a:int, b:int}} -> bool *)

prettyPrintTypeOf(typeof(
	Fun(Var("x"),TRecord([(Lab("1"),TRecord([(Lab("a"),Int),(Lab("b"),Int)])),
						  (Lab("2"),Real)]),
		 Value(Fun(Var("y"),TRecord([(Lab("1"),TRecord([(Lab("a"),a'),(Lab("b"),a'')])),
									 (Lab("2"),a''')]),
			Case(Variable(Var("x")),
			     [(PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("x")))]),
				   Condition(BoolExpr(LESS,Variable(Var("x")),Value(Concrete(R(2.0)))),
						     BoolExpr(EQ,Variable(Var("a")),
						                 Value(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(N(3)))]))),
						     BoolExpr(EQ,Variable(Var("y")),
						                 Value(VRecord([(Lab("1"),VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))])),
									                    (Lab("2"),Concrete(N(2)))])))))])))),[]));
(* {1:{a:int,b:bool},2:real} -> {1:{a:int,b:bool},2:int} -> bool
 [ '''a -> int, ''a -> bool, 'a -> int ] *)
	
prettyPrintTypeOf(typeofexpr(App(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(10)))))),
	Value(Concrete(N(2)))),[]));
(* int *)

prettyPrintTypeOf(typeofexpr(App(
	Value(Fun(Var("x"),TRecord([(Lab("i"),TRecord([(Lab("2"),Bool)])),(Lab("j"),Real),(Lab("k"),TFun(Int,Real))]),
		  Value(Fun(Var("y"),TRecord([(Lab("i"),TRecord([(Lab("1"),a'),(Lab("2"),a'')])),(Lab("j"),b')]),
			Case(Variable(Var("x")),
				 [(PRecord([(Lab("j"),PVar(Var("x"))),(Lab("i"),PVar(Var("a"))),(Lab("k"),PVar(Var("b")))]),
				   Case(Variable(Var("y")),
					  [(PRecord([(Lab("i"),PVar(Var("xx"))),(Lab("j"),PVar(Var("yy")))]),
					    Condition(BoolExpr(LESS,ArithExpr(PLUS,Variable(Var("x")),App(Variable(Var("yy")),Value(Concrete(R(3.0))))),
											    ArithExpr(PLUS,Value(Concrete(R(2.0))),App(Variable(Var("b")),Value(Concrete(N(2)))))),
							      BoolExpr(EQ,Variable(Var("a")), Value(VRecord([(Lab("2"),Concrete(B(true)))]))),
							      BoolExpr(EQ,Variable(Var("xx")),Value(VRecord([(Lab("1"),Concrete(N(1))),(Lab("2"),Concrete(B(false)))])))))]))]))))),
	Record([(Lab("k"),Condition(Value(Concrete(B(true))),Value(Fun(Var("x"),Int,Value(Concrete(R(2.0))))),
														 Value(Fun(Var("y"),Int,Value(Concrete(R(2.0))))))),
		    (Lab("i"),Value(VRecord([(Lab("2"),Concrete(B(false)))]))),
			(Lab("j"),ArithExpr(PLUS,Value(Concrete(R(2.0))),Value(Concrete(R(3.0)))))])),[]));
(* (fn x:{i:{2:bool},j:real,k:(int->real)} =>
	fn y:{i:{1:'a,2:''a},j:'b} =>
		case x of {j=x,i=a,k=b} ->
			case y of {i=xx,j=yy} ->
				if (x + (yy 3.0)) < (2.0 + (b 2))
				then a = {2=true}
				else xx = {1=1,2=false})
   ({k=if true then fn x:int => 2.0 else fn y:int => 2.0,
	 i={2=false},
	 j=2.0+3.0})
   =>
   {i:{1:int, 2:bool}, j:(real -> real)} -> bool
   [ ''a -> bool, 'a -> int, 'a56 -> real, 'a57 -> real, 'a58 -> 'a56, 'b -> ('a57 -> 'a58) ]
*)
     
prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Value(Concrete(N(2)))),[]));
(* 'a59, [ 'a60 -> int, 'a61 -> 'a59, 'a -> ('a60 -> 'a61) ] *)

prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))))),[]));
(* 'a62, [ 'a65 -> int, 'a66 -> int, 'a64 -> 'a62, 'a63 -> ('a65 -> 'a66), 'a -> ('a63 -> 'a64) ] *)

prettyPrintTypeOf(typeofexpr(App(
	Value(Fun(Var("x"),a',Record([(Lab("a"),Variable(Var("x"))),(Lab("b"),Variable(Var("x")))]))),
	Value(Concrete(N(2)))),[]));
(* {a:'a,b:'a} *)
	
prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Value(va')),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Value(VRecord([(Lab("i"),va'),(Lab("j"),vb')]))),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Value(Concrete(N(3)))),[]));
(* 'a69, [ 'a70 -> int, 'a71 -> 'a69, 'a -> ('a70 -> 'a71) ] *)
	
prettyPrintTypeOf(typeofexpr(App(
	Value(va''),Value(Concrete(N(2)))),[]));
(* FAIL *)
	
prettyPrintTypeOf(typeofexpr(App(
	Value(va'''),Value(Concrete(N(2)))),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(App(
	Value(Concrete(B(true))),Value(Concrete(B(true)))),[]));
(* FAIL *)
	
prettyPrintTypeOf(typeofexpr(App(
	Value(va'),Case(Value(VRecord([(Lab("a"),Concrete(N(1))),(Lab("c"),Concrete(B(true))),
								   (Lab("b"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))])),
					[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PWildcard),(Lab("c"),PVar(Var("y")))]),
				      BoolExpr(EQ,(BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(2))))),Variable(Var("y"))))])),[]));
(* 'a72, [ 'a73 -> bool, 'a74 -> 'a72, 'a -> ('a73 -> 'a74) ] *)

prettyPrintTypeOf(typeofexpr(Record([
	(Lab("a"),ArithExpr(PLUS,Value(va'),Value(vb''))),
	(Lab("b"),BoolExpr(EQ,Value(va'),Value(Concrete(N(2)))))]),[]));
(* {a:int,b:bool}, [''b->int,'a->int] *)

prettyPrintTypeOf(typeofexpr(Case(
	Value(Concrete(N(3))),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(N(2)))),
		    (PVar(Var("x")),Variable(Var("x")))]))]),[]));
(* case 3 of 1 -> case true of true -> 1 | false -> 2
		   | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
		   | x -> case x of 1 -> 2 | x -> x
  =>
  Int *)
  
prettyPrintTypeOf(typeofexpr(Value(Fun(Var("x"),Int,Case(
	Variable(Var("x")),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(N(2)))),
		    (PVar(Var("x")),Variable(Var("x")))]))]))),[]));
(* fn x:int => case x of 1 -> case true of true -> 1 | false -> 2
					   | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
					   | x -> case x of 1 -> 2 | x -> x
  =>
  Int->Int *)
  
prettyPrintTypeOf(typeofexpr(Value(Fun(Var("x"),Int,Case(
	Variable(Var("x")),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(N(2)))),
		    (PVar(Var("x")),
			 App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
			     Variable(Var("x"))))]))]))),[]));
(* fn x:int => case x of 1 -> case true of true -> 1 | false -> 2
					   | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
					   | x -> case x of 1 -> 2 | x -> (fn x:int=>x*10) x
  =>
  Int->Int *)
 
prettyPrintTypeOf(typeofexpr(Value(Fun(Var("x"),Int,Case(
	Variable(Var("x")),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(R(2.0)))),
		    (PVar(Var("x")),
			 App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
			     Variable(Var("x"))))]))]))),[]));
(* fn x:int => case x of 1 -> case true of true -> 1 | false -> 2
					   | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
					   | x -> case x of 1 -> 2.0 | x -> (fn x:int=>x*10) x
  =>
  'a88 *)	 
			 
prettyPrintTypeOf(typeofexpr(Value(Fun(Var("x"),Real,Case(
	Variable(Var("x")),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(N(2)))),
		    (PVar(Var("x")),
			 App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
			     Variable(Var("x"))))]))]))),[]));
(* fn x:real => case x of 1 -> case true of true -> 1 | false -> 2
					    | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
					    | x -> case x of 1 -> 2 | x -> (fn x:int=>x*10) x
  =>
  FAIL *) 
  
prettyPrintTypeOf(typeofexpr(Value(Fun(Var("x"),Int,Case(
	Variable(Var("x")),
	[(PVal(N(1)),
	  Case(Value(Concrete(B(true))),
		   [(PVal(B(true)),Value(Concrete(N(1)))),
		    (PVal(B(false)),Value(Concrete(N(2))))])),
	 (PVal(N(2)),
	  Case(Value(VRecord([(Lab("i"),Concrete(N(1))),(Lab("j"),Concrete(N(2)))])),
	       [(PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]),
		     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	 (PVar(Var("x")),
	  Case(Variable(Var("x")),
	       [(PVal(N(1)),Value(Concrete(N(2)))),
		    (PVar(Var("x")),
			 App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
			     Variable(Var("x"))))])),
	 (PVal(B(true)),Value(Concrete(N(5))))]))),[]));
(* fn x:int => case x of 1 -> case true of true -> 1 | false -> 2
					   | 2 -> case {i=1,j=2} of {i=x,j=y} -> x+y
					   | x -> case x of 1 -> 2 | x -> (fn x:int=>x*10) x
					   | true -> 5
  =>
  FAIL *) 
  
prettyPrintTypeOf(typeofexpr(
	Let(Var("x"),Int,Value(Concrete(N(1))),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))),[]));
(* int *)
	
prettyPrintTypeOf(typeofexpr(
	Let(Var("x"),Bool,Value(Concrete(B(true))),
		Condition(Variable(Var("x")),
				  Let(Var("y"),TFun(Int,TFun(Int,Int)),Value(Fun(Var("x"),Int,Value(Fun(Var("y"),Int,Variable(Var("x")))))),
					  App(Variable(Var("y")),Value(Concrete(N(1))))),
				  Let(Var("z"),TFun(Real,TFun(Int,Int)),Value(Fun(Var("x"),Real,Value(Fun(Var("y"),Int,Variable(Var("y")))))),
				      App(Variable(Var("z")),Value(Concrete(R(1.0))))))),[]));
(* int -> int *)

prettyPrintTypeOf(typeofexpr(
Case(Record([(Lab("a"),Let(Var("x"),Int,Value(Concrete(N(10))),
						   Value(Fun(Var("y"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))))),
			 (Lab("b"),Value(Fun(Var("z"),Bool,Condition(Variable(Var("z")),Value(Concrete(N(10))),Value(Concrete(N(20))))))),
			 (Lab("c"),Value(Concrete(N(1))))]),
   [(PRecord([(Lab("c"),PVal(N(0))),(Lab("a"),PWildcard),(Lab("b"),PVar(Var("x")))]),Value(Concrete(N(10)))),
    (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("c"),PVar(Var("z"))),(Lab("b"),PVar(Var("y")))]),
	 App(Variable(Var("x")),App(Variable(Var("y")),BoolExpr(EQ,Variable(Var("z")),Value(Concrete(N(1)))))))]),[]));
(* case {a=let x:int = 10 in fn y:int => x+y end, b=fn z:bool => if z then 10 else 20,c=1}
   of {c=0,a=_,b=x} -> 10
   |  {a=x,c=z,b=y} -> x (y (z=1)) *)
  
prettyPrintTypeOf(typeof(VList([Concrete(N(1)),Concrete(N(2)),Concrete(N(3))]),[]));			(* int list *)
prettyPrintTypeOf(typeof(VList([Concrete(R(1.0)),Concrete(R(2.0)),Concrete(R(3.0))]),[])); 		(* real list *)
prettyPrintTypeOf(typeof(VList([Concrete(B(true)),Concrete(B(true)),Concrete(B(false))]),[])); 	(* bool list *)

prettyPrintTypeOf(typeof(
VList([Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	   Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))]),[]));
(* (int->int) list *)

prettyPrintTypeOf(typeof(Concrete(EmptyList),[])); (* 'a101 list *)

prettyPrintTypeOf(typeof(VList([VList([Concrete(N(1))]),VList([Concrete(N(2))])]),[])); (* int list list *)

prettyPrintTypeOf(typeof(VList([Concrete(EmptyList),Concrete(EmptyList)]),[])); (* 'a104 list *)

prettyPrintTypeOf(typeof(VList([
	VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(B(true))),(Lab("c"),Fun(Var("x"),Int,Value(Concrete(R(1.0)))))]),
	VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(false))),(Lab("c"),Fun(Var("y"),Int,ArithExpr(DIVIDE,Value(Concrete(R(1.0))),Value(Concrete(R(3.0))))))])]),
	[]));
(* {a:int,b:bool,c:(int->real)} list *)

prettyPrintTypeOf(typeof(Fun(Var("x"),Int,List([
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),
	ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))])),[]));
(* int -> (int list) *)

prettyPrintTypeOf(typeofexpr(List([
	Condition(Value(Concrete(B(false))),Value(Concrete(N(1))),Value(Concrete(N(3)))),
	App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
		Case(Record([(Lab("a"),Value(Concrete(N(5)))),(Lab("b"),Value(Concrete(N(6))))]),
			 [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
			   ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))])),
	App(Value(Fun(Var("y"),Int,
			      Let(Var("x"),TFun(Int,Int),Value(Fun(Var("z"),Int,ArithExpr(TIMES,Variable(Var("z")),Variable(Var("y"))))),
					  App(Variable(Var("x")),Value(Concrete(N(5))))))),
		Value(Concrete(N(6))))]),[]));
(* int list *)

prettyPrintTypeOf(typeofexpr(Case(
	List([Value(Concrete(N(1))),Value(Concrete(N(2)))]),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PWildcard),Value(Concrete(B(false))))]),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(Case(
	List([Value(Concrete(N(1))),Value(Concrete(N(2)))]),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PWildcard,PWildcard),Value(Concrete(B(false)))),
	 (PWildcard,Value(Concrete(B(false)))),
	 (PVar(Var("x")),Value(Concrete(B(false))))]),[]));
(* bool *)
	 
prettyPrintTypeOf(typeofexpr(Case(
	List([Value(Concrete(N(1))),Value(Concrete(N(2)))]),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PWildcard,PWildcard),Value(Concrete(B(false)))),
	 (PWildcard,Value(Concrete(B(false)))),
	 (PVal(N(2)),Value(Concrete(B(false))))]),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(Case(
	List([Value(Concrete(B(true))),Value(Concrete(B(false)))]),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),Variable(Var("x")))]),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(Case(
	List([Value(Concrete(B(true))),Value(Concrete(B(false)))]),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),BoolExpr(EQ,Variable(Var("y")),Value(VList([Concrete(B(true))]))))]),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	List([Value(Concrete(N(1))),Value(Concrete(N(2)))]),
	List([Value(Concrete(N(3)))])),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	List([Value(Concrete(R(1.0))),Value(Concrete(R(2.0)))]),
	List([Value(Concrete(R(3.0)))])),[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	List([Value(Concrete(B(true))),Value(Concrete(B(false)))]),
	List([Value(Concrete(B(true)))])),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	Value(VList([(Concrete(N(1)))])),
	List([Value(Concrete(N(2)))])),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(Concrete(EmptyList)),Value(Concrete(EmptyList))),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	Value(VList([(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(B(true))),(Lab("c"),VRecord([]))])),
		         (VRecord([(Lab("a"),Concrete(N(5))),(Lab("b"),Concrete(B(false))),(Lab("c"),VRecord([]))]))])),
	List([(Condition(Value(Concrete(B(true))),
					 Record([(Lab("a"),Value(Concrete(N(5)))),(Lab("b"),BoolExpr(EQ,Value(Concrete(EmptyList)),Value(Concrete(EmptyList)))),
					         (Lab("c"),Case(Value(Concrete(N(3))),[(PWildcard,Record([]))]))]),
					 Record([(Lab("a"),Value(Concrete(N(2)))),(Lab("b"),Value(Concrete(B(true)))),(Lab("c"),Record([]))])))])),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,
	Value(VList([Concrete(N(1))])),
	Value(Concrete(EmptyList))),[]));
(* bool *)

prettyPrintTypeOf(typeofexpr(Cons(
	Value(Concrete(N(1))),
	Value(VList([Concrete(N(2)),Concrete(N(3))]))),[]));
(* int list *)

prettyPrintTypeOf(typeofexpr(Cons(
	Value(Concrete(B(true))),
	Value(VList([Concrete(B(false)),Concrete(B(true))]))),[]));
(* bool list *)

prettyPrintTypeOf(typeofexpr(Cons(
	Value(Fun(Var("x"),Int,Value(Concrete(N(0))))),
	Value(VList([Fun(Var("y"),Int,Value(Concrete(N(1))))]))),[]));
(* (int->int) list *)

prettyPrintTypeOf(typeofexpr(Cons(
	Value(Concrete(B(true))),
	Value(Concrete(EmptyList))),[]));
(* bool list *)

(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)