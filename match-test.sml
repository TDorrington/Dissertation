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

fun prettyPrintMatch(c) = (case c of
	  NONE => "FAIL"
	| SOME (gamma,theta) => 
		"[ " ^ prettyPrintGamma(gamma) ^ "] , [" ^ prettyPrintTheta(theta) ^ " ]");
	
prettyPrintMatch(matchTypes(Int,PWildcard,[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(Bool,PWildcard,[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(Real,PWildcard,[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PWildcard,[],[]));	(* [], [] *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PWildcard,[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PWildcard,[],[]));		(* [], [] *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PWildcard,[],[]));			(* [], [] *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PWildcard,[],[]));	(* [], [] *)

prettyPrintMatch(matchTypes(Int,PVar(Var("x")),[],[]));				(* [x->1], []    *)
prettyPrintMatch(matchTypes(Bool,PVar(Var("x")),[],[]));			(* [x->true], [] *)
prettyPrintMatch(matchTypes(Real,PVar(Var("x")),[],[]));			(* [x->1.0], []  *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PVar(Var("x")),[],[]));	(* [x->fn x:int=>1], [] *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PVar(Var("x")),[],[]));				(* [x->v['a]], []       *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PVar(Var("x")),[],[]));		(* [x->v[''a]], []      *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PVar(Var("x")),[],[]));			(* [x->v['''a]], []     *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PVar(Var("x")),[],[]));	(* [x->{a=1,b=1.0}], [] *)

prettyPrintMatch(matchTypes(Int,PVal(N(3)),[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(Bool,PVal(N(3)),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(Real,PVal(N(3)),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PVal(N(3)),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PVal(N(3)),[],[]));				(* [], ['a->int]   *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PVal(N(3)),[],[]));		(* [], [''a->int]  *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PVal(N(3)),[],[]));			(* [], ['''a->int] *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PVal(N(3)),[],[]));	(* FAIL *)

prettyPrintMatch(matchTypes(Int,PVal(R(3.0)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(Bool,PVal(R(3.0)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(Real,PVal(R(3.0)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PVal(R(3.0)),[],[]));		(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PVal(R(3.0)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PVal(R(3.0)),[],[]));		(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PVal(R(3.0)),[],[]));		(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PVal(R(3.0)),[],[]));	(* FAIL *)

prettyPrintMatch(matchTypes(Int,PVal(B(true)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(Bool,PVal(B(true)),[],[]));				(* [], [] *)
prettyPrintMatch(matchTypes(Real,PVal(B(true)),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PVal(B(true)),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PVal(B(true)),[],[]));				(* [], ['a->bool]  *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PVal(B(true)),[],[]));		(* [], [''a->bool] *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PVal(B(true)),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PVal(B(true)),[],[]));	(* FAIL *)

prettyPrintMatch(matchTypes(Int,PRecord([]),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(Bool,PRecord([]),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(Real,PRecord([]),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PRecord([]),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PRecord([]),[],[]));				(* [], ['a->{}]  *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([]),[],[]));		(* [], [''a->{}] *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PRecord([]),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([]),PRecord([]),[],[]));								(* [], [] *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int),(Lab("b"),Real)]),PRecord([]),[],[]));	(* FAIL *)

prettyPrintMatch(matchTypes(Int,PRecord([(Lab("a"),PWildcard)]),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PRecord([(Lab("a"),PWildcard)]),[],[]));		(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PRecord([(Lab("1"),PWildcard)]),[],[]));
(* [], ['a->{1:'a0}]  *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([(Lab("1"),PWildcard)]),[],[]));		
(* [], [''a->{1:''a0}] *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PRecord([(Lab("1"),PWildcard)]),[],[]));		
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([]),PRecord([(Lab("1"),PWildcard)]),[],[]));								
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int)]),PRecord([(Lab("1"),PWildcard)]),[],[]));	
(* [], [] *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int)]),PRecord([(Lab("1"),PWildcard)]),[],[]));	
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int),(Lab("2"),Real)]),PRecord([(Lab("1"),PWildcard)]),[],[]));	
(* FAIL *)

prettyPrintMatch(matchTypes(Int,PRecord([(Lab("a"),PVar(Var("x")))]),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PRecord([(Lab("a"),PVar(Var("x")))]),[],[]));		(* FAIL *)
prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));
(* [x->v['a0]], ['a->{1:'a0}]  *)
prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));		
(* [x->v['a0]], [''a->{1:''a0}] *)
prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));		
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([]),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));								
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int)]),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));	
(* [x->1], [] *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int)]),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));	
(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int),(Lab("2"),Real)]),PRecord([(Lab("1"),PVar(Var("x")))]),[],[]));	
(* FAIL *)

prettyPrintMatch(matchTypes(Int,PRecord([(Lab("a"),PVal(N(3)))]),[],[]));				(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Int),PRecord([(Lab("a"),PVal(N(3)))]),[],[]));		(* FAIL *)

prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));
(* [], ['a->{1:'a0}, 'a0->int]  *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));		
(* [], [''a->{1:''a0}, ''a0->int] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([(Lab("1"),PVal(B(false)))]),[],[]));		
(* [], [''a->{1:''a0}, ''a0->bool] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PRecord([(Lab("1"),PVal(R(3.0)))]),[],[]));		
(* FAIL *)

prettyPrintMatch(matchTypes(THole(TypeHole(ArithTypeVar("a"))),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));		
(* FAIL *)

prettyPrintMatch(matchTypes(TRecord([]),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));								
(* FAIL *)

prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int)]),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));	
(* [], [] *)

prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Bool)]),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));	
(* FAIL *)

prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int)]),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));	
(* FAIL *)

prettyPrintMatch(matchTypes(TRecord([(Lab("1"),Int),(Lab("2"),Real)]),PRecord([(Lab("1"),PVal(N(3)))]),[],[]));	
(* FAIL *)

prettyPrintMatch(matchTypes(
	TRecord([(Lab("1"),Int),(Lab("2"),Bool),(Lab("3"),TFun(Int,Real)),(Lab("4"),THole(TypeHole(ArithTypeVar("a"))))]),
	PRecord([(Lab("1"),PWildcard),(Lab("2"),PVal(B(false))),(Lab("3"),PVar(Var("x"))),(Lab("4"),PVar(Var("y")))]),
	[],[]));
(* [y->v['''a],x->fn x:int=>1.0], [] *)

prettyPrintMatch(matchTypes(
	TRecord([(Lab("1"),Int),(Lab("2"),Bool),(Lab("3"),TFun(Int,Real)),(Lab("4"),THole(TypeHole(ArithTypeVar("a"))))]),
	PRecord([(Lab("1"),PWildcard),(Lab("2"),PVal(B(false))),(Lab("3"),PVar(Var("x"))),(Lab("4"),PVar(Var("x")))]),
	[],[]));
(* FAIL *)

prettyPrintMatch(matchTypes(
	TRecord([(Lab("1"),Int),(Lab("2"),Bool),(Lab("3"),TFun(Int,Real)),(Lab("5"),THole(TypeHole(ArithTypeVar("a"))))]),
	PRecord([(Lab("1"),PWildcard),(Lab("2"),PVal(B(false))),(Lab("3"),PVar(Var("x"))),(Lab("4"),PVar(Var("y")))]),
	[],[]));
(* FAIL *)

prettyPrintMatch(matchTypes(
	TRecord([(Lab("1"),TRecord([(Lab("a"),Int),(Lab("b"),TFun(Int,a'))])),(Lab("2"),Int)]),
	PRecord([(Lab("1"),PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))])),(Lab("2"),PVar(Var("z")))]),
	[],[]));
(* [z->1, y->fn x:int => v['a], x->1], [] *)

fun prettyPrintMatchList(c) = (case c of

	  NONE => "FAIL"
	| SOME (subExprList,theta) => 
		
		let fun iterPrint([]) = ""
			| 	iterPrint([(e1,g1)]) = "( " ^ prettyPrintExpression(Expression(e1)) ^ ", " ^ prettyPrintGamma(g1) ^ " )"
			|	iterPrint((e1,g1)::l1) = 
			"( " ^ prettyPrintExpression(Expression(e1)) ^ ", " ^ prettyPrintGamma(g1) ^ " )," ^ iterPrint(l1)
			
		in "[ " ^ iterPrint(subExprList) ^ " ], [ " ^ prettyPrintTheta(theta) ^ " ]" end);
		
prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2)))))],[],[]));
(* [ (2+2, ) ], [] *)

prettyPrintMatchList(matchTypesList(
	Real,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2)))))],[],[]));
(* FAIL *)
	
prettyPrintMatchList(matchTypesList(
	Bool,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2)))))],[],[]));
(* FAIL *)

prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVal(N(3)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* [ (2+2, ), (x+y, ) ], [] *)

prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVal(N(3)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(N(4)),Variable(Var("x"))),
	 (PVar(Var("x")),Variable(Var("x"))),
	 (PWildcard,Value(Fun(Var("x"),Int,Value(Concrete(N(3))))))],[],[]));
(* [ (2+2, ), (x+y, ), (x, ), (x,x->1), (fn x:int=>3, ) ], [] *)

prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVal(N(3)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(N(4)),Variable(Var("x"))),
	 (PVar(Var("x")),Variable(Var("x"))),
	 (PWildcard,Value(Fun(Var("x"),Int,Value(Concrete(N(3)))))),
	 (PVal(B(true)),Value(Concrete(N(3))))],[],[]));
(* FAIL *)
	
prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVal(N(3)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(N(4)),Variable(Var("x"))),
	 (PVar(Var("x")),Variable(Var("x"))),
	 (PWildcard,Value(Fun(Var("x"),Int,Value(Concrete(N(3)))))),
	 (PVal(R(2.0)),Value(Concrete(N(3))))],[],[]));
(* FAIL *)

prettyPrintMatchList(matchTypesList(
	Bool,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVal(N(3)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(N(4)),Variable(Var("x"))),
	 (PVar(Var("x")),Variable(Var("x"))),
	 (PWildcard,Value(Fun(Var("x"),Int,Value(Concrete(N(3))))))],[],[]));
(* FAIL *)

prettyPrintMatchList(matchTypesList(
	Int,
	[(PVal(N(2)),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PRecord([(Lab("i"),PVal(N(2))),(Lab("j"),PVal(N(3)))]),Variable(Var("y")))],[],[]));
(* FAIL *)

prettyPrintMatchList(matchTypesList(
	Int,
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->1), (y+y,y->1), (z,z->1)], [] *)

prettyPrintMatchList(matchTypesList(
	Bool,
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->true), (y+y,y->true), (z,z->true)], [] *)

prettyPrintMatchList(matchTypesList(
	Real,
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->1.0), (y+y,y->1.0), (z,z->1.0)], [] *)

prettyPrintMatchList(matchTypesList(
	TFun(Int,Int),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->fn x:int=>1), (y+y,y->fn x:int=>1), (z,z->fn x:int=>1)], [] *)

prettyPrintMatchList(matchTypesList(
	TFun(Int,TFun(Bool,Real)),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->fn x:int=>fn x:bool=>1.0), 
	 (y+y,y->fn x:int=>fn x:bool=>1.0), 
	 (z,z->fn x:int=>fn x:bool=>1.0)], [] *)
	 
prettyPrintMatchList(matchTypesList(
	THole(TypeHole(TypeVar("a"))),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->v['a]), (y+y,y->v['a]), (z,z->v['a])], [] *)

prettyPrintMatchList(matchTypesList(
	THole(TypeHole(ArithTypeVar("a"))),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->v['''a]), (y+y,y->v['''a]), (z,z->v['''a])], [] *)

prettyPrintMatchList(matchTypesList(
	THole(TypeHole(EqualityTypeVar("a"))),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->v[''a]), (y+y,y->v[''a]), (z,z->v[''a])], [] *)

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Real),(Lab("c"),Bool),(Lab("d"),TFun(Int,Bool))]),
	[(PVar(Var("x")),ArithExpr(PLUS,Value(Concrete(N(2))),Value(Concrete(N(2))))),
	 (PVar(Var("y")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("y")))),
	 (PVar(Var("z")),Variable(Var("z")))],[],[]));
(* [ (2+2,x->{a=1,b=1.0,c=true,d=fn x:int=>true}), 
     (y+y,y-{a=1,b=1.0,c=true,d=fn x:int=>true}), 
	 (z,z->{a=1,b=1.0,c=true,d=fn x:int=>true})], [] *)
	 
prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* [ (x*x,x->1), (x-x,x->1), (x+y,y->1,x->1) ], [] *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Bool),(Lab("b"),Int)]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* [ (x*x,x->true), (x-x,x->true), (x+y,y->1,x->true) ], [] *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Real)]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),TFun(Int,Int))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(R(3.0)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3))),(Lab("c"),PVal(N(5)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	Int,
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	TFun(Int,Int),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(3)))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("x")))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[]));
(* FAIL *)	

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),THole(TypeHole(TypeVar("a"))))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true))),(Lab("c"),PWildcard)]),
	  Variable(Var("x"))),
	 (PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(false))),(Lab("c"),PWildcard)]),
	  Variable(Var("y"))),
	 (PRecord([(Lab("a"),PVar(Var("z"))),(Lab("b"),PWildcard),
			   (Lab("c"),PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]))]),
	  ArithExpr(PLUS,Variable(Var("z")),Variable(Var("x"))))],[],[]));
(* [ (x,x->1), (y,y->1), (z+x,y->v['a104],x->v['a103],z->1) ], ['a->{i:'a103,j:'a104}] *)

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),THole(TypeHole(TypeVar("a"))))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true))),(Lab("c"),PWildcard)]),
	  Variable(Var("x"))),
	 (PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(false))),(Lab("c"),PWildcard)]),
	  Variable(Var("y"))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PWildcard),
			   (Lab("c"),PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]))]),
	  ArithExpr(PLUS,Variable(Var("z")),Variable(Var("x"))))],[],[]));
(* FAIL *)

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),THole(TypeHole(TypeVar("a")))),
		     (Lab("b"),THole(TypeHole(TypeVar("b")))),
			 (Lab("c"),THole(TypeHole(TypeVar("c"))))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true))),(Lab("c"),PWildcard)]),
	  Variable(Var("x"))),
	 (PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(false))),(Lab("c"),PWildcard)]),
	  Variable(Var("y"))),
	 (PRecord([(Lab("a"),PVar(Var("z"))),(Lab("b"),PWildcard),
			   (Lab("c"),PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]))]),
	  ArithExpr(PLUS,Variable(Var("z")),Variable(Var("x"))))],[],[]));
(* [ (x,x->v['a]),(y,y->v['a]),(z+x,y->v['a108],x->v['a107],z->v['a]) ], 
   [ 'c->{i:'a107, j:'a108}, 'b->bool ] *)

prettyPrintMatchList(matchTypesList(
	TRecord([(Lab("a"),THole(TypeHole(TypeVar("a")))),
		     (Lab("b"),THole(TypeHole(TypeVar("b")))),
			 (Lab("c"),THole(TypeHole(TypeVar("c"))))]),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true))),(Lab("c"),PWildcard)]),
	  Variable(Var("x"))),
	 (PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(false))),(Lab("c"),PWildcard)]),
	  Variable(Var("y"))),
	 (PRecord([(Lab("a"),PVar(Var("z"))),(Lab("b"),PWildcard),
			   (Lab("c"),PRecord([(Lab("i"),PVar(Var("x"))),(Lab("j"),PVar(Var("y")))]))]),
	  ArithExpr(PLUS,Variable(Var("z")),Variable(Var("x"))))],[(Var("a"),Value(Concrete(N(10))))],[]));
(* [ (x,x->v['a],a->10),(y,y->v['a],a->10),(z+x,y->v['a108],x->v['a107],z->v['a],a->10) ], 
   [ 'c->{i:'a107, j:'a108}, 'b->bool ] *)
   
prettyPrintMatch(matchTypes(TList(Int),PVal(EmptyList),[],[]));	(* [] *)
prettyPrintMatch(matchTypes(TList(Bool),PVal(EmptyList),[],[]));	(* [] *)
prettyPrintMatch(matchTypes(TList(Real),PVal(EmptyList),[],[]));	(* [] *)
prettyPrintMatch(matchTypes(TList(TFun(Int,Bool)),PVal(EmptyList),[],[]));	(* [] *)

prettyPrintMatch(matchTypes(TList(Int),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* [x->1,y->[1]] *)
prettyPrintMatch(matchTypes(TList(Bool),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* [x->true,y->[true]] *)
prettyPrintMatch(matchTypes(TList(Real),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* [x->1.0,y->[1.0]] *)

prettyPrintMatch(matchTypes(TList(Int),PCons(PVar(Var("x")),PCons(PVar(Var("y")),PVar(Var("z")))),[],[]));
(* [x->1,y->1,z->[1]] *)

prettyPrintMatch(matchTypes(TList(TList(Int)),PCons(PCons(PVar(Var("x")),PVar(Var("y"))),PVar(Var("z"))),[],[]));
(* [x->1,y->[1],z->[[1]]] *)

prettyPrintMatch(matchTypes(Int,PVal(EmptyList),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(Bool,PVal(EmptyList),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(Real,PVal(EmptyList),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Bool),PVal(EmptyList),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int)]),PVal(EmptyList),[],[]));(* FAIL *)

prettyPrintMatch(matchTypes(Int,PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(Bool,PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(Real,PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* FAIL *)
prettyPrintMatch(matchTypes(TFun(Int,Bool),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));			(* FAIL *)
prettyPrintMatch(matchTypes(TRecord([(Lab("a"),Int)]),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));	(* FAIL *)

prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PVal(EmptyList),[],[]));	
(* [], ['a->'a41 list, 'a41->'a40] *)

prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));
(* [y->[v['a42]],x->v['a42]], ['a-> 'a43 list, 'a43->'a42] *)

prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PCons(PCons(PVar(Var("x")),PVar(Var("z"))),PVar(Var("y"))),[],[]));

prettyPrintMatch(matchTypes(THole(TypeHole(TypeVar("a"))),PCons(PVar(Var("x")),PCons(PVar(Var("z")),PVar(Var("y")))),[],[]));
(* [ y -> [v['a50]], z -> v['a50], x -> v['a48]] , ['a48 -> 'a50, 'a49 -> 'a48, 'a -> 'a49 list ] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PVal(EmptyList),[],[]));	
(* [], [''a->''a52 list, 'a51->''a52] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PCons(PVar(Var("x")),PVar(Var("y"))),[],[]));
(* [y->[v[''a54]],x->v[''a54]], ['a53->''a54, ''a->''a54 list] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PCons(PCons(PVar(Var("x")),PVar(Var("z"))),PVar(Var("y"))),[],[]));
(* [y->[[v[''a58]]], z->[v[''a58]], x->v[''a58]], ['a57->''a58, ''a56->''a58 list, 'a55->''a56, ''a->''a56 list] *)

prettyPrintMatch(matchTypes(THole(TypeHole(EqualityTypeVar("a"))),PCons(PVar(Var("x")),PCons(PVar(Var("z")),PVar(Var("y")))),[],[]));
(* [ y -> [v[''a60]], z -> v[''a60], x -> v[''a60]] , ['a61 -> ''a60, 'a59 -> ''a60, ''a -> ''a60 list ] *)

prettyPrintMatch(matchTypes(TList(Int),PCons(PVar(Var("x")),PVal(EmptyList)),[],[])); (* [x->1] *)

prettyPrintMatch(matchTypes(TList(Bool),PCons(PVar(Var("x")),PWildcard),[],[])); (* [x->true] *)

prettyPrintMatch(matchTypes(TList(TRecord([(Lab("a"),Int)])),
						    PCons(PRecord([(Lab("a"),PVar(Var("x")))]),PVar(Var("y"))),[],[]));
(* [y->[{a=1}], x->1] *)

prettyPrintMatch(matchTypes(TList(TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),TFun(Int,Int))])),
						    PCons(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("c"),PVar(Var("y"))),(Lab("b"),PVal(B(true)))]),
								  PCons(PRecord([(Lab("a"),PVar(Var("a"))),(Lab("c"),PVar(Var("b"))),(Lab("b"),PVal(B(false)))]),
										PVar(Var("k")))),
							[],[]));
(* [g->[{a=1,b=true,c=fn x:int=>1}], x->1, y->fn x:int=>1, a->1, b->fn x:int=>1] *)
							
prettyPrintMatch(matchTypes(TList(Int),PCons(PRecord([(Lab("a"),PVar(Var("x")))]),PVar(Var("y"))),[],[])); (* FAIL *)
prettyPrintMatch(matchTypes(TList(Real),PCons(PRecord([(Lab("a"),PVar(Var("x")))]),PVar(Var("y"))),[],[]));(* FAIL *)
prettyPrintMatch(matchTypes(TList(Bool),PCons(PRecord([(Lab("a"),PVar(Var("x")))]),PVar(Var("y"))),[],[]));(* FAIL *)

prettyPrintMatchList(matchTypesList(TList(Int),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(false))))],[],[]));
(* [ (true, ), (false, y->[1],x->1) ] *)

prettyPrintMatchList(matchTypesList(TList(Int),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PVal(EmptyList)),Value(Concrete(B(true)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(false))))],[],[]));
(* [ (true, ), (true, x->1), (false, y->[1],x->1) ] *)
	 
fun prettyPrintMatchE(result) = (case result of 
	  Fail => "FAIL"
	| Hole h => "HOLE of " ^ prettyPrintHole(h)
	| Success (e,s,t,g) => prettyPrintExpression(Expression(e)) ^ ", [ " ^ prettyPrintGamma(g) ^ " ], [ " ^
		prettyPrintSigma(s) ^ " ], [ " ^ prettyPrintTheta(t) ^ " ]");
		
		
prettyPrintMatchE(match((Concrete(N(2))),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));		(* 1, [] *)
prettyPrintMatchE(match((Concrete(B(true))),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));	(* 1, [] *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));		(* 1, [] *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));	
(* 1, [] *)

prettyPrintMatchE(match((va'),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));			(* 1, [] *)
prettyPrintMatchE(match((va''),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));			(* 1, [] *)
prettyPrintMatchE(match((va'''),[(PWildcard,Value(Concrete(N(1))))],[],[],[]));		(* 1, [] *)

prettyPrintMatchE(match((Concrete(N(2))),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));	(* true, [x->2]    *)
prettyPrintMatchE(match((Concrete(B(true))),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));(* true, [x->true] *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));	(* true, [x->2.0]  *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));	
(* true, [x->fn x:int => 2] *)

prettyPrintMatchE(match((va'),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));		(* true, [x->v['a]]   *)
prettyPrintMatchE(match((va''),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));		(* true, [x->v[''a]]  *)
prettyPrintMatchE(match((va'''),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));	(* true, [x->v['''a]] *)

prettyPrintMatchE(match(VRecord([(Lab("a"),(Concrete(N(2)))),(Lab("b"),(va'))]),[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[]));	
(* true, [x->{a=2,b=v['a]}] *)

prettyPrintMatchE(match((Concrete(N(2))),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));	(* FAIL *)
prettyPrintMatchE(match((Concrete(R(3.0))),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));	(* FAIL *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));	(* FAIL *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((va'),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));		(* FAIL *)

prettyPrintMatchE(match(VRecord([(Lab("a"),(Concrete(N(2)))),(Lab("b"),(va'))]),[(PVal(R(2.0)),Value(Concrete(N(0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((Concrete(N(2))),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));		(* 1.0, []  *)
prettyPrintMatchE(match((Concrete(N(3))),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(N(2))),[(PVal(N(3)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(B(true))),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL  *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((va'),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [v['a]->2],   ['a>int],    [] *)
prettyPrintMatchE(match((va''),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [v[''a]->2],  [''a->int],  [] *)
prettyPrintMatchE(match((va'''),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [v['''a]->2], ['''a->int], [] *)

prettyPrintMatchE(match(VRecord([(Lab("a"),(Concrete(N(2)))),(Lab("b"),(va'))]),[(PVal(N(2)),Value(Concrete(R(1.0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((Concrete(N(2))),[(PVal(B(true)),Value(Concrete(R(1.0))))],[],[],[]));			(* FAIL  *)
prettyPrintMatchE(match((Concrete(B(true))),[(PVal(B(true)),Value(Concrete(R(1.0))))],[],[],[]));		(* 1.0, [] *)
prettyPrintMatchE(match((Concrete(B(false))),[(PVal(B(true)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(B(true))),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(B(false))),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [] *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));		(* FAIL  *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((va'),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [v['a]->false],   ['a>bool],    [] *)
prettyPrintMatchE(match((va''),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	(* 1.0, [v[''a]->false],  [''a->bool],  [] *)
prettyPrintMatchE(match((va'''),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	(* FAIL *)

prettyPrintMatchE(match(VRecord([(Lab("a"),(Concrete(N(2)))),(Lab("b"),(va'))]),[(PVal(B(false)),Value(Concrete(R(1.0))))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((Concrete(N(2))),[(PRecord([]),Value(VRecord([])))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(B(true))),[(PRecord([]),Value(VRecord([])))],[],[],[]));		(* FAIL *)
prettyPrintMatchE(match((Concrete(R(2.0))),[(PRecord([]),Value(VRecord([])))],[],[],[]));		(* FAIL *)

prettyPrintMatchE(match((Fun(Var("x"),Int,Value(Concrete(N(2))))),[(PRecord([]),Value(VRecord([])))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match((va'),[(PRecord([]),Value(VRecord([])))],[],[],[]));			(* {}, [v['a]->{}], ['a->{}], [] *)
prettyPrintMatchE(match((va''),[(PRecord([]),Value(VRecord([])))],[],[],[]));			(* {}, [v[''a]->{}], [''a->{}], [] *)
prettyPrintMatchE(match((va'''),[(PRecord([]),Value(VRecord([])))],[],[],[]));			(* FAIL*)
prettyPrintMatchE(match(VRecord([]),[(PRecord([]),Value(VRecord([])))],[],[],[]));			(* {}, [] *)
prettyPrintMatchE(match((VRecord([])),[(PRecord([]),Value(VRecord([])))],[],[],[]));	(* {}, [] *)

prettyPrintMatchE(match(VRecord([(Lab("a"),(Concrete(N(2)))),(Lab("b"),(va'))]),[(PRecord([]),Value(VRecord([])))],[],[],[]));	
(* FAIL *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(R(3.0))),(Lab("c"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))])),
	[(PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PWildcard)]),Variable(Var("x")))],
	[],[],[]));
(* x, [x->3.0] *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(R(3.0))),(Lab("c"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))])),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PWildcard)]),Variable(Var("x")))],
	[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(R(3.0))),(Lab("c"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x")))))])),
	[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z")))]),Variable(Var("x")))],
	[],[],[]));
(* x, [x->3.0,y->2,z->fn x:int=>x+x] *)

prettyPrintMatchE(match(
	(va'),
	[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z")))]),Variable(Var("z")))],
	[],[],[]));
(* z,[v['a]->{a=v['a0],b=v['a1],c=v['a2]}], ['a->{a:'a0,b:'a1,c:'a2}], [x->v['a1],y->v['a0],z->v['a2]] *)

prettyPrintMatchE(match(
	(va''),
	[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],
	[],[],[]));
(* x+y,[v[''a]->{a=v[''a0],=:v[''a1],c=v[''a2]}], [''a->{a:''a0,b:''a1,c:''a2}], [x->v[''a1],y->v[''a0],z->v[''a2]] *)

prettyPrintMatchE(match(
	(va'''),
	[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z")))]),
	  Value(Concrete(N(10))))],
	[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	(va'''),
	[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z")))]),
	  Value(Concrete(N(5))))],
	[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	VRecord([(Lab("a"),(Concrete(N(3)))),
			 (Lab("b"),(VRecord([(Lab("1"),Fun(Var("x"),Real,ArithExpr(TIMES,Value(Concrete(R(2.0))),Variable(Var("x"))))),
								 (Lab("2"),Concrete(B(true)))]))),
			(Lab("c"),(va')),
			(Lab("d"),(VHole(BinaryOpHole(ArithOper(PLUS),vb',vc'))))]),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z"))),(Lab("d"),PWildcard)]),
	  App(Variable(Var("z")),Variable(Var("x"))))],
	[],[],[]));
(* z x, [z->v['a], x->{1=fn x:real=>2.0*x, 2=true}] *)

prettyPrintMatchE(match(
	VRecord([(Lab("a"),(Concrete(N(3)))),
			 (Lab("b"),(VRecord([(Lab("1"),Fun(Var("x"),Real,ArithExpr(TIMES,Value(Concrete(R(2.0))),Variable(Var("x"))))),
								 (Lab("2"),Concrete(B(true)))]))),
			 (Lab("c"),(va')),
			 (Lab("d"),(VHole(BinaryOpHole(ArithOper(PLUS),vb',vc'))))]),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z"))),(Lab("d"),PVar(Var("y")))]),
	  Case(Variable(Var("x")),[(PRecord([(Lab("1"),PVar(Var("i"))),(Lab("2"),PVar(Var("j")))]),
							    App(Variable(Var("i")),Variable(Var("z"))))]))],
	[],[],[]));
(* case x of {1=i,2=j} -> i z, [y->v[v['b]+v['c]], z->v['a], x->{1=fn x:real=>2.0*x, 2=true}] *)

prettyPrintMatchE(match(
	(VRecord([
			(Lab("a"),Concrete(N(3))),
			(Lab("b"),VRecord([(Lab("1"),Fun(Var("x"),Real,ArithExpr(TIMES,Value(Concrete(R(2.0))),Variable(Var("x"))))),
									 (Lab("2"),Concrete(B(true)))])),
			(Lab("c"),va'),
			(Lab("d"),VHole(BinaryOpHole(ArithOper(PLUS),vb',vc')))])),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVar(Var("x"))),(Lab("c"),PVar(Var("z"))),(Lab("d"),PVal(N(2)))]),
	  Value(Concrete(N(10))))],
	[],[],[]));
(* HOLE of v[ {a=3, b={1=fn x:real => 2.0*x, 2=true}, c=v['a], d=v[ v['b] + v['c] ]} ] *)

prettyPrintMatchE(match(
	(VRecord[(Lab("a"),VHole(ConditionHole(va',Value(Concrete(N(2))),Value(Concrete(N(3))))))]),
	[(PRecord([(Lab("a"),PVal(N(3)))]),Value(Concrete(N(5))))],[],[],[]));
(* HOLE of v[a=v[if v['a] then 2 else 3]}] *)

prettyPrintMatchE(match(
	(VRecord[(Lab("a"),VHole(ConditionHole(va',Value(Concrete(N(2))),Value(Concrete(N(3)))))),
				  (Lab("b"),VHole(ConditionHole(vb',Value(Concrete(B(true))),Value(Concrete(B(false))))))]),
	[(PRecord([(Lab("a"),PVal(N(3)))]),Value(Concrete(N(5))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	(VRecord[(Lab("a"),VHole(ConditionHole(va',Value(Concrete(N(2))),Value(Concrete(N(3)))))),
				  (Lab("b"),VHole(ConditionHole(vb',Value(Concrete(B(true))),Value(Concrete(B(false))))))]),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVal(B(false)))]),Value(Concrete(N(2))))],[],[],[]));
(* HOLE of v[ {a=v[if v['a] then 2 else 3], b=v[if v['b] then true else false]}]*)

prettyPrintMatchE(match(
	(VRecord[(Lab("a"),VHole(ConditionHole(va',Value(Concrete(N(2))),Value(Concrete(N(3)))))),
				  (Lab("b"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(3))))))]),
	[(PRecord([(Lab("a"),PVal(N(3))),(Lab("b"),PVal(B(false)))]),Value(Concrete(N(1))))],[],[],[]));
(* HOLE of v[ {a=v[if v['a] then 2 else 3], b=fn x:int => x+3}]*)

prettyPrintMatchE(match(
	(VHole(CaseHole(VHole(BinaryOpHole(ArithOper(PLUS),va',vb')),
						 [(PVal(N(3)),Value(Fun(Var("x"),Int,Value(Concrete(N(2))))))]))),
	[(PVal(N(2)),Value(Concrete(R(0.0))))],[],[],[]));
(* HOLE of v[ case v[ v['a] + v['b] ] of 3 -> fn x:int=>2 ] *)

prettyPrintMatchE(match(
	(Concrete(N(3))),
	[(PVal(N(1)),Variable(Var("x"))),
	 (PVal(N(2)),Variable(Var("y"))),
	 (PVal(N(3)),Variable(Var("z"))),
	 (PWildcard,Variable(Var("a")))],[],[],[]));
(* z, [] *)

prettyPrintMatchE(match(
	(Concrete(N(4))),
	[(PVal(N(1)),Variable(Var("x"))),
	 (PVal(N(2)),Variable(Var("y"))),
	 (PVal(N(3)),Variable(Var("z"))),
	 (PWildcard,Variable(Var("a")))],[],[],[]));
(* a, [] *)

prettyPrintMatchE(match(
	(Concrete(N(4))),
	[(PVal(N(1)),Variable(Var("x"))),
	 (PVal(N(2)),Variable(Var("y"))),
	 (PVal(N(3)),Variable(Var("z"))),
	 (PVar(Var("x")),Variable(Var("x")))],[],[],[]));
(* x, [x->4] *)

prettyPrintMatchE(match(
	(Concrete(N(4))),
	[(PVal(N(1)),Variable(Var("x"))),
	 (PVal(N(2)),Variable(Var("y"))),
	 (PVal(N(3)),Variable(Var("z"))),
	 (PVar(Var("x")),Variable(Var("x"))),
	 (PRecord([]),Variable(Var("b")))],[],[],[]));
(* x, [x->4] *)
(* Even though {} not of type int, it will in reality, c.f. below example
   'match' always assumes all the patterns and expression we are case-ing on
   are of the valid types *)
	 
prettyPrintMatchE(match(
	(Concrete(N(4))),
	[(PVal(N(1)),Variable(Var("x"))),
	 (PVal(N(2)),Variable(Var("y"))),
	 (PVal(N(3)),Variable(Var("z"))),
	 (PRecord([]),Variable(Var("b"))),
	 (PVar(Var("x")),Variable(Var("x")))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	(Concrete(B(true))),
	[(PVal(B(false)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(B(true)), ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))),
	 (PVar(Var("x")),ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("y"))))],[],[],[]));
(* x*y, [] *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))])),
	[(PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVal(B(false)))]),Variable(Var("a"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(1)))]),Variable(Var("b"))),
	 (PRecord([(Lab("a"),PVal(N(5))),(Lab("b"),PVal(B(false)))]),Variable(Var("c"))),
	 (PRecord([(Lab("b"),PVal(B(false))),(Lab("a"),PVal(N(1)))]),Variable(Var("d")))],[],[],[]));
(* d, [], *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))])),
	[(PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVal(B(false)))]),Variable(Var("a"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(1)))]),Variable(Var("b"))),
	 (PRecord([(Lab("a"),PVal(N(5))),(Lab("b"),PVal(B(false)))]),Variable(Var("c"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(3)))]),Variable(Var("d"))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))],[],[],[]));
(* x+y, [y->false, x->1], *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))])),
	[(PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVal(B(false)))]),Variable(Var("a"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(1)))]),Variable(Var("b"))),
	 (PRecord([(Lab("a"),PVal(N(5))),(Lab("b"),PVal(B(false)))]),Variable(Var("c"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(3)))]),Variable(Var("d"))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true)))]),Variable(Var("x"))),
	 (PRecord([(Lab("b"),PWildcard),(Lab("a"),PVar(Var("x")))]),Variable(Var("y")))],[],[],[]));
(* y, [x->1], *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))])),
	[(PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVal(B(false)))]),Variable(Var("a"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(1)))]),Variable(Var("b"))),
	 (PRecord([(Lab("a"),PVal(N(5))),(Lab("b"),PVal(B(false)))]),Variable(Var("c"))),
	 (PRecord([(Lab("b"),PVal(B(true))),(Lab("a"),PVal(N(3)))]),Variable(Var("d"))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true)))]),Variable(Var("x"))),
	 (PWildcard,Variable(Var("y")))],[],[],[]));
(* y, [], *)

prettyPrintMatchE(match(
	(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(R(1.0))),
				   (Lab("c"),Concrete(B(true))),
				   (Lab("d"),Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("e"),VRecord([(Lab("i"),Fun(Var("y"),Real,ArithExpr(DIVIDE,Variable(Var("y")),Variable(Var("y"))))),
									  (Lab("j"),VRecord([])),
									  (Lab("k"),Concrete(N(1)))]))])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(B(false))),
			   (Lab("d"),PWildcard),(Lab("e"),PWildcard)]),
	  Record([(Lab("1"),Variable(Var("x"))),(Lab("2"),Variable(Var("y")))])),
	  
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVal(B(true))),
			   (Lab("d"),PVar(Var("z"))),(Lab("e"),PVar(Var("a")))]),
	  Case(Variable(Var("a")),
		   [(PRecord([(Lab("i"),PVar(Var("f"))),(Lab("j"),PRecord([])),(Lab("k"),PVar(Var("e")))]),
		     Record([(Lab("1"),App(Variable(Var("f")),Variable(Var("y")))),
					 (Lab("2"),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("e"))))]))]))],[],[],[]));
(* case a of {i=f,j={},k=e} -> {1=f y, 2 =x+e},
   [a->{i=fn y:real=>y/y, j={},k=1}, z->fn x:int=>z*z, y->1.0, x->1] *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(true)))),
	 (PVal(EmptyList),Value(Concrete(B(false))))],[],[],[]));
(* false *)
	
prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PWildcard,Value(Concrete(B(true))))],[],[],[]));
(* true *)
	 
prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PVar(Var("x")),Value(Concrete(B(true))))],[],[],[])); 
(* true, [x->[]] *)
	
prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)
	
prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PCons(PWildcard,PWildcard),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PVal(N(2)),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PRecord([]),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PRecord([(Lab("a"),PVal(N(1)))]),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PVal(B(true)),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	Concrete(EmptyList),
	[(PVal(R(1.0)),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

(* --- *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PVal(EmptyList),Value(Concrete(B(true))))],[],[],[]));
(* FAIL *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* true, x->0, y->[1] *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PVar(Var("x")),PWildcard),Value(Concrete(B(true))))],[],[],[]));
(* true, x->0 *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PWildcard,PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[1] *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PWildcard,PWildcard),Value(Concrete(B(true))))],[],[],[]));
(* true *)
	
prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PVal(N(0)),PWildcard),Value(Concrete(B(true))))],[],[],[]));
(* true *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PCons(PVal(N(0)),PCons(PVar(Var("x")),PVar(Var("y")))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[],x->1 *)

prettyPrintMatchE(match(
	VList([Concrete(N(0)),Concrete(N(1))]),
	[(PVal(EmptyList),Value(Concrete(B(false)))),
	 (PCons(PVal(N(1)),PVal(EmptyList)),Value(Concrete(B(false)))),
	 (PCons(PVal(N(2)),PVar(Var("x"))),Value(Concrete(B(false)))),
	 (PCons(PVal(N(1)),PWildcard),Value(Concrete(B(false)))),
	 (PCons(PVal(N(1)),PCons(PVal(N(0)),PVar(Var("y")))),Value(Concrete(B(false)))),
	 (PCons(PVal(N(0)),PCons(PVal(N(1)),PVal(EmptyList))),Value(Concrete(B(true))))],[],[],[]));
(* true *)

prettyPrintMatchE(match(
	VList([VList([Concrete(N(1)),Concrete(N(2))]),VList([Concrete(N(3))])]),
	[(PVal(EmptyList),Value(Concrete(B(false)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[[3]], x->[1,2] *)

prettyPrintMatchE(match(
	VList([VList([Concrete(N(1)),Concrete(N(2))]),VList([Concrete(N(3))])]),
	[(PVal(EmptyList),Value(Concrete(B(false)))),
	 (PCons(PCons(PVar(Var("x")),PVar(Var("z"))),PCons(PVar(Var("y")),PVar(Var("k")))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[3],k->[], x->1, z->[2] *)
	
prettyPrintMatchE(match(
	VList([VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))]),
		   VRecord([(Lab("a"),Concrete(N(10))),(Lab("b"),Concrete(B(false)))])]),
	[(PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[{a=10,b=false}], x->{a=1,b=false} *)

prettyPrintMatchE(match(
	VList([VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(false)))]),
		   VRecord([(Lab("a"),Concrete(N(10))),(Lab("b"),Concrete(B(false)))])]),
	[(PCons(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(false)))]),
			PVar(Var("y"))),Value(Concrete(B(true))))],[],[],[]));
(* true, y->[{a=10,b=false}], x->1 *)
	
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml";  *)