(* ----------------------------------------------------------------------------------- *)
(* TETS CASES FOR SUBSTITUTION STRUCTURE *)

val l = [ (1,"a"), (2,"b"), (3,"c"), (4,"d") ];
Substitution.contains(1,l);  (* true  *)
Substitution.contains(3,l);  (* true  *)
Substitution.contains(5,l);  (* false *)
val l = Substitution.union(l,5,"e"); (* [(5,"e"),(1,"a"),(2,"b"),(3,"c"),(4,"d")] *)
Substitution.get(1,l);		 (* "a" *)
Substitution.get(5,l);       (* "e" *)
val l = Substitution.update(l,5,"f");(* [(5,"f"),(1,"a"),(2,"b"),(3,"c"),(4,"d")] *)
Substitution.get(5,l);		 (* "f" *)
Substitution.domain(l);		 (* [5,1,2,3,4] *)
Substitution.range(l);		 (* ["f","a","b","c","d"] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR RESOLVE CHAIN *)

val a = (ValueHole(TypeVar("a")));
val b = (ValueHole(TypeVar("b")));
val c = (ValueHole(TypeVar("c")));
val d = (ValueHole(TypeVar("d")));

val sigma = [ (a,VHole(SimpleHole(b))), (b,VHole(SimpleHole(c))), (c,N(3)) ];

resolveChainSigma(VHole(SimpleHole(a)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(b)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(c)),sigma); (* 3 *)

val sigma = [ (a,VHole(SimpleHole(b))), (b,ValuePair(VHole(SimpleHole(c)),VHole(SimpleHole(d)))), 
			  (c,N(3)), (d,B(true)) ];

resolveChainSigma(VHole(SimpleHole(a)),sigma); (* (3,true) *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR LIST HELPERS *)
 
val l = [ (1,2), (3,4), (5,6)];
replace(l,1,5);	(* [(5, 2), (3, 4), (5, 6)] *)
replace(l,2,6); (* [(1, 6), (3, 4), (5, 6)] *)
replace(l,3,7); (* [(1, 2), (7, 4), (5, 6)] *)
replace(replace(replace(l,2,1),4,3),6,5); (* [(1, 1), (3, 3), (5, 5)] *)

append([],[1,2,3]);	     (* [1,2,3] *)
append([1,2,3],[]);		 (* [1,2,3] *)
append([1,2,3],[4,5,6]); (* [1,2,3,4,5,6] *)

union([],[1,2,3]);	     (* [1,2,3] *)
union([1,2,3],[1]);		 (* [2,3,1] *)
union([1,2,3],[1,2,3]);  (* [1,2,3] *)
union([1,2,3],[4,5,6]);  (* [1,2,3,4,5,6] *)
union([1,2,3],[4,5,6,1,2,3]);  (* [4,5,6,1,2,3] *)

remove([1,2,3],[1]);		 	 (* [2,3]	  *)
remove([1,1,1],[1]);		 	 (* []        *)
remove([1,2,3],[4]);		 	 (* [1,2,3]	  *)
remove([1,2,3,1,2,3],[1]);   	 (* [2,3,2,3] *)
remove([1,2,3,4],[1,5]);	 	 (* [2,3,4]   *)
remove([1,2,3,4],[1,2,3]);	 	 (* [4] 	  *)
remove([1,2],[5]);			 	 (* [1,2]  	  *)
remove([1,2,3,1,2,3],[1,2]); 	 (* [3,3] 	  *)
remove([1,2,3,1,2,3],[1,2,3,4]); (* [] 		  *)

element([1,2,3],1);		 (* true *)
element([],1);			 (* false *)
element([1],1);			 (* true *)
element([1],2);			 (* false *)
element([1,2,3,4,5],6);	 (* false *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR SUBSTITUTE AND ALPHAVARIANT *)

val sub = [ (Var("x"),Value(N(3))) ];

fun prettyPrintE(e) = prettyPrintExpression(Expression(e));

prettyPrintE(substitute(Value(N(4)),sub)); (* 4 *)
prettyPrintE(substitute(Variable(Var("x")),sub)); (* 3 *)
prettyPrintE(substitute(ArithExpr(PLUS,Value(N(3)),Value(N(4))),sub)); (* 3 + 4 *)
prettyPrintE(substitute(ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),sub)); (* 3 + 3 *)
prettyPrintE(substitute(Condition(Value(B(true)),Variable(Var("x")),
								 ArithExpr(PLUS,Variable(Var("x")),Value(N(1)))),sub)); 
(* if true then 3 else 3 + 1 *)

prettyPrintE(substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("y"),Var("z")),
			   ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z"))))),
sub)); 
(* case (3,4) of (y,z) -> 3 + y + z *)
	
prettyPrintE(substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("x"),Var("z")),
			   ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z"))))),
sub)); 
(* case (3,4) of (x0,z0) -> x0 + x0 + z0 *)
	
val sub = [ (Var("x"),Value(N(3))), (Var("y"),Value(N(4))) ];

prettyPrintE(substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("x"),Var("z")),
			   ArithExpr(SUBTRACT,Variable(Var("y")),ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("z"))))),
sub)); (* case (3,4) of (x1,z1) -> 4 - (x1/z1) *)

prettyPrintE(substitute(ArithExpr(PLUS,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
			   Case(Value(ValuePair(N(1),N(2))),
			   VariablePair(Var("x"),Var("y")),
			   ArithExpr(SUBTRACT,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
					Case(Value(ValuePair(N(1),N(2))),
					VariablePair(Var("x"),Var("y")),
					ArithExpr(SUBTRACT,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))),
							 Case(Value(ValuePair(N(1),N(2))),
							 VariablePair(Var("x"),Var("y")),
							 BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))))))),sub));
(* 3*4 + case (1,2) of (x2,y2) -> x2*y2 - case (1,2) of (x2,y2) -> x2*x2 - case (1,2) of (x2,y2) -> x2=y2 *)

prettyPrintE(substitute(
Case(
	Case(ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
		 VariablePair(Var("x"),Var("y")),
		 ExpressionPair(ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))),
	VariablePair(Var("x"),Var("y")),
	ArithExpr(SUBTRACT,Variable(Var("x")),Variable(Var("y")))),sub));
						
(* (case (case (x,y) of (x,y) -> (x+y,x*y)) of (x,y) -> x-y)[3/x][4/y]
   => 
   case (case (3,4) of (x4,y4)->(x4+y4,x4*y4)) of (x3,y3) -> x3-y3
*)

val va'   = SimpleHole(ValueHole(TypeVar("a")));
val vb'   = SimpleHole(ValueHole(TypeVar("b")));
val va''  = SimpleHole(ValueHole(EqualityTypeVar("a")));
val vb''  = SimpleHole(ValueHole(EqualityTypeVar("b")));
val va''' = SimpleHole(ValueHole(ArithTypeVar("a")));
val vb''' = SimpleHole(ValueHole(ArithTypeVar("b")));

prettyPrintE(substitute(Value(ValuePair(N(3),R(3.0))),[])); (* (3,3.0) *)
prettyPrintE(substitute(Value(VHole(va')),[])); (* v['a] *)
prettyPrintE(substitute(Value(ValuePair(VHole(va'),VHole(vb'))),[])); (* (v['a],v['b]) *)
prettyPrintE(substitute(Value(VHole(BinaryOp(EXPR_PAIR,va',vb'))),[])); (* v[('a,'b)] *)
prettyPrintE(substitute(Value(VHole(BinaryOp(ArithOper(PLUS),va',vb'))),[])); (* v['a+'b] *)
prettyPrintE(substitute(ArithExpr(PLUS,Value(VHole(va')),Value(VHole(vb'))),[])); (* v['a]+v['b] *)
prettyPrintE(substitute(Value(VHole(ConditionHole(va',
		   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		   BoolExpr(EQ,
					BoolExpr(LESS,Variable(Var("x")),Variable(Var("y"))),
					BoolExpr(MORE,Variable(Var("x")),Variable(Var("y"))))))),sub));
(* v[if 'a then 3+4 else (3<4)=(3>4)] *)

prettyPrintE(substitute(Value(VHole(CaseHole(va',
		   VariablePair(Var("x"),Var("z")),
		   ArithExpr(PLUS,Variable(Var("x")),
					 ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z"))))))),sub));
(* v[case 'a of (x0,z0) -> x0 + 4 + z0 *)

prettyPrintE(substitute(Value(VHole(CaseHole(
	CaseHole(va',VariablePair(Var("x"),Var("y")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
	VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),sub));
(* v[case (case 'a of (x,y)->x+y) of (x,y) -> x+y]
   =>
   v[case (case 'a of (x0,y0)->x0+y0) of (x1,y1)->x1+y1]
*)   

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR FREE VARIABLES *)

(* Tests free variables of a single expression *)
fvExpr(Variable(Var("x"))); (* [ x ] *)
fvExpr(ArithExpr(PLUS,Value(N(3)),Value(N(3)))); (* [ ] *)
fvExpr(BoolExpr(EQ,Variable(Var("y")),Value(N(3)))); (* [ y ] *)
fvExpr(ArithExpr(TIMES,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
					   ArithExpr(PLUS,Variable(Var("a")),Variable(Var("b"))))); (* [ x y a b ] *)
fvExpr(ArithExpr(TIMES,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),
					   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))); (* [ x ] *)
fvExpr(Value(N(3))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			Value(B(true)))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			Variable(Var("x")))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			Variable(Var("y")))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			Variable(Var("z")))); (* [ z ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			ArithExpr(PLUS,Variable(Var("z")),Variable(Var("a"))))); (* [ a, z ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("x"),Var("y")),
			ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))); (* [ ] *)
		
fvExpr(Value(VHole(va'))); (* [ ] *)
fvExpr(Value(ValuePair(VHole(va'),VHole(vb')))); (* [ ] *)
fvExpr(Value(ValuePair(N(3),N(4)))); (* [ ] *)
fvExpr(Value(VHole(BinaryOp(ArithOper(PLUS),va',vb'))));
fvExpr(Value(VHole(BinaryOp(EXPR_PAIR,va''',vb''))));
fvExpr(Value(VHole(CaseHole(
	CaseHole(va',VariablePair(Var("x"),Var("y")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z")))),
	VariablePair(Var("a"),Var("b")),
	ExpressionPair(ExpressionPair(Variable(Var("a")),Variable(Var("c"))),
				   ExpressionPair(Variable(Var("d")),Variable(Var("d"))))))));
(* [ z, c, d] *)
fvExpr(Value(VHole(ConditionHole(
	CaseHole(va',VariablePair(Var("x"),Var("y")),Value(VHole(va'))),
	Value(N(3)),Value(N(4))))));
(* [ ] *)

(* Tests free variables of a list of expressions *)
fv( [Variable(Var("x")),
	 ArithExpr(PLUS,Value(N(3)),Variable(Var("y"))),
	 Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("a"),Var("b")),
			ArithExpr(PLUS,Variable(Var("b")),Variable(Var("a")))),
	 Condition(Variable(Var("x")),Value(B(true)),BoolExpr(EQ,Variable(Var("k")),Value(N(4)))) ]);
(* [ y, x, k ] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR GEN *)

gen(Bool,[]); (* true *)
gen(Int,[]);  (* 1    *)
gen(Real,[]); (* 1.0  *)
gen(Pair(Int,Int),[]); (* (1,1) *)
gen(Pair(Pair(Int,Real),Pair(Bool,Int)),[]); (* ((1,1.0),(true,1)) *)
gen(THole(TypeHole(TypeVar("a"))),[]); (* v['a] *)
gen(THole(TypeHole(TypeVar("a"))),[ (TypeHole(TypeVar("a")),Int) ]); (* 1 *)
gen(THole(TypeHole(TypeVar("a"))),
	[(TypeHole(TypeVar("a")),Pair(THole(TypeHole(TypeVar("b"))),THole(TypeHole(ArithTypeVar("a"))))),
	 (TypeHole(TypeVar("b")),Int),
	 (TypeHole(ArithTypeVar("a")),Pair(Int,Int))]);
(* (1, (1,1)) *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR TYPEOF *)

fun prettyPrintTypeOf(NONE,_) = "FAIL"
|	prettyPrintTypeOf(SOME t,theta) = 
	prettyPrintType(t) ^ ", theta = [ " ^ prettyPrintTheta(theta) ^ " ]";

(* Concrete values *)
prettyPrintTypeOf(typeof(N(3),[]));		(* Int *)
prettyPrintTypeOf(typeof(B(true),[]));	(* Bool *)
prettyPrintTypeOf(typeof(R(3.0),[]));	(* Real *)
prettyPrintTypeOf(typeof(ValuePair(N(3),N(4)),[]));			(* (Int,Int) *)
prettyPrintTypeOf(typeof(ValuePair(B(true),R(5.0)),[]));	(* (Bool,Real) *)

(* Simple value holes *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(TypeVar("a")))),[]));			(* 'a   *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(EqualityTypeVar("a")))),[]));	(* ''a  *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(ArithTypeVar("a")))),[]));		(* '''a *)

(* Complex value holes *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),va',vb')),[]));	 		(* '''a0, [  'a->'''a0,   'b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),va''',vb''')),[]));	(* '''a0, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(SUBTRACT),va''',vb'')),[]));	(* Int,   ['''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),[])); 		(* Real,  [  'a->Real,    'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va'',vb')),[]));		(* FAIL 							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb')),[]));		(* Real,  [ '''a->Real,   'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb'')),[]));	(* FAIL  							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),va''',va''')),[]));		(* '''a,  [ ]						  *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va',vb')),[]));			(* Bool, [  'a->''a0,    'b->''a0 ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va''',vb''')),[]));		(* Bool, ['''a->Int,   '''b->Int  ]	 *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va'',vb''')),[]));			(* Bool, ['''a->Int,   '''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS),va',vb')),[]));			(* Bool, [  'a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE),va'',vb''')),[]));		(* Bool, [ ''a->Int,  '''b->Int   ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS_EQ),va''',vb''')),[]));	(* Bool, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',vb'')),[]));		(* Bool, [ ''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',va'')),[]));		(* Bool, [ ''a->Int ]  				 *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS),
	BinaryOp(ArithOper(PLUS),va',vb'),
	BinaryOp(ArithOper(SUBTRACT),va',vb'))),[])); 
(* 
   v[ ('a+'b) < ('a-'b) ]
   =>
   Bool, ['a->'''a0, 'b->'''a0] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),
	BinaryOp(ArithOper(DIVIDE),va',vb'),
	BinaryOp(ArithOper(PLUS),va',vb'))),[]));
(* 
   v[ ('a/'b) * ('a+'b) ]
   =>
   Real, ['a->Real, 'b->Real] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),
	BinaryOp(ArithOper(DIVIDE),va',vb'),
	BinaryOp(BoolOper(EQ),va'',vb''))),[]));
(* 
   v[ ('a/'b) * (''a=''b) ]
   =>
   FAIL 
*)	

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	BinaryOp(ArithOper(PLUS),va''',vb''),
	BinaryOp(ArithOper(TIMES),va',vb'))),[]));
(* 
   v[ ('''a+''b) / ('a*'b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	BinaryOp(ArithOper(PLUS),va''',vb''),
	BinaryOp(ArithOper(TIMES),va''',vb''))),[]));
(* 
   v[ ('''a+''b) / ('''a*''b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),
	BinaryOp(ArithOper(PLUS),
		BinaryOp(ArithOper(PLUS),va',va'),
		BinaryOp(ArithOper(PLUS),va',va')),
	BinaryOp(ArithOper(PLUS),
		BinaryOp(ArithOper(PLUS),va',va'),
		BinaryOp(ArithOper(DIVIDE),vb',vb')))),[]));
(*
   v[ ( ('a+'a) + ('a+'a) ) + ( ('a+'a) + ('b/'b) ) ]
   =>
   Real, ['a->'''a0, '''a0->Real, 'b->Real]
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(EXPR_PAIR,va',vb')),[]));
(* 'a * 'b *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(EXPR_PAIR,
	BinaryOp(EXPR_PAIR,va''',vb'''),
	BinaryOp(EXPR_PAIR,va'',vb''))),[]));
(* ('''a * '''b) * (''a * ''b) *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),
	BinaryOp(EXPR_PAIR,va''',vb'''),
	BinaryOp(EXPR_PAIR,va'',vb''))),[]));
(* Bool, ['''a->Int, '''b->Int, ''b->Int, ''a->Int] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(N(3)),Value(N(4)))),[]));
(* Int, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(N(3)),Value(R(4.0)))),[]));
(* 'a0, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(
	BinaryOp(BoolOper(LESS),va',vb'),
	Value(N(3)),Value(N(4)))),[]));
(* Int, ['a->'''a0, 'b->'''a0] *)
	
prettyPrintTypeOf(typeof(VHole(ConditionHole(
	BinaryOp(EXPR_PAIR,va''',vb'''),
	Value(N(3)),Value(N(4)))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va'',Value(N(3)),Value(N(4)))),[]));
(* Int, [''a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va''',Value(N(3)),Value(N(4)))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),Value(N(3)))),[]));
(* 
   v[case 'a of (x,y) -> 3]
   =>
   Int, ['a->('a0,'a1)] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(vb',VariablePair(Var("x"),Var("y")),Variable(Var("x")))),[]));
(* 
   v[case 'b of (x,y) -> x]
   =>
   'a0, ['b->('a0,'a1)] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),[]));
(* 
   v[case 'a of (x,y) -> x+y]
   =>
   '''a2, ['a->('a0,'a1), 'a0->'''a2, 'a1->'''a2] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va''',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),
		ArithExpr(PLUS,Variable(Var("y")),Value(N(3)))))),[]));
(* v[case '''a of (x,y) -> x+(y+3)]
   =>
   Int, ['''a->('''a0,'''a1), '''a0->Int, '''a1->Int]
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	BoolExpr(EQ,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[]));
(* 
   v[case ''a of (x,y) -> (x+y)=(x*y)]
   =>
   Bool, [''a->(''a0,''a1), ''a0->Int, ''a1->Int] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(BinaryOp(EXPR_PAIR,va''',vb'''),VariablePair(Var("x"),Var("y")),
	BoolExpr(LESS,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[]));
(*
   v[case ('''a,'''b) of (x,y) -> (x+y)<(x*y)]
   =>
   Bool, ['''a->'''a0,'''b->'''a0]
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))),[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[]));
(*
   v[case 'a of (x,y) -> case (3,4) of (x,y) -> x+y]
   =>
   Int, ['a->('a0,'a1)]
*)

prettyPrintTypeOf(typeof(VHole(ConditionHole(
	CaseHole(va',VariablePair(Var("x"),Var("y")),Value(VHole(va'))),
	Value(N(3)),Value(N(4)))),[]));
(* FAIL *)

(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)

prettyPrintTypeOf(typeofexpr(ArithExpr(PLUS,Value(N(3)),Value(N(4))),[],[])); 			(* Int *) 
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(R(5.0))),[],[])); 	(* Real *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(R(3.0)),Value(N(3))),[],[])); 		(* FAIL *)
prettyPrintTypeOf(typeofexpr(ArithExpr(TIMES,Value(R(3.0)),Value(R(5.0))),[],[])); 		(* Real *)
prettyPrintTypeOf(typeofexpr(ArithExpr(SUBTRACT,Value(B(true)),Value(R(3.0))),[],[])); 	(* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(LESS,Value(N(3)),Value(N(5))),[],[])); 		(* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(MORE,Value(R(3.0)),Value(R(6.0))),[],[])); 	(* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(N(3)),Value(N(5))),[],[])); 			(* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(ValuePair(N(3),B(true))),Value(ValuePair(N(6),B(false)))),[],[])); (* Bool *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(R(3.0)),Value(R(5.0))),[],[])); 		(* FAIL *)

prettyPrintTypeOf(typeofexpr(ArithExpr(PLUS,Value(N(3)),Value(VHole(va'))),[],[])); 	(* Int, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(VHole(vb')),Value(R(5.0))),[],[])); (* Real, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(TIMES,Value(VHole(va''')),Value(VHole(vb''))),[],[])); 	(* Int, ['''a->Int, ''b->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(VHole(vb'')),Value(VHole(va'''))),[],[]));  (* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(LESS,Value(N(3)),Value(VHole(va'))),[],[])); 			(* Bool, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(LESS_EQ,Value(VHole(vb')),Value(R(5.0))),[],[])); 	(* Bool, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(VHole(va''')),Value(VHole(vb''))),[],[])); 	(* Bool, [''b->Int,'''a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(VHole(vb''')),Value(VHole(va'''))),[],[])); 	(* Bool, ['''a->Int,'''b->Int]  *)

prettyPrintTypeOf(typeofexpr(ExpressionPair(Value(N(3)),Value(VHole(va'))),[],[])); (* Int * 'a *)
prettyPrintTypeOf(typeofexpr(ExpressionPair(
	ArithExpr(TIMES,Value(VHole(va''')),Value(VHole(vb''))),
	BoolExpr(LESS_EQ,Value(VHole(vb')),Value(R(5.0)))),[],[])); 
(* Int * Bool, ['b->Real, ''b->Int, '''a -> Int] *)
	
prettyPrintTypeOf(typeofexpr(Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),Variable(Var("x"))),[],[]));
(* case (3,4) of (x,y) -> x => Int *)

prettyPrintTypeOf(typeofexpr(Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),[],[]));
(* case (3,4) of (x,y) -> x+y => Int *)

prettyPrintTypeOf(typeofexpr(Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),
		ArithExpr(PLUS,Variable(Var("y")),Value(N(3))))),[],[]));
(* case (3,4) of (x,y) -> x+(y+3) =>Int *)

prettyPrintTypeOf(typeofexpr(Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
	BoolExpr(EQ,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(* case (3,4) of (x,y) -> (x+y)=(x*y) => Bool *)

prettyPrintTypeOf(typeofexpr(Case(Value(VHole(BinaryOp(EXPR_PAIR,va''',vb'''))),VariablePair(Var("x"),Var("y")),
	BoolExpr(LESS,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(*
   case v[('''a,'''b)] of (x,y) -> (x+y)<(x*y)]
   =>
   Bool, ['''a->'''a0,'''b->'''a0]
*)

prettyPrintTypeOf(typeofexpr(Case(Value(ValuePair(N(3),R(5.0))),VariablePair(Var("x"),Var("y")),
	ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y")))),[],[]));
(* FAIL *)

prettyPrintTypeOf(typeofexpr(Case(Value(VHole(va')),VariablePair(Var("x"),Var("y")),
	Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(* 
   case v['a] of (x,y) -> case (3,4) of (x,y) -> x+y 
   => 
   Int, ['a->'a0*'a1] 
*)

prettyPrintTypeOf(typeofexpr(Case(Value(VHole(va')),VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS, Variable(Var("x")),
		Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
			ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[],[]));
(* 
   case v['a] of (x,y) -> x + (case (3,4) of (x,y) -> x+y)
   => 
   Int, ['a->'a0*a'1, 'a0->Int] 
*)

prettyPrintTypeOf(typeofexpr(Case(Value(VHole(va')),VariablePair(Var("x"),Var("y")),
	BoolExpr(EQ,
		BoolExpr(EQ,	
			ArithExpr(PLUS, Variable(Var("x")),
				Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
					ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),
		Value(N(3))),
	Variable(Var("y")))),[],[]));
(* 
   case v['a] of (x,y) -> ((x + (case (3,4) of (x,y) -> x+y))=3) = y
   => 
   Bool, ['a->'a0*'a1, 'a0->Int, 'a1->Bool] 
*)


(* ----------------------------------------------------------------------------------- *)
(* TETS CASES FOR UNIFY *)

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

fun prettyPrintUnifyTest(constraints,map) =

	let val (theta,b) = unify(constraints,map)
	in if b then "[" ^ prettyPrintTheta(theta) ^ "]" else "false" end;

(* test outputs assume true, unless stated otherwise *)
prettyPrintUnifyTest( [Int,Int], []);	(* [] *)
prettyPrintUnifyTest( [a',Int], []);	(* [ ('a -> Int) ] *)
prettyPrintUnifyTest( [a''',Real], []);	(* [ ('''a -> Real) ] *)
prettyPrintUnifyTest( [a''',Bool], []);	(* false *)
prettyPrintUnifyTest( [a'',Real], []);	(* false *)
prettyPrintUnifyTest( [a',b'], []);		(* [ ('a -> 'b) ] *)
prettyPrintUnifyTest( [a''',b'''], []);	(* [ ('''a -> '''b) ] *)
prettyPrintUnifyTest( [a'',b'''], []);	(* [ (''a -> Int), ('''b -> Int) ] *)
prettyPrintUnifyTest( [a',b',c'], []); 	(* [ ('a -> 'c), ('b -> 'c) ] *)
prettyPrintUnifyTest( [a',b',Int], []); (* [ ('a -> Int'), ('b -> Int)] *)
prettyPrintUnifyTest( [a',b'',c'''],[]);(* [ ('a -> Int), (''b -> Int), ('''c -> Int) ] *)
prettyPrintUnifyTest( [Int,Int,Int],[]);(* [] *)
prettyPrintUnifyTest( [a',Int], [(a'1,Int)]);	(* [ ('a -> Int) ] *)
prettyPrintUnifyTest( [a',Int], [(a'1,Real)]);  (* false *)
prettyPrintUnifyTest( [a',b',c'], [(a'1,Int)]);	(* [ ('b -> Int), ('c -> Int), ('a -> Int) ] *)
prettyPrintUnifyTest( [a',b',c'], [(a'1,Int),(b'1,Real)]);  (* false *)
prettyPrintUnifyTest( [a',b'',c'''], [(a'1,Int)]);			(* [ (('b -> Int), ('c -> Int) ] *)
prettyPrintUnifyTest( [a',b'',c'''], [(a'1,Int), (b''1,Int)]);  (* [ ('a -> Int), ('b -> Int), ('''c -> Int) ] *)
prettyPrintUnifyTest( [a',b'',c'''], [(a'1,Real)]); 		(* false *)
prettyPrintUnifyTest( [Pair(Int,Int),Pair(Int,Int)], []);  	(* [] *)
prettyPrintUnifyTest( [Pair(a',Int),Pair(Real,b')], []);	(* [ ('a -> Real), ('b -> Int) ] *)
prettyPrintUnifyTest( [a',Pair(Real,Real)], []);			(* [ ('a -> Real * Real) ] *)
prettyPrintUnifyTest( [a',Pair(a',Int),Pair(Real,b')], []);	(* false *)
prettyPrintUnifyTest( [a',Pair(c',Int),Pair(Real,b')], []);	(* [ 'a -> Real * Int, 'c -> Real, 'b -> Int *)
prettyPrintUnifyTest( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Int], []); (* false *)
prettyPrintUnifyTest( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Pair(Pair(a'',a'''),Pair(Bool,d'))], []);
	(* ['a -> ( (Int * Int) * (Bool * 'd) ), '''c -> Int, ''a -> Int, '''a -> Int, 'b -> 'd ] *)
prettyPrintUnifyTest( [a',Pair(Int,Real)], [(a'1,Int)]);  (* false *)
prettyPrintUnifyTest( [a',Pair(b',c')], [(b'1,Int),(c'1,Int)]); (* [ 'a -> Int * Int, 'b -> Int, 'c -> Int ] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR NARROW *)
(* will use unify in its implementation, so also further tests for this *)

(* basic primitive values: int, real, bool *)
prettyPrintConfig(narrow(N(3),Int,[],[]));		(* 3,     [], [] *)
prettyPrintConfig(narrow(R(3.0),Real,[],[]));	(* 3.0,   [], [] *)
prettyPrintConfig(narrow(N(3),Real,[],[]));		(* Stuck, [], [] *)
prettyPrintConfig(narrow(B(true),Bool,[],[]));	(* true,  [] ,[] *)
prettyPrintConfig(narrow(B(false),Int,[],[]));	(* Stuck, [], [] *)

(* tests for pairs *)
prettyPrintConfig(narrow(ValuePair(N(3),N(4)),Pair(Int,Int),[],[])); (* (3,4), [], [] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Real,Bool),[],[])); (* (3.0,true), [], [] *)
prettyPrintConfig(narrow(ValuePair(R(3.0),B(true)),Pair(Bool,Real),[],[])); (* (Stuck, [], [] *)

(* tests for value holes *)
val va' = VHole(ValueHole(TypeVar("a")));
val vb' = VHole(ValueHole(TypeVar("b")));

val va'' = VHole(ValueHole(EqualityTypeVar("a")));
val vb'' = VHole(ValueHole(EqualityTypeVar("b")));

val va''' = VHole(ValueHole(ArithTypeVar("a")));
val vb''' = VHole(ValueHole(ArithTypeVar("b")));

prettyPrintConfig(narrow(va',Int,[],[])); 	(* 1, [v['a]->1],   ['a->Int]   *)
prettyPrintConfig(narrow(va'',Int,[],[]));	(* 1, [v[''a]->1],  [''a->Int]  *)
prettyPrintConfig(narrow(va''',Int,[],[]));	(* 1, [v['''a]->1], ['''a->Int] *)
prettyPrintConfig(narrow(va''',Bool,[],[]));(* Stuck, [], [] *)
prettyPrintConfig(narrow(va',Pair(Int,Int),[],[])); (* (1,2), [v['a]->(1,1)], ['a->Int*Int] *)
prettyPrintConfig(narrow(va',Pair(b',c'),[],[]));	(* (v['b],v['c]), [v['a]->(v['b],v['c])], ['a -> 'b * 'c] *)
prettyPrintConfig(narrow(va',Pair(a',b'),[],[]));	(* Occurs check: Stuck, [], [] *)
prettyPrintConfig(narrow(va''',b'',[],[]));			(* 1, [v['''a]->1], ['''a->Int,''b->Int] *)