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

(Expression(substitute(Value(N(4)),sub))); (* 4 *)
(Expression(substitute(Variable(Var("x")),sub))); (* 3 *)
(Expression(substitute(ArithExpr(PLUS,Value(N(3)),Value(N(4))),sub))); (* 3 + 4 *)
(Expression(substitute(ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),sub))); (* 3 + 3 *)
(Expression(substitute(Condition(Value(B(true)),Variable(Var("x")),
								 ArithExpr(PLUS,Variable(Var("x")),Value(N(1)))),sub))); 
(* if true then 3 else 3 + 1 *)
						
(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("y"),Var("z")),
			   ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (y,z) -> 3 + y + z *)
	
(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("x"),Var("z")),
			   ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (x0,z0) -> x0 + x0 + z0 *)
	
val sub = [ (Var("x"),Value(N(3))), (Var("y"),Value(N(4))) ];

(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   VariablePair(Var("x"),Var("z")),
			   ArithExpr(SUBTRACT,Variable(Var("y")),ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (x1,z1) -> 4 - (x1/z1) *)

(Expression(
	substitute(ArithExpr(PLUS,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
			   Case(Value(ValuePair(N(1),N(2))),
			   VariablePair(Var("x"),Var("y")),
			   ArithExpr(SUBTRACT,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
					Case(Value(ValuePair(N(1),N(2))),
					VariablePair(Var("x"),Var("y")),
					ArithExpr(SUBTRACT,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))),
							 Case(Value(ValuePair(N(1),N(2))),
							 VariablePair(Var("x"),Var("y")),
							 BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))))))),sub)));
(* 3*4 + case (1,2) of (x2,y2) -> x2*y2 - case (1,2) of (x2,y2) -> x2*x2 - case (1,2) of (x2,y2) -> x2=y2 *)

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
			
(* Tests free variables of a list of expressions *)
fv( [Variable(Var("x")),
	 ArithExpr(PLUS,Value(N(3)),Variable(Var("y"))),
	 Case(Value(ValuePair(N(3),N(3))),
			VariablePair(Var("a"),Var("b")),
			ArithExpr(PLUS,Variable(Var("b")),Variable(Var("a")))),
	 Condition(Variable(Var("x")),Value(B(true)),BoolExpr(EQ,Variable(Var("k")),Value(N(4)))) ]);
(* [ y, x, k ] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR EXPRESSION TO VALUE *)
(*
exprToValHole(Value(N(3)));
exprToValHole(Value(R((3.0))));
exprToValHole(Value(B(true)));
exprToValHole(Value(ValuePair(N(3),B(true))));
exprToValHole(Value(ValuePair(R(3.0),N(4))));
*)

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

(* Concrete values *)
typeof(N(3),[]);						(* Int *)
typeof(B(true),[]);						(* Bool *)
typeof(R(3.0),[]);						(* Real *)
typeof(ValuePair(N(3),N(4)),[]);		(* (Int,Int) *)
typeof(ValuePair(B(true),R(5.0)),[]);	(* (Bool,Real) *)

(* Simple value holes *)
typeof(VHole(SimpleHole(ValueHole(TypeVar("a")))),[]);				(* 'a   *)
typeof(VHole(SimpleHole(ValueHole(EqualityTypeVar("a")))),[]);		(* ''a  *)
typeof(VHole(SimpleHole(ValueHole(ArithTypeVar("a")))),[]);			(* '''a *)

val va'   = SimpleHole(ValueHole(TypeVar("a")));
val vb'   = SimpleHole(ValueHole(TypeVar("b")));
val va''  = SimpleHole(ValueHole(EqualityTypeVar("a")));
val vb''  = SimpleHole(ValueHole(EqualityTypeVar("b")));
val va''' = SimpleHole(ValueHole(ArithTypeVar("a")));
val vb''' = SimpleHole(ValueHole(ArithTypeVar("b")));

(* Complex value holes *)
typeof(VHole(BinaryOp(ArithOper(PLUS),va',vb')),[]);	 	(* '''a3, [  'a->'''a3,   'b->'''a3]  *)
typeof(VHole(BinaryOp(ArithOper(TIMES),va''',vb''')),[]);	(* '''a4, ['''a->'''a4, '''b->'''a4]  *)
typeof(VHole(BinaryOp(ArithOper(SUBTRACT),va''',vb'')),[]);	(* Int,   ['''a->Int,    ''b->Int  ]  *)
typeof(VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),[]); 		(* Real,  [  'a->Real,    'b->Real ]  *)
typeof(VHole(BinaryOp(ArithOper(DIVIDE),va'',vb')),[]);		(* FAIL 							  *)
typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb')),[]);	(* Real,  [ '''a->Real,   'b->Real ]  *)
typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb'')),[]);	(* FAIL  							  *)
typeof(VHole(BinaryOp(ArithOper(PLUS),va''',va''')),[]);	(* '''a,  [ ]						  *)

typeof(VHole(BinaryOp(BoolOper(EQ),va',vb')),[]);			(* Bool, [  'a->''a5,    'b->''a5 ]  *)
typeof(VHole(BinaryOp(BoolOper(EQ),va''',vb''')),[]);		(* Bool, []	 						 *)
typeof(VHole(BinaryOp(BoolOper(EQ),va'',vb''')),[]);		(* Bool, [] 						 *)
typeof(VHole(BinaryOp(BoolOper(LESS),va',vb')),[]);			(* Bool, [  'a->'''a6, '''b->'''a6]  *)
typeof(VHole(BinaryOp(BoolOper(MORE),va'',vb''')),[]);		(* Bool, [ ''a->Int,  '''b->Int   ]  *)
typeof(VHole(BinaryOp(BoolOper(LESS_EQ),va''',vb''')),[]);	(* Bool, ['''a->'''a7, '''b->'''a7]  *)
typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',vb'')),[]);	(* Bool, [ ''a->Int,    ''b->Int  ]  *)
typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',va'')),[]);	(* Bool, [ ''a->Int ]  				 *)

typeof(VHole(BinaryOp(BoolOper(LESS),
	BinaryOp(ArithOper(PLUS),va',vb'),
	BinaryOp(ArithOper(SUBTRACT),va',vb'))),[]); 
(* 
   v[ ('a+'b) < ('a-'b) ]
   =>
   Bool, ['a->'''a8, 'b->'''a8] 
*)

typeof(VHole(BinaryOp(ArithOper(TIMES),
	BinaryOp(ArithOper(DIVIDE),va',vb'),
	BinaryOp(ArithOper(PLUS),va',vb'))),[]);
(* 
   v[ ('a/'b) * ('a+'b) ]
   =>
   Real, ['a->Real, 'b->Real] 
*)

typeof(VHole(BinaryOp(ArithOper(TIMES),
	BinaryOp(ArithOper(DIVIDE),va',vb'),
	BinaryOp(BoolOper(EQ),va'',vb''))),[]);
(* 
   v[ ('a/'b) * (''a=''b) ]
   =>
   FAIL 
*)	

typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	BinaryOp(ArithOper(PLUS),va''',vb''),
	BinaryOp(ArithOper(TIMES),va',vb'))),[]);
(* 
   v[ ('''a+''b) / ('a*'b) ]
   =>
   FAIL 
*)

typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	BinaryOp(ArithOper(PLUS),va''',vb''),
	BinaryOp(ArithOper(TIMES),va''',vb''))),[]);
(* 
   v[ ('''a+''b) / ('''a*''b) ]
   =>
   FAIL 
*)

typeof(VHole(BinaryOp(ArithOper(PLUS),
	BinaryOp(ArithOper(PLUS),
		BinaryOp(ArithOper(PLUS),va',va'),
		BinaryOp(ArithOper(PLUS),va',va')),
	BinaryOp(ArithOper(PLUS),
		BinaryOp(ArithOper(PLUS),va',va'),
		BinaryOp(ArithOper(DIVIDE),vb',vb')))),[]);
(*
   v[ ( ('a+'a) + ('a+'a) ) + ( ('a+'a) + ('b/'b) ) ]
   =>
   Real, ['a->'''a11, '''a11->Real, 'b->Real]
*)

typeof(VHole(BinaryOp(EXPR_PAIR,va',vb')),[]);
(* 'a * 'b *)

typeof(VHole(BinaryOp(EXPR_PAIR,
	BinaryOp(EXPR_PAIR,va''',vb'''),
	BinaryOp(EXPR_PAIR,va'',vb''))),[]);
(* ('''a * '''b) * (''a * ''b) *)

typeof(VHole(BinaryOp(BoolOper(EQ),
	BinaryOp(EXPR_PAIR,va''',vb'''),
	BinaryOp(EXPR_PAIR,va'',vb''))),[]);
(* Bool *)

typeof(VHole(ConditionHole(va',Value(N(3)),Value(N(4)))),[]);
(* 'a12, ['a->Bool] *)

typeof(VHole(ConditionHole(
	BinaryOp(BoolOper(LESS),va',vb'),
	Value(N(3)),Value(N(4)))),[]);
(* 'a14, ['a->'''a13, 'b->'''a13] *)
	
typeof(VHole(ConditionHole(
	BinaryOp(EXPR_PAIR,va''',vb'''),
	Value(N(3)),Value(N(4)))),[]);
(* FAIL *)

typeof(VHole(ConditionHole(va'',Value(N(3)),Value(N(4)))),[]);
(* 'a15, [''a->Bool] *)

typeof(VHole(ConditionHole(va''',Value(N(3)),Value(N(4)))),[]);
(* FAIL *)

typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),Value(N(3)))),[]);
(* 
   v[case 'a of (x,y) -> 3]
   =>
   Int, ['a->('a16,'a17)] 
*)

typeof(VHole(CaseHole(vb',VariablePair(Var("x"),Var("y")),Variable(Var("x")))),[]);
(* 
   v[case 'b of (x,y) -> x]
   =>
   'a18, ['b->('a18,'a19)] 
*)

typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),[]);
(* 
   v[case 'a of (x,y) -> x+y]
   =>
   '''a22, ['a->('a20,'a21), 'a20->'''a22, 'a21->'''a22] 
*)

typeof(VHole(CaseHole(va''',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),
		ArithExpr(PLUS,Variable(Var("y")),Value(N(3)))))),[]);
(* v[case '''a of (x,y) -> x+(y+3)]
   =>
   Int, ['''a->('''a23,'''a24), '''a23->Int, '''a24->Int]
*)

typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	BoolExpr(EQ,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[]);
(* 
   v[case ''a of (x,y) -> (x+y)=(x*y)]
   =>
   Bool, [''a->(''a25,''a26), ''a25->Int, ''a26->Int] 
*)

typeof(VHole(CaseHole(BinaryOp(EXPR_PAIR,va''',vb'''),VariablePair(Var("x"),Var("y")),
	BoolExpr(LESS,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[]);
(*
   v[case ('''a,'''b) of (x,y) -> (x+y)<(x*y)]
   =>
   Bool, ['''a->'''a27,'''b->'''a27]
*)

typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))),[]);
(* FAIL *)

typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[]);
(*
   v[case 'a of (x,y) -> case (3,4) of (x,y) -> x+y]
   =>
   Int, ['a->('a28,'a29)]
*)
