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
val e = (ValueHole(TypeVar("e")));
val f = (ValueHole(TypeVar("f")));

val sigma = [ (a,VHole(SimpleHole(b))), (b,VHole(SimpleHole(c))), (c,N(3)) ];

resolveChainSigma(VHole(SimpleHole(a)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(b)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(c)),sigma); (* 3 *)

val sigma = [(a,VHole(BinaryOp(BoolOper(EQ),VHole(SimpleHole(b)),VHole(SimpleHole(c))))),
			 (b,ValuePair(VHole(SimpleHole(d)),VHole(SimpleHole(e)))),
			 (c,N(3))];
			 
resolveChainSigma(VHole(SimpleHole(a)),sigma); (* v[ (v['d],v['e]) = 3 ] *)

val sigma = [ (a,VHole(SimpleHole(b))), (b,ValuePair(VHole(SimpleHole(c)),VHole(SimpleHole(d)))), 
			  (c,N(3)), (d,B(true)) ];

resolveChainSigma(VHole(SimpleHole(a)),sigma); (* (3,true) *)

val sigma = [(a,VHole(ConditionHole(VHole(SimpleHole(b)),
			    BoolExpr(LESS,Value(N(3)),Value(VHole(SimpleHole(c)))),
				BoolExpr(MORE,Value(N(10)),Value(VHole(SimpleHole(d))))))),
			 (b,VHole(CaseHole(VHole(SimpleHole(e)),VariablePair(Var("x"),Var("y")),
							   BoolExpr(LESS,Variable(Var("x")),ArithExpr(TIMES,Value(N(2)),Variable(Var("y"))))))),
			 (e,ValuePair(N(1),N(2))),
			 (d,VHole(BinaryOp(ArithOper(SUBTRACT),VHole(SimpleHole(f)),N(5))))];
			 
prettyPrintValue(resolveChainSigma(VHole(SimpleHole(a)),sigma));
(* v[ if v[case (1,2) of (x,y) -> x < 2*y] then (3 < v['c]) else (10 > (v['f]-5)) *)

val a' = (THole(TypeVar("a")));
val b' = (THole(TypeVar("b")));
val c' = (THole(TypeVar("c")));

val theta = [

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

prettyPrintE(substitute(Value(ValuePair(N(3),R(3.0))),[])); (* (3,3.0) *)
prettyPrintE(substitute(Value(va'),[])); (* v['a] *)
prettyPrintE(substitute(Value(ValuePair(va',vb')),[])); (* (v['a],v['b]) *)
prettyPrintE(substitute(Value(VHole(BinaryOp(EXPR_PAIR,va',vb'))),[])); (* v[('a,'b)] *)
prettyPrintE(substitute(Value(VHole(BinaryOp(ArithOper(PLUS),va',vb'))),[])); (* v['a+'b] *)
prettyPrintE(substitute(ArithExpr(PLUS,Value(va'),Value(vb')),[])); (* v['a]+v['b] *)
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
	VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),
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
		
fvExpr(Value(va')); (* [ ] *)
fvExpr(Value(ValuePair(va',vb'))); (* [ ] *)
fvExpr(Value(ValuePair(N(3),N(4)))); (* [ ] *)
fvExpr(Value(VHole(BinaryOp(ArithOper(PLUS),va',vb'))));
fvExpr(Value(VHole(BinaryOp(EXPR_PAIR,va''',vb''))));
fvExpr(Value(VHole(CaseHole(
	VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z"))))),
	VariablePair(Var("a"),Var("b")),
	ExpressionPair(ExpressionPair(Variable(Var("a")),Variable(Var("c"))),
				   ExpressionPair(Variable(Var("d")),Variable(Var("d"))))))));
(* [ z, c, d] *)
fvExpr(Value(VHole(ConditionHole(
	VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),Value(va'))),
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

fun unifyTest(a,b) = (case unify(a,b) of

	  NONE => "FAIL"
	| SOME(theta) => prettyPrintTheta(theta));

unifyTest( [Int,Int], []);	(* [] *)
unifyTest( [a',a'], []);	(* [] *)	
unifyTest( [a',Int], []);	(* [ ('a -> Int) ] *)
unifyTest( [a''',Real], []);	(* [ ('''a -> Real) ] *)
unifyTest( [a''',Bool], []);	(* FAIL *)
unifyTest( [a'',Real], []);		(* FAIL *)
unifyTest( [a',b'], []);		(* [ ('a -> 'b) ] *)
unifyTest( [a''',b'''], []);	(* [ ('''a -> '''b) ] *)
unifyTest( [a'',b'''], []);		(* [ (''a -> Int), ('''b -> Int) ] *)
unifyTest( [a',b',c'], []); 	(* [ ('a -> 'c), ('b -> 'c) ] *)
unifyTest( [a',b',Int], []); 	(* [ ('a -> Int), ('b -> Int)] *)
unifyTest( [a',b'',c'''],[]);	(* [ ('a -> Int), (''b -> Int), ('''c -> Int) ] *)
unifyTest( [Int,Int,Int],[]);	(* [] *)
unifyTest( [Int,Int,Real],[]);	(* [] *)
unifyTest( [Real,Real,Real],[]);(* [] *)
unifyTest( [a',Int], [(a'1,Int)]);		(* [ ('a -> Int) ] *)
unifyTest( [a',Int], [(a'1,Real)]);  	(* FAIL *)
unifyTest( [a',b',c'], [(a'1,Int)]);	(* [ ('b -> Int), ('c -> Int), ('a -> Int) ] *)
unifyTest( [a',b'], [(a'1,c')]); 		(* [ 'a -> 'c, 'b -> 'c ] *)
unifyTest( [a',b',c'], [(a'1,Int),(b'1,Real)]);  	(* FAIL *)
unifyTest( [a',b'',c'''], [(a'1,Int)]);				(* [ (''b -> Int), ('''c -> Int), ('a -> Int) ] *)
unifyTest( [a',b'',c'''], [(a'1,Int), (b''1,Int)]); (* [ ('a -> Int), (''b -> Int), ('''c -> Int) ] *)
unifyTest( [a',b'',c'''], [(a'1,Real)]); 			(* FAIL *)

unifyTest( [Pair(Int,Int),Pair(Int,Int)], []);  	(* [] *)
unifyTest( [Pair(Int,Int),Pair(Bool,Int)], []);		(* FAIL *)
unifyTest( [Pair(a',Int),Pair(Real,b')], []);		(* [ ('a -> Real), ('b -> Int) ] *)
unifyTest( [a',Pair(Real,Real)], []);				(* [ ('a->'a0*'a1), ('a0->real), ('a1->real) ] *)
unifyTest( [a',Pair(a',Int),Pair(Real,b')], []);	(* FAIL *)
unifyTest( [a',Pair(c',Int),Pair(Real,b')], []);	(* [ 'a10 -> real, 'a11 -> int, 'a12 -> real, 'a13 -> int, 'c -> real, 'b -> int, 'a -> ('a10 * 'a11) ] *)
unifyTest( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Int], []); (* FAIL *)
unifyTest( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Pair(Pair(a'',a'''),Pair(Bool,d'))], []);
(* ['a22 -> int, 'a23 -> int, 'a24 -> bool, 'a25 -> 'd, 'a28 -> int, 'a29 -> int, 'a30 -> bool, 'a31 -> 'd, '''c -> int, ''a -> int, '''a -> int, 'b -> 'd, 'a27 -> ('a30 * 'a31), 'a26 -> ('a28 * 'a29), 'a21 -> ('a24 * 'a25), 'a20 -> ('a22 * 'a23), 'a -> ('a20 * 'a21) ] *)
unifyTest( [a',Pair(Int,Real)], [(a'1,Int)]); 	 	 (* FAIL *)
unifyTest( [a',Pair(b',c')], [(b'1,Int),(c'1,Int)]);
(* [ 'a32 -> int, 'a33 -> int, 'a -> ('a32 * 'a33), 'b -> int, 'c -> int ] *)

unifyTest( [a'',Pair(Int,Bool)], []);
(* ''a34 -> int, ''a35 -> bool, ''a -> (''a34 * ''a35) *)
unifyTest( [a'',Pair(Int,a'''),Pair(b''',Real)], []);
(* FAIL *)
unifyTest( [a'',Pair(Int,b''),Pair(b''',Bool)], []);
(* [ ''a40 -> int, ''a41 -> bool, ''a42 -> int, ''a43 -> bool, '''b -> int, ''b -> bool, ''a -> (''a40 * ''a41) ]*)
unifyTest( [a''',Pair(Int,Int)], []);
(* FAIL *)
unifyTest( [a'',Pair(a''',b''')], []);
(* [ ''a44 -> int, '''a -> int, ''a45 -> int, '''b -> int, ''a -> (''a44 * ''a45) ] *)
unifyTest( [a'',Pair(b'',c''),Pair(d'',a''')], []);
(* [ ''a46 -> ''d, ''a47 -> int, ''a48 -> ''d, ''a49 -> int, ''b -> ''d, ''c -> int, '''a -> int, ''a -> (''a46 * ''a47) ] *)
unifyTest( [a'',Pair(b'',c'''),Pair(d'',a''')], []);
(* [ ''a50 -> ''d, ''a51 -> int, '''c -> int, ''a52 -> ''d, ''a53 -> int, ''b -> ''d, '''a -> '''c, ''a -> (''a50 * ''a51) ] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR TYPEOF *)

fun prettyPrintTypeOf(NONE,_) = "FAIL"
|	prettyPrintTypeOf(SOME t,theta) = 
	prettyPrintType(t) ^ ", theta = [ " ^ prettyPrintTheta(theta) ^ " ]";

prettyPrintTypeOf(typeof(N(3),[],[]));		(* Int *)
prettyPrintTypeOf(typeof(B(true),[],[]));	(* Bool *)
prettyPrintTypeOf(typeof(R(3.0),[],[]));	(* Real *)
prettyPrintTypeOf(typeof(ValuePair(N(3),N(4)),[],[]));			(* (Int,Int) *)
prettyPrintTypeOf(typeof(ValuePair(B(true),R(5.0)),[],[]));		(* (Bool,Real) *)

prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(TypeVar("a")))),[],[]));			(* 'a   *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(EqualityTypeVar("a")))),[],[]));	(* ''a  *)
prettyPrintTypeOf(typeof(VHole(SimpleHole(ValueHole(ArithTypeVar("a")))),[],[]));		(* '''a *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),va',vb')),[],[]));	 		(* '''a0, [  'a->'''a0,   'b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),va''',vb''')),[],[]));		(* '''a0, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(SUBTRACT),va''',vb'')),[],[]));	(* Int,   ['''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),[],[])); 		(* Real,  [  'a->Real,    'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va'',vb')),[],[]));		(* FAIL 							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb')),[],[]));		(* Real,  [ '''a->Real,   'b->Real ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),va''',vb'')),[],[]));		(* FAIL  							  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),va''',va''')),[],[]));		(* '''a,  [ ]						  *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va',vb')),[],[]));				(* Bool, [  'a->''a0,    'b->''a0 ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va''',vb''')),[],[]));			(* Bool, ['''a->Int,   '''b->Int  ]	 *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),va'',vb''')),[],[]));			(* Bool, ['''a->Int,   '''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS),va',vb')),[],[]));			(* Bool, [  'a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE),va'',vb''')),[],[]));		(* Bool, [ ''a->Int,  '''b->Int   ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS_EQ),va''',vb''')),[],[]));	(* Bool, ['''a->'''a0, '''b->'''a0]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',vb'')),[],[]));		(* Bool, [ ''a->Int,    ''b->Int  ]  *)
prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(MORE_EQ),va'',va'')),[],[]));		(* Bool, [ ''a->Int ]  				 *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS),
	VHole(BinaryOp(ArithOper(PLUS),va',vb')),
	VHole(BinaryOp(ArithOper(SUBTRACT),va',vb')))),[],[])); 
(* 
   v[ ('a+'b) < ('a-'b) ]
   =>
   Bool, ['a->'''a0, 'b->'''a0] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),
	VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOp(ArithOper(PLUS),va',vb')))),[],[]));
(* 
   v[ ('a/'b) * ('a+'b) ]
   =>
   Real, ['a->Real, 'b->Real] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(LESS),
	VHole(BinaryOp(ArithOper(PLUS),va',vb')),
	VHole(BinaryOp(ArithOper(SUBTRACT),vc',vd')))),[],[])); 
(* 
   v[ ('a+'b) < ('c-'d) ]
   =>
   Bool, ['a->'''a0, 'b->'''a0, 'c->'''a1, 'd->'''a1, '''a0->'''a2, '''a1->'''a2] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),
	VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOp(ArithOper(PLUS),vc',vd')))),[],[]));
(* 
   v[ ('a/'b) * ('c+'d) ]
   =>
   Real, ['a->Real, 'b->Real, 'c->a0, 'd->a0, 'a0->Real] 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(TIMES),
	VHole(BinaryOp(ArithOper(DIVIDE),va',vb')),
	VHole(BinaryOp(BoolOper(EQ),va'',vb'')))),[],[]));
(* 
   v[ ('a/'b) * (''a=''b) ]
   =>
   FAIL 
*)	

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	VHole(BinaryOp(ArithOper(PLUS),va''',vb'')),
	VHole(BinaryOp(ArithOper(TIMES),va',vb')))),[],[]));
(* 
   v[ ('''a+''b) / ('a*'b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(DIVIDE),
	VHole(BinaryOp(ArithOper(PLUS),va''',vb'')),
	VHole(BinaryOp(ArithOper(TIMES),va''',vb'')))),[],[]));
(* 
   v[ ('''a+''b) / ('''a*''b) ]
   =>
   FAIL 
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(ArithOper(PLUS),
	VHole(BinaryOp(ArithOper(PLUS),
		VHole(BinaryOp(ArithOper(PLUS),va',va')),
		VHole(BinaryOp(ArithOper(PLUS),va',va')))),
	VHole(BinaryOp(ArithOper(PLUS),
		VHole(BinaryOp(ArithOper(PLUS),va',va')),
		VHole(BinaryOp(ArithOper(DIVIDE),vb',vb')))))),[],[]));
(*
   v[ ( ('a+'a) + ('a+'a) ) + ( ('a+'a) + ('b/'b) ) ]
   =>
   Real, ['a->'''a0, '''a0->Real, 'b->Real]
*)

prettyPrintTypeOf(typeof(VHole(BinaryOp(EXPR_PAIR,va',vb')),[],[]));
(* 'a * 'b *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(EXPR_PAIR,
	VHole(BinaryOp(EXPR_PAIR,va''',vb''')),
	VHole(BinaryOp(EXPR_PAIR,va'',vb'')))),[],[]));
(* ('''a * '''b) * (''a * ''b) *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),
	VHole(BinaryOp(EXPR_PAIR,N(3),vb''')),
	VHole(BinaryOp(EXPR_PAIR,N(4),vb'')))),[],[]));
(* Bool, ['''b->Int, ''b->Int] *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),
	VHole(BinaryOp(EXPR_PAIR,N(3),vb''')),
	VHole(BinaryOp(EXPR_PAIR,R(4.0),vb'')))),[],[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(BinaryOp(BoolOper(EQ),
	VHole(BinaryOp(EXPR_PAIR,va''',vb''')),
	VHole(BinaryOp(EXPR_PAIR,va'',vb'')))),[],[]));
(* Bool, ['''a->Int, '''b->Int, ''b->Int, ''a->Int] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(N(3)),Value(N(4)))),[],[]));
(* Int, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va',Value(N(3)),Value(R(4.0)))),[],[]));
(* 'a0, ['a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(BinaryOp(BoolOper(LESS),va',vb')),
	Value(N(3)),Value(N(4)))),[],[]));
(* Int, ['a->'''a0, 'b->'''a0] *)
	
prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(BinaryOp(EXPR_PAIR,va''',vb''')),
	Value(N(3)),Value(N(4)))),[],[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va'',Value(N(3)),Value(N(4)))),[],[]));
(* Int, [''a->Bool] *)

prettyPrintTypeOf(typeof(VHole(ConditionHole(va''',Value(N(3)),Value(N(4)))),[],[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),Value(N(3)))),[],[]));
(* 
   v[case 'a of (x,y) -> 3]
   =>
   Int, ['a->('a0,'a1)] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(vb',VariablePair(Var("x"),Var("y")),Variable(Var("x")))),[],[]));
(* 
   v[case 'b of (x,y) -> x]
   =>
   'a0, ['b->('a0,'a1)] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(* 
   v[case 'a of (x,y) -> x+y]
   =>
   '''a2, ['a->('a0,'a1), 'a0->'''a2, 'a1->'''a2] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va''',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),
		ArithExpr(PLUS,Variable(Var("y")),Value(N(3)))))),[],[]));
(* v[case '''a of (x,y) -> x+(y+3)]
   =>
   FAIL
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS,Variable(Var("x")),
		ArithExpr(PLUS,Variable(Var("y")),Value(N(3)))))),[],[]));
(* v[case '''a of (x,y) -> x+(y+3)]
   =>
   Int, [''a->(''a0,''a1), ''a0->Int, ''a1->Int]
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	BoolExpr(EQ,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[],[]));
(* 
   v[case ''a of (x,y) -> (x+y)=(x*y)]
   =>
   Bool, [''a->(''a0,''a1), ''a0->Int, ''a1->Int] 
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(VHole(BinaryOp(EXPR_PAIR,va''',vb''')),VariablePair(Var("x"),Var("y")),
	BoolExpr(LESS,
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))),[],[]));
(*
   v[case ('''a,'''b) of (x,y) -> (x+y)<(x*y)]
   =>
   Bool, ['''a->'''a0,'''b->'''a0]
*)

prettyPrintTypeOf(typeof(VHole(CaseHole(va'',VariablePair(Var("x"),Var("y")),
	ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(* FAIL *)

prettyPrintTypeOf(typeof(VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),
	Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[],[]));
(*
   v[case 'a of (x,y) -> case (3,4) of (x,y) -> x+y]
   =>
   Int, ['a->('a0,'a1)]
*)

prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(CaseHole(va',VariablePair(Var("x"),Var("y")),Value(va'))),
	Value(N(3)),Value(N(4)))),[],[]));
(* v[if (case 'a of (x,y) -> 'a) then 3 else 4]
   => 
   FAIL *)
   
prettyPrintTypeOf(typeof(VHole(ConditionHole(
	VHole(CaseHole(ValuePair(va',vb'),VariablePair(Var("x"),Var("y")),Value(va'))),
	Value(N(3)),Value(N(4)))),[],[]));
(* v[if (case 'a of (x,y) -> 'a) then 3 else 4]
   => 
   Int, ['a->Bool] *)

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

prettyPrintTypeOf(typeofexpr(ArithExpr(PLUS,Value(N(3)),Value(va')),[],[])); 		(* Int, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(vb'),Value(R(5.0))),[],[]));	(* Real, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(TIMES,Value(va'''),Value(vb'')),[],[])); 	(* Int, ['''a->Int, ''b->Int] *)
prettyPrintTypeOf(typeofexpr(ArithExpr(DIVIDE,Value(vb''),Value(va''')),[],[]));  	(* FAIL *)

prettyPrintTypeOf(typeofexpr(BoolExpr(LESS,Value(N(3)),Value(va')),[],[])); 		(* Bool, ['a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(LESS_EQ,Value(vb'),Value(R(5.0))),[],[])); 	(* Bool, ['b->Real] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(va'''),Value(vb'')),[],[])); 		(* Bool, [''b->Int,'''a->Int] *)
prettyPrintTypeOf(typeofexpr(BoolExpr(EQ,Value(vb'''),Value(va''')),[],[])); 		(* Bool, ['''a->Int,'''b->Int]  *)

prettyPrintTypeOf(typeofexpr(ExpressionPair(Value(N(3)),Value(va')),[],[])); (* Int * 'a *)
prettyPrintTypeOf(typeofexpr(ExpressionPair(
	ArithExpr(TIMES,Value(va'''),Value(vb'')),
	BoolExpr(LESS_EQ,Value(vb'),Value(R(5.0)))),[],[])); 
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

prettyPrintTypeOf(typeofexpr(Case(Value(va'),VariablePair(Var("x"),Var("y")),
	Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
		ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),[],[]));
(* 
   case v['a] of (x,y) -> case (3,4) of (x,y) -> x+y 
   => 
   Int, ['a->'a0*'a1] 
*)

prettyPrintTypeOf(typeofexpr(Case(Value(va'),VariablePair(Var("x"),Var("y")),
	ArithExpr(PLUS, Variable(Var("x")),
		Case(Value(ValuePair(N(3),N(4))),VariablePair(Var("x"),Var("y")),
			ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))),[],[]));
(* 
   case v['a] of (x,y) -> x + (case (3,4) of (x,y) -> x+y)
   => 
   Int, ['a->'a0*'a1, 'a0->Int] 
*)

prettyPrintTypeOf(typeofexpr(Case(Value(va'),VariablePair(Var("x"),Var("y")),
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