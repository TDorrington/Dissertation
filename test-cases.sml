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
(* TETS CASES FOR UNIFY *)
(* ' is type variables, '' equality type variables, and ''' arithmetic type variables *)
(* 1 appended to end means it is equivalent type, but type hole version *)

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

(* Pretty prints output from unify depending on boolean flag *)
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

remove([1,2,3],1);		 (* [2,3] *)
remove([1,1,1],1);		 (* [] *)
remove([1,2,3],4);		 (* [1,2,3] *)
remove([1,2,3,1,2,3],1); (* [2,3,2,3] *)

element([1,2,3],1);		 (* true *)
element([],1);			 (* false *)
element([1],1);			 (* true *)
element([1],2);			 (* false *)
element([1,2,3,4,5],6);	 (* false *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR FREE VARIABLES *)

(* Tests free variables of a single expression *)
fvExpr(Variable(Var("x"))); (* [ x ] *)
fvExpr(Plus(Value(N(3)),Value(N(3)))); (* [ ] *)
fvExpr(Equal(Variable(Var("y")),Value(N(3)))); (* [ y ] *)
fvExpr(Times(Plus(Variable(Var("x")),Variable(Var("y"))),
			 Plus(Variable(Var("a")),Variable(Var("b"))))); (* [ x y a b ] *)
fvExpr(Times(Plus(Variable(Var("x")),Variable(Var("x"))),
			 Plus(Variable(Var("x")),Variable(Var("x"))))); (* [ x y a b ] *)
fvExpr(Value(N(3))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Value(B(true)))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Variable(Var("x")))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Variable(Var("y")))); (* [ ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Variable(Var("z")))); (* [ z ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Plus(Variable(Var("z")),Variable(Var("a"))))); (* [ a, z ] *)
fvExpr(Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			Plus(Variable(Var("x")),Variable(Var("y"))))); (* [ ] *)
			
(* Tests free variables of a list of expressions *)
fv( [Variable(Var("x")),
	 Plus(Value(N(3)),Variable(Var("y"))),
	 Case(Value(ValuePair(N(3),N(3))),
			ExpressionPair(Variable(Var("a")),Variable(Var("b"))),
			Plus(Variable(Var("b")),Variable(Var("a")))),
	 Condition(Variable(Var("x")),Value(B(true)),Equal(Variable(Var("k")),Value(N(4)))) ]);
(* [ y, x, k ] *)
			
(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR SUBSTITUTE AND ALPHAVARIANT *)

val sub = [ (Var("x"),Value(N(3))) ];
prettyPrintExpression(Expression(substitute(Value(N(4)),sub))); (* 4 *)
prettyPrintExpression(Expression(substitute(Variable(Var("x")),sub))); (* 3 *)
prettyPrintExpression(Expression(substitute(Plus(Value(N(3)),Value(N(4))),sub))); (* 3 + 4 *)
prettyPrintExpression(Expression(substitute(Plus(Variable(Var("x")),Variable(Var("x"))),sub))); (* 3 + 3 *)
prettyPrintExpression(Expression(substitute(Condition(Value(B(true)),Variable(Var("x")),Plus(Variable(Var("x")),
						Value(N(1)))),sub))); (* if true then 3 else 3 + 1 *)
						
prettyPrintExpression(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   ExpressionPair(Variable(Var("y")),Variable(Var("z"))),
			   Plus(Variable(Var("x")),Plus(Variable(Var("y")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (y,z) -> 3 + y + z *)
	
prettyPrintExpression(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   ExpressionPair(Variable(Var("x")),Variable(Var("z"))),
			   Plus(Variable(Var("x")),Plus(Variable(Var("x")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (x0,z0) -> x0 + x0 + z0 *)
	
val sub = [ (Var("x"),Value(N(3))), (Var("y"),Value(N(4))) ];

prettyPrintExpression(Expression(
	substitute(Case(Value(ValuePair(N(3),N(4))),
			   ExpressionPair(Variable(Var("x")),Variable(Var("z"))),
			   Plus(Variable(Var("y")),Plus(Variable(Var("x")),Variable(Var("z"))))),
	sub))); (* case (3,4) of (x1,z1) -> 4 + x1 + z1 *)

prettyPrintExpression(Expression(
	substitute(Plus(Plus(Variable(Var("x")),Variable(Var("y"))),
			   Case(Value(ValuePair(N(1),N(2))),
			   ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			   Plus(Times(Variable(Var("x")),Variable(Var("y"))),
					Case(Value(ValuePair(N(1),N(2))),
					ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
					Subtract(Times(Variable(Var("x")),Variable(Var("x"))),
							 Case(Value(ValuePair(N(1),N(2))),
							 ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
							 Equal(Variable(Var("x")),Variable(Var("y"))))))))),sub)));
(* 3+4 + case of (x2,y2) -> x2*y2 + case (1,2) of (x2,y2) -> x2 * x2 - case (1,2) of (x2,y2) -> x2=y2 *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR RESOLVE CHAIN *)

val a = (ValueHole(TypeVar("a")));
val b = (ValueHole(TypeVar("b")));
val c = (ValueHole(TypeVar("c")));
val d = (ValueHole(TypeVar("d")));

val sigma = [ (a,VHole(b)), (b,VHole(c)), (c,N(3)) ];

resolveChain(VHole(a),sigma); (* 3 *)
resolveChain(VHole(b),sigma); (* 3 *)
resolveChain(VHole(c),sigma); (* 3 *)

val sigma = [ (a,VHole(b)), (b,ValuePair(VHole(c),VHole(d))), (c,N(3)), (d,B(true)) ];

resolveChain(VHole(a),sigma); (* (3,true) *)


(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR EVALUATE *)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(N(1)),Value(N(2)))),[],[])));
(* 3 *)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(VHole(a)),Value(VHole(b)))),[],[])));
(* v['''a] + v['''b], [ v['b]->v['''b], v['a]->v['''a] ], ['a->'''a, 'b->'''b] *)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(VHole(a)),Value(VHole(c)))),
	[ (a, VHole(ValueHole(ArithTypeVar("a")))), (b, VHole(ValueHole(ArithTypeVar("b")))) ],
	[ (TypeHole(TypeVar("a")),THole(TypeHole(ArithTypeVar("a")))), (TypeHole(TypeVar("b")),THole(TypeHole(ArithTypeVar("b")))) ] )));
(* v['''a] + v['''c], [ v['b]->v['''b], v['a]->v['''a], v['c]->v['''c] ], ['a->'''a, 'b->'''b, 'c->'''c] *)

prettyPrintConfig(evaluate(
	Config(Expression(Plus(Plus(Value(N(1)),Value(N(2))),
						   Plus(Value(N(5)),Value(N(3))))),[],[])));
(* 11 *)

prettyPrintConfig(evaluate(Config(Expression(Value(N(3))),[],[])));
(* 3 *)

prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(Value(N(3)),Value(N(4)))),[],[])));
(* (3,4) *)

prettyPrintConfig(evaluate(Config(Expression(
	Plus(Plus(Value(VHole(a)),Value(VHole(b))),
		 Plus(Value(VHole(a)),Value(VHole(c))))),[],[])));
(* v['''a] + v['''b] + v['''a] + v['''c] *)

prettyPrintConfig(evaluate(Config(Expression(
	Plus(Plus(Value(VHole(a)),Value(VHole(b))),
		 Plus(Value(N(3)),Value(VHole(c))))),[],[])));
(* 6 *)

prettyPrintConfig(evaluate(Config(Expression(
	Condition(LessThan(Value(VHole(ValueHole(TypeVar("a")))),Value(VHole(ValueHole(TypeVar("b"))))),
			  Value(N(3)),Value(N(4)))),[],[])));
(* if v['''a]<v['''b] then 3 else 4 *)

			  
prettyPrintConfig(evaluate(Config(Expression(
		Case(Value(ValuePair(N(1),N(2))),
			 ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			 Plus(Times(Variable(Var("x")),Variable(Var("y"))),
				  Case(Value(ValuePair(N(3),N(4))),
				  ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
				  Subtract(Times(Variable(Var("x")),Variable(Var("x"))),
						   Case(Value(ValuePair(N(5),N(6))),
						   ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
						   Condition(Equal(Variable(Var("x")),Variable(Var("y"))),
									 Value(N(1)),Value(N(0))))))))),[],[])));
(* case (1,2) of (x,y) -> x*y +
		case (3,4) of (x,y) -> x*x -
				case (5,6) of (x,y) -> if x=y then 1 else 0 
   => 11 *)
  
 

							  