(* ----------------------------------------------------------------------------------- *)
(* test cases for unify *)
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

(* test outputs assume true, unless stated otherwise *)
unify( [Int,Int], []);		(* gives mapping [] *)
unify( [a',Int], []);		(* gives mapping [ (a' -> Int) ] *)
unify( [a''',Real], []);	(* gives mapping ------- *)
unify( [a''',Bool], []);	(* gives mapping [] with false *)
unify( [a'',Real], []);		(* gives mapping [] with false *)
unify( [a',b'], []);		(* gives mapping [ (a' -> b') ] *)
unify( [a''',b'''], []);	(* gives mapping [ (a''' -> b''') ] *)
unify( [a'',b'''], []);		(* gives mapping [ (a'' -> Int), (b''' -> Int) ] *)
unify( [a',b',c'], []); 	(* gives mapping [ (a' -> c'), (b' -> c') ] *)
unify( [a',b',Int], []);   	(* gives mapping [ (a' -> Int'), (b' -> Int)] *)
unify( [a',b'',c'''],[]);	(* gives mapping [ (a' -> Int), (b'' -> Int), (c''' -> Int) ] *)
unify( [Int,Int,Int],[]);	(* gives mapping [] *)
unify( [a',Int], [(a'1,Int)]);		(* gives mapping [ (a' -> Int) ] *)
unify( [a',Int], [(a'1,Real)]);  	(* gives mapping with false *)
unify( [a',b',c'], [(a'1,Int)]);	(* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b',c'], [(a'1,Int),(b'1,Real)]); (* gives mapping with false *)
unify( [a',b'',c'''], [(a'1,Int)] );		(* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b'',c'''], [(a'1,Int), (b''1,Int)] );  (* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b'',c'''], [(a'1,Real)] ); 		(* gives mapping with false *)
unify( [Pair(Int,Int),Pair(Int,Int)], []);  (* gives mapping [] *)
unify( [Pair(a',Int),Pair(Real,b')], []);	(* gives mapping [ ('a -> Real), ('b -> Int) ] *)
unify( [a',Pair(Real,Real)], []);			(* gives mapping [ (a' -> Real * Real) ] *)
unify( [a',Pair(a',Int),Pair(Real,b')], []);(* gives mapping with false *)
unify( [a',Pair(c',Int),Pair(Real,b')], []);(* gives mapping [ 'a -> Real * Int, 'c -> Real, 'b -> Int *)
unify( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Int], []); (* gives mapping with false *)
unify( [a',Pair(Pair(c''',Int),Pair(Bool,b')),Pair(Pair(a'',a'''),Pair(Bool,d'))], []);
	(* gives mapping ['a -> ( (Int * Int) * (Bool * 'd) ), 
					  '''c -> Int, ''a -> Int, '''a -> Int, 'b -> 'd ] *)
unify( [a',Pair(Int,Real)], [(a'1,Int)] );  (* gives mapping with false *)
unify( [a',Pair(b',c')], [(b'1,Int),(c'1,Int)]); (* gives mapping [ 'a -> Int * Int, 'b -> Int, 'c -> Int ] *)

(* ----------------------------------------------------------------------------------- *)

 (* TESTS FOR SUBSTITUTE *)
 
 substitute(Value(N(4)),N(3),Var("x"),[],[],evaluate); 			(* gives Value(N(4)),[],[]) *)
 substitute(Variable(Var("x")),N(3),Var("x"),[],[],evaluate);   (* gives Value(N(3)),[],[]) *)
 substitute(Plus(Value(N(3)),Value(N(4))),N(3),Var("x"),[],[], evaluate); 	(* gives Plus(Value(N(3)),Value(N(4))),[],[] *)
 substitute(Plus(Variable(Var("x")),Variable(Var("x"))),N(3),Var("x"),[],[],evaluate); (* gives Plus(Value(N(3)),Value(N(3))),[],[] *)
 substitute(Condition(Value(B(true)),Variable(Var("x")),Plus(Variable(Var("x")),Value(N(1)))),N(3),Var("x"),[],[],evaluate);
 (* gives (if true then 3 else 3 + 1,[],[]) *)
 
 evaluate(Config(Expression(Case(Value(ValuePair(N(1),N(2))),
								 ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
								 Plus(Variable(Var("x1")),Variable(Var("x2"))))),
							[],[]));
(* < case (1,2) of (x1,x2) -> x1 + x2 > -> < 1 + 2 > *)

evaluate(Config(Expression(
	Case(Value(ValuePair(N(1),N(2))),
		 ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
		 Case(ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
			  ExpressionPair(Variable(Var("x3")),Variable(Var("x4"))),
			  Plus(Variable(Var("x3")),Variable(Var("x3")))))), [], []));
(* <case (1,2) of (x1,x2) ->
		case (x1,x2) of (x3,x4) -> x3+x4 
	maps to
	*)