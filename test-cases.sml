
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
(* TEST CASES FOR EVALUATE *)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(N(1)),Value(N(2)))),[],[])));
(* <1+2,[],[]> -> <3,[],[]> *)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(VHole(a)),Value(VHole(b)))),[],[])));
(* <v['a]+v['b],[],[] 
   ->
   <v['''a] + v['''b], [ v['b]->v['''b], v['a]->v['''a] ], ['a->'''a, 'b->'''b]> 
*)

prettyPrintConfig(evaluate(Config(Expression(Plus(Value(VHole(a)),Value(VHole(c)))),
	[ (a, VHole(ValueHole(ArithTypeVar("a")))), (b, VHole(ValueHole(ArithTypeVar("b")))) ],
	[ (TypeHole(TypeVar("a")),THole(TypeHole(ArithTypeVar("a")))), (TypeHole(TypeVar("b")),THole(TypeHole(ArithTypeVar("b")))) ] )));
(* <v['a]+v['c], [ v['b]->v['''b], v['a]->v['''a] ], ['a->'''a, 'b->'''b]>
   ->
   v['''a] + v['''c], [ v['b]->v['''b], v['a]->v['''a], v['c]->v['''c] ], ['a->'''a, 'b->'''b, 'c->'''c] 
*)

prettyPrintConfig(evaluate(
	Config(Expression(Plus(Plus(Value(N(1)),Value(N(2))),
						   Plus(Value(N(5)),Value(N(3))))),[],[])));
(* <(1+2)+(5+3),[],[]> -> <11,[],[]> *)

prettyPrintConfig(evaluate(Config(Expression(Value(N(3))),[],[])));
(* <3,[],[]> -> <3,[],[]> *)

prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(Value(N(3)),Value(N(4)))),[],[])));
(* <(3,4),[],[]> -> <(3,4),[],[]> *)

prettyPrintConfig(evaluate(Config(Expression(
	Plus(Plus(Value(VHole(a)),Value(VHole(b))),
		 Plus(Value(VHole(a)),Value(VHole(c))))),[],[])));
(* <(v['a]+v['b])+(v['a]+v['c]), [], []> 
   ->
   <v['''a] + v['''b] + v['''a] + v['''c], [v['c]->v['''c],v['b]->v['''b],v['a]->v['''a]], ['c->'''c,'b->'''b,'a->'''a]>
*)
   
prettyPrintConfig(evaluate(Config(Expression(
	Plus(Plus(Value(VHole(a)),Value(VHole(b))),
		 Plus(Value(N(3)),Value(VHole(a))))),[],[])));
(* <(v['a]+v['b])+(3+v['c]),[],[]> 
   -> 
   <(v['''a]+v['''b])+4,[v['b]->v['''b],v['a]->v['''a],v['c]->1],['c->int,'b->'''b,'a->'''a]> 
*)

prettyPrintConfig(evaluate(Config(Expression(
	Condition(LessThan(Value(VHole(ValueHole(TypeVar("a")))),Value(VHole(ValueHole(TypeVar("b"))))),
			  Plus(Value(VHole(ValueHole(TypeVar("a")))),Value(N(4))),Value(N(4)))),[],[])));
(* if v['a]<v['b] then (v['a] + 3) else 4 
   -> 
  <4, [v['''b] -> 1, v['''a] -> 1, v['b] -> v['''b], v['a] -> v['''a]], ['''b -> int, '''a -> int, 'b -> '''b, 'a -> '''a]>
*)
	  
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

prettyPrintConfig(evaluate(Config(Expression(
		Case(ExpressionPair(Value(VHole(ValueHole(TypeVar("a")))),Plus(Value(VHole(ValueHole(TypeVar("b")))),Value(N(2)))),
			 ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
			 Plus(Times(Variable(Var("x")),Variable(Var("y"))),
				  Case(Value(ValuePair(N(3),N(4))),
				  ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
				  Subtract(Times(Variable(Var("x")),Variable(Var("x"))),
						   Case(Value(ValuePair(N(5),N(6))),
						   ExpressionPair(Variable(Var("x")),Variable(Var("y"))),
						   Condition(Equal(Variable(Var("x")),Variable(Var("y"))),
  (* case (v['a],v['b]+2) of (x,y) -> x*y +
		case (3,4) of (x,y) -> x*x -
				case (5,6) of (x,y) -> if x=y then 1+v['a] else 0 
   => 12 *)