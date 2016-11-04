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
narrow(N(3),Int,[],[]);	
narrow(R(3.0),Real,[],[]);
narrow(N(3),Real,[],[]);
narrow(B(true),Bool,[],[]);
narrow(B(false),Int,[],[]);

(* tests for pairs *)


(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR SUBSTITUTE *)
 
prettyPrintExpression(Expression(substitute(Value(N(4)),N(3),Var("x")))); (* 4 *)
prettyPrintExpression(Expression(substitute(Variable(Var("x")),N(3),Var("x")))); (* 3 *)
prettyPrintExpression(Expression(substitute(Plus(Value(N(3)),Value(N(4))),N(3),Var("x")))); (* 3 + 4 *)
prettyPrintExpression(Expression(substitute(Plus(Variable(Var("x")),Variable(Var("x"))),N(3),Var("x")))); (* 3 + 3 *)
prettyPrintExpression(Expression(substitute(Condition(Value(B(true)),Variable(Var("x")),Plus(Variable(Var("x")),Value(N(1)))),N(3),Var("x")))); (* if true then 3 else 3 + 1 *)


(* ----------------------------------------------------------------------------------- *)