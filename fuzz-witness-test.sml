(* 
	Write down expression, and generate CounterExpression version
	Hand-generate the variables->types list
	Call fuzz() to generate a list of possible fuzzed versions of the expression
	Call iterFindWitness to generate a list of witness strings
	Call prettyPrintFindWitnessList to print those strings
*)
	
(* --- Filter example --- *) 

	val c' = THole(TypeHole(TypeVar("c")));	

	(* Function which takes a list and filters the elements of the list based on predicate (fn x:int => x = 10) *)
	val filterExpr = 
		Value(Fun(Var("list"),THole(TypeHole(TypeVar("b"))),
			  LetRec(Var("filter"),TFun(TFun(c',Bool),TFun(TList(c'),TList(c'))),
					 Value(Fun(Var("p"),TFun(c',Bool),Value(Fun(Var("l"),TList(c'),
						   Case(Variable(Var("l")),
							  [(PVal(EmptyList),Value(Concrete(EmptyList))),
							   (PCons(PVar(Var("x")),PVar(Var("xs"))),
								Condition(App(Variable(Var("p")),Variable(Var("x"))),
										  Cons(Variable(Var("x")),
											   App(App(Variable(Var("filter")),Variable(Var("p"))),
												   Variable(Var("xs")))),
										  App(App(Variable(Var("filter")),Variable(Var("p"))),
											  Variable(Var("xs")))))]))))),
		App(App(Variable(Var("filter")),
				Value(Fun(Var("y"),Int,BoolExpr(EQ,Variable(Var("y")),Value(Concrete(N(10))))))),
			Variable(Var("list"))))));
	(* fn list:'b => let val rec filter : ('c->bool)->'c list -> 'c list = fn p:('c->bool) => fn l:'c list => 
							case l of [] -> [] | x::xs -> if (p x) then x::(filter p xs) else (filter p xs)
					 in filter (fn y:int => y=10) list *)
				
	val var_types = [(Var("list"),TList(Int)),
					 (Var("filter"),TFun(TFun(Int,Bool),TFun(TList(Int),TList(Int)))),
					 (Var("p"),TFun(Int,Bool)),
					 (Var("l"),TList(Int)),
					 (Var("x"),Int),
					 (Var("xs"),TList(Int)),
					 (Var("y"),Int)];
				
	val SOME(fuzzFilterExpr) = fuzz(toCounterExpr(filterExpr),var_types);
	(* [fn list:'b => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
   	   [fn list:'b => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in (filter) (fn y:int => y = 10) + list end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (filter) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in (filter + fn y:int => y = 10) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:real => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => (y) (10))) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 2.0)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => list = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((list) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:{a:int} => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:bool => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case filter of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x = ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end], 
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else (filter) (p) + xs) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (filter)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else (filter + p) (xs)) in ((filter) (fn y:int => y = 10)) (list) end], 
	   * [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (list)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((list) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end], 
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x = ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end], 
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: (filter) (p) + xs else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (filter) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: (filter + p) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (list)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((list) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  list :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if p + x then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (list) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (list) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end],
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> {} | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end], 
	   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case filter of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn y:int => y = 10)) (list) end] *)
   
	prettyPrintFindWitnessList(findWitnessList(fuzzFilterExpr));
	(* [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: [1.0]], 
	   [ Witness: [1]], 
	   [ Witness: [1]], 
	   [ Witness: [1]],
	   [ Witness: fn x:(int->bool) => v['a226]],
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: fn x:int => true], 
	   [ Witness: fn x:(int->bool) => fn x:int list => [1]],
	   [ Witness: v['a]], 
	   [ Witness: v['a]],
	   [ Witness: v['a]],
	   [ Witness: v['a]], 
	   [ Witness: fn x:int => true],
	   [ Witness: fn x:(int->bool) => fn x:int list => [1]], 
	   [ Witness: 1],
	   [ Witness: v['a]], 
	   [ Witness: 1],
	   [ Witness: fn x:int => true],
	   [ Witness: v['a]], 
	   [ Witness: v['a] ] *)

(* --- Case & Arith Expression example --- *)

	val arithExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
							  [(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("z")))]),
							    ArithExpr(PLUS,Value(Concrete(N(3))),
											   ArithExpr(TIMES,Variable(Var("y")),Variable(Var("z")))))])));
	(* fn x:'b => case x of {a=y,b=z} -> 3+(y*z) *)									   
	
	val var_types = [(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int)])),
					 (Var("y"),Int),
					 (Var("z"),Int)];
	
	val SOME(fuzzArithExpr) = fuzz(toCounterExpr(arithExpr),var_types);
	(* [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z],
  	   [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z],
  	   [fn x:'b => case x of {a=y, b=z} -> 3 :: y * z],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + y :: z],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + y * x],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + x * z],
	   [fn x:'b => case x of {a=y, b=z} -> 2.0 + y * z],
	   [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z] *)
	   
	prettyPrintFindWitnessList(findWitnessList(fuzzArithExpr));
	(* [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: {a=v['a658], b=v['a659]}], 
	   [ Witness: {a=v['a664], b=v['a665]}], 
	   [ Witness: {a=1, b=v['a673]}], 
	   [ Witness: {a=v['a678], b=v['a679]}], 
	   [ Result :3.0, with witness: {a=1.0, b=1.0}], 
	   [ Witness: v['a] ] *)

(* --- Case & Lists example --- *)

	(* Tests if a list empty or not *)
	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PVal(EmptyList),Value(Concrete(B(true)))),
						 (PWildcard,Value(Concrete(B(false))))])));
	(* fn x:'b => case x of [] -> true | _ => false *)
			
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c")))))];
			
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	(* [fn x:'b => case true of [ ] -> true | _ -> false],
	   [fn x:'b => case true of [ ] -> true | _ -> false],
	   [fn x:'b => case x of [ ] -> true | _ -> 2],
   	   [fn x:'b => case x of [ ] -> 2 | _ -> false],
	   [fn x:'b => case true of [ ] -> true | _ -> false] *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
	(* [ Witness: v['a]], 
	   [ Witness: v['a]], 
	   [ Witness: [v['a707]]], 
	   [ Witness: [v['a712]]], 
	   [ Witness: v['a] ] *)

(* --- Lists example --- *)

	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(true)))]),
						  List([Variable(Var("y"))])),
						 (PWildcard,Value(Concrete(EmptyList)))])));
	(* fn x:'b => case x of {a=y,b=true} -> [y] | _ -> [] *)
						 
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c"))))),
				     (Var("y"),THole(TypeHole(TypeVar("c"))))];
						 
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	prettyPrintFuzzList(toCounterExpr(listExpr),var_types);
	(* [fn x:'b => case true of {a=y, b=true} -> [y] | _ -> [ ]],
       [fn x:'b => case true of {a=y, b=true} -> [y] | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [y] | _ -> {}],
	   [fn x:'b => case x of {a=y, b=true} -> {a0=y} | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [x] | _ -> [ ]],
	   [fn x:'b => case true of {a=y, b=true} -> [y] | _ -> [ ]] *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
 (* [ Witness: v['a]], 
	[ Witness: v['a]], 
	[ Witness: {a=v['a727], b=true}], 
	[ Witness: {a=v['a733], b=true}], 
	[ Result :[{a=v['a859],b=true}}], with witness: {a=v['a743], b=true}], 
	[ Witness: v['a] ] *)
	
(* --- Simple function application example --- *)

	val funExpr = Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2))))));
	
	val var_types = [(Var("x"),Int)];
	
	val SOME(fuzzFunExpr) = fuzz(toCounterExpr(funExpr),var_types);
	prettyPrintFuzzList(toCounterExpr(funExpr),var_types);
	(* [fn x:real => x*2],
	   [fn x:int => x::2],
	   [fn x:int => x*2.0],
	   [fn x:int => 1.0*2] *)
	   
	prettyPrintFindWitnessList(findWitnessList(fuzzFunExpr));
	
	prettyPrintExpression(Expression(toCounterExpr(funExpr)));
	
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)