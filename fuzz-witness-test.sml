(* 
	Write down expression, and generate CounterExpression version
	Hand-generate the variables->types list
	Call fuzz() to generate a list of possible fuzzed versions of the expression
	Call iterFindWitness to generate a list of witness strings
	Call prettyPrintFindWitnessList to print those strings
*)

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
	prettyPrintFuzzList(toCounterExpr(arithExpr),var_types);
	(* [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z],
  	   [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z],
  	   [fn x:'b => case x of {a=y, b=z} -> 3 :: y * z],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + y :: z],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + y * x],
	   [fn x:'b => case x of {a=y, b=z} -> 3 + x * z],
	   [fn x:'b => case x of {a=y, b=z} -> 2.0 + y * z],
	   [fn x:'b => case fn x:int => [true] of {a=y, b=z} -> 3 + y * z] *)
	   
	prettyPrintExpression(Expression(toCounterExpr(arithExpr)));
	(* ( fn x:'b => (case (x,[1]) of {a=y, b=z} -> ( (3,[2]) + ( (y,[3]) * (z,[4]), [5]), [6]), [7]), [8]) *)
	   
	prettyPrintFindWitnessList(findWitnessList(fuzzArithExpr));
	(* [ Witness: v['a0],stuck at expression 1; expression changed was 1; expressions impacted were [8, 7, 1]],
  	   [ Witness: v['a9],stuck at expression 1; expression changed was 1; expressions impacted were [8, 7, 1]], 
	   [ Witness: {a=v['a26], b=v['a27]},stuck at expression 5; expression changed was 6; expressions impacted were [7, 2, 5, 6]],
	   [ Witness: {a=v['a41], b=v['a42]},stuck at expression 5; expression changed was 5; expressions impacted were [2, 6, 3, 4, 5]],
	   [ Witness: {a=1, b=v['a57]},stuck at expression 4; expression changed was 4; expressions impacted were [3, 5, 4]],
	   [ Witness: {a=v['a69], b=v['a70]},stuck at expression 3; expression changed was 3; expressions impacted were [4, 5, 3]], 
	   [ Result: 3.0, with witness: v['a]; expression changed was 2; expressions impacted were [5, 6, 2]], 
	   [ Witness: v['a97],stuck at expression 1; expression changed was 1; expressions impacted were [7, 1] ] *)

(* --- Case & Lists example --- *)

	(* Tests if a list empty or not *)
	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PVal(EmptyList),Value(Concrete(B(true)))),
						 (PCons(PWildcard,PWildcard),Value(Concrete(B(false)))),
						 (PWildcard,Value(Concrete(B(false))))])));
	(* fn x:'b => case x of [] -> true | _::_ => false | _ => false *)
			
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c")))))];
			
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	prettyPrintFuzzList(toCounterExpr(listExpr),var_types);
	(* [fn x:'b => case true of [ ] -> true | _::_ -> false | _ -> false],
	   [fn x:'b => case true of [ ] -> true | _::_ -> false | _ -> false], 
	   [fn x:'b => case x of [ ] -> true | _::_ -> false | _ -> 2], 
	   [fn x:'b => case x of [ ] -> true | _::_ -> 2 | _ -> false], 
	   [fn x:'b => case x of [ ] -> 2 | _::_ -> false | _ -> false],
	   [fn x:'b => case true of [ ] -> true | _::_ -> false | _ -> false] *)
	   
	prettyPrintExpression(Expression(toCounterExpr(listExpr)));
	(* (fn x:'b => (case (x,[1]) of [ ] -> (true,[2]) | _::_ -> (false,[3]) | _ -> (false,[4]), [5]), [6]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
	(* [ Witness: v['a106],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	   [ Witness: v['a111],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	   [ Witness: [v['a126]],stuck at expression 4; expression changed was 4; expressions impacted were [5, 3, 2, 4]],
	   [ Witness: [v['a137]],stuck at expression 3; expression changed was 3; expressions impacted were [5, 2, 4, 3]],
	   [ Witness: [v['a148]],stuck at expression 3; expression changed was 2; expressions impacted were [5, 3, 4, 2]],
	   [ Witness: v['a149],stuck at expression 1; expression changed was 1; expressions impacted were [5, 1] ] *)
	   
(* --- Lists example --- *)

	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(true)))]),
						  List([Variable(Var("y")),Variable(Var("y"))])),
						 (PRecord([(Lab("a"),PVar(Var("z"))),(Lab("b"),PVal(B(false)))]),
						  List([Variable(Var("z"))])),
						 (PWildcard,Value(Concrete(EmptyList)))])));
	(* fn x:'b => case x of {a=y,b=true} -> [y,y] | {a=z,b=false} -> [z] | _ -> [] *)
						 
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c"))))),
				     (Var("y"),THole(TypeHole(TypeVar("c")))),
					 (Var("z"),THole(TypeHole(TypeVar("c"))))];
						 
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	(* [fn x:'b => case true of {a=y, b=true} -> [y, y] | {a=z, b=false} -> [z] | _ -> [ ]],
   	   [fn x:'b => case true of {a=y, b=true} -> [y, y] | {a=z, b=false} -> [z] | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [y, y] | {a=z, b=false} -> [z] | _ -> {}],
	   [fn x:'b => case x of {a=y, b=true} -> [y, y] | {a=z, b=false} -> {a0=z} | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [y, y] | {a=z, b=false} -> [x] | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> {a0=y, a1=y} | {a=z, b=false} -> [z] | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [y, x] | {a=z, b=false} -> [z] | _ -> [ ]],
	   [fn x:'b => case x of {a=y, b=true} -> [x, y] | {a=z, b=false} -> [z] | _ -> [ ]],
	   [fn x:'b => case true of {a=y, b=true} -> [y, y] | {a=z, b=false} -> [z] | _ -> [ ]] *)
	
	prettyPrintExpression(Expression(toCounterExpr(listExpr)));
	(* (fn x:'b => (case (x, [1]) of {a=y, b=true} -> ( [(y,[2]), (y,[3])],[4]) | {a=z, b=false} -> ([(z,[5])], [6]) | _ -> ([ ],[7]), [8]), [9]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
 (* [ Witness: v['a106],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	[ Witness: v['a111],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	[ Witness: [v['a126]],stuck at expression 4; expression changed was 4; expressions impacted were [5, 3, 2, 4]],
	[ Witness: [v['a137]],stuck at expression 3; expression changed was 3; expressions impacted were [5, 2, 4, 3]],
	[ Witness: [v['a148]],stuck at expression 3; expression changed was 2; expressions impacted were [5, 3, 4, 2]],
	[ Witness: v['a149],stuck at expression 1; expression changed was 1; expressions impacted were [5, 1] ] *)
	
(* --- Simple function application example --- *)

	val funExpr = Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2))))));
	
	val var_types = [(Var("x"),Int)];
	
	val SOME(fuzzFunExpr) = fuzz(toCounterExpr(funExpr),var_types);
	(* [fn x:real => x*2],
	   [fn x:int => x::2],
	   [fn x:int => x*2.0],
	   [fn x:int => 1.0*2] *)
	   
	prettyPrintExpression(Expression(toCounterExpr(funExpr))); 
	(* (fn x:int => ( (x,[1]) * (2,[2]), [3]), [4]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzFunExpr));
	(* [ Witness: 1.0,stuck at expression 2; expression changed was 4; expressions impacted were [4]],
	   [ Witness: 1,stuck at expression 2; expression changed was 3; expressions impacted were [4, 1, 2, 3]],
	   [ Witness: 1,stuck at expression 2; expression changed was 2; expressions impacted were [1, 3, 2]],
	   [ Witness: 1,stuck at expression 2; expression changed was 1; expressions impacted were [2, 3, 1] ] *)
	
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
	(* [ Witness: v['a279],stuck at expression 30; expression changed was 30; expressions impacted were [31, 30]], 
	   [ Witness: v['a282],stuck at expression 30; expression changed was 30; expressions impacted were [31, 30]],
	   [ Witness: v['''a391],stuck at expression 29; expression changed was 29; expressions impacted were [30, 27, 28, 29]],
	   [ Witness: v['a432],stuck at expression 28; expression changed was 28; expressions impacted were [27, 29, 28]],
	   [ Witness: v['a473],stuck at expression 27; expression changed was 27; expressions impacted were [28, 29, 22, 26, 27]],
	   [ Witness: [1.0],stuck at expression 27; expression changed was 26; expressions impacted were [22, 27, 26]],
	   [ Witness: [1],stuck at expression 27; expression changed was 25; expressions impacted were [26, 23, 24, 25]],
	   [ Witness: [1],stuck at expression 27; expression changed was 24; expressions impacted were [23, 25, 24]],
	   [ Witness: [1],stuck at expression 27; expression changed was 23; expressions impacted were [24, 25, 23]],
	   [ Witness: fn x:(int -> bool) => v['a855],stuck at expression 22; expression changed was 22; expressions impacted were [26, 27, 22]],
	   [ Witness: v['a863],stuck at expression 30; expression changed was 21; expressions impacted were [30, 21]],
	   [ Witness: v['a866],stuck at expression 30; expression changed was 20; expressions impacted were [21, 20]],
	   [ Witness: v['a871],stuck at expression 30; expression changed was 1; expressions impacted were [20, 19, 1]],
	   [ Witness: [1],stuck at expression 5; expression changed was 5; expressions impacted were [19, 2, 18, 3, 4, 5]],
	   [ Witness: [1],stuck at expression 17; expression changed was 17; expressions impacted were [12, 18, 15, 16, 17]],
	   [ Witness: [1],stuck at expression 16; expression changed was 16; expressions impacted were [15, 17, 16]],
	   [ Witness: [1],stuck at expression 15; expression changed was 15; expressions impacted were [16, 17, 13, 14, 15]],
	   [ Witness: [1],stuck at expression 14; expression changed was 14; expressions impacted were [13, 15, 14]], 
	   [ Witness: [1],stuck at expression 13; expression changed was 13; expressions impacted were [14, 15, 13]], 
	   [ Witness: [1],stuck at expression 12; expression changed was 12; expressions impacted were [17, 18, 6, 11, 12]],
	   [ Witness: [1],stuck at expression 11; expression changed was 11; expressions impacted were [6, 12, 9, 10, 11]], 
	   [ Witness: [1],stuck at expression 10; expression changed was 10; expressions impacted were [9, 11, 10]],
	   [ Witness: [1],stuck at expression 9; expression changed was 9; expressions impacted were [10, 11, 7, 8, 9]],
	   [ Witness: [1],stuck at expression 8; expression changed was 8; expressions impacted were [7, 9, 8]], 
	   [ Witness: [1],stuck at expression 7; expression changed was 7; expressions impacted were [8, 9, 7]], 
	   [ Result: [ ], with witness: v['a]; expression changed was 6; expressions impacted were [11, 12, 6]], 
	   [ Witness: [1],stuck at expression 5; expression changed was 5; expressions impacted were [18, 3, 4, 5]],
	   [ Witness: [1],stuck at expression 4; expression changed was 4; expressions impacted were [3, 5, 4]] *)   				   
	 
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)