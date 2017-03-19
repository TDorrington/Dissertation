(*

Programs

- fn x:'b => case x of {a=y,b=z} -> 3+(y*z)

- fn x:'b => case x of [] -> true | _::_ => false | _ => false

- fn x:'b => case x of {a=y,b=true} -> [y,y] | {a=z,b=false} -> [z] | _ -> []

- fn x:int => fn y:int => x+y

- fn x => fn y => x::x::y

- fn list:'b => 
  let val rec filter : ('c->bool)->'c list -> 'c list = fn p:('c->bool) => fn l:'c list => 
					    case l of [] -> [] | x::xs -> if (p x) then x::(filter p xs) else (filter p xs)
  in filter (fn y:int => y=10) list
				 
- fn l1:'a => fn l2:'b => 
  let val rec merge:'d = fn record:'c => case record of {a=[],b=ss} => ss | {a=sss,b=[]} => sss | {a=x::xs,b=y::ys} => if x<=y then x::(merge {a=xs,b=y::ys}) else y::(marge {a=x::xs,b=ys})
  in merge {a=l1,b=l2} end
  
- fn pred => fn argList => 
  let val rec exists = fn p => fn l => case l of [] => false | x::xs => x orelse (exists p xs)
  in exists pred argList end
  
- fn matA => fn matB => 
  let val rec dotprod = fn matrices => case matrices of {a=[],b=[]} => 0.0 | {a=x::xs,b=y::ys} => x*y + (dotprod {a=xs,b=ys{)
  in dotprod {a=matA,b=matB}
  
- fn func:('a->'b) => fn list:('a list) =>
  let val rec map : ('a->'b) -> 'a list -> 'b list = 
  fn f:('a->'b) => fn l:('a list) => case l of [] => [] | x::xs => (f x)::(map f xs)
  in map func list end 
  
- fn x:int => let val rec fac:(int->int) = fn n:int => if n<=1 then 1 else n * fac (n-1) in fac x end

(subtract one from all counters - we are excluding the 1 occurence here)

Fuzzed expression passed ML type checker (i.e. failed):      +t+ : 185
Fuzzed expression failed ML type checer (i.e was well-typed) -t- : 0

Witness for fuzzed expression correct and stuck expression same as expression changed      +w+ : 104
Witness for fuzzed expression correct and stuck expression in list of impacted expressions /w/ : 53
Witness for fuzzed expresssion correct, but stuck expression neither same/impacted         |w| : 24
Witness for fuzzed expression wrong														   -w- : 0

Whatever difference remains is the number of expressions that evalauted to a result (i.e. did not get stuck)

*)

(* --- Case & Arith Expression example --- *)
(* fn x:'b => case x of {a=y,b=z} -> 3+(y*z) *)		

	val arithExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
							  [(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVar(Var("z")))]),
							    ArithExpr(PLUS,Value(Concrete(N(3))),
											   ArithExpr(TIMES,Variable(Var("y")),Variable(Var("z")))))])));							   

	val var_types = [(Var("x"),TRecord([(Lab("a"),Int),(Lab("b"),Int)])),
					 (Var("y"),Int),
					 (Var("z"),Int)];
	
	val SOME(fuzzArithExpr) = fuzz(toCounterExpr(arithExpr),var_types);
	(* +t+ [fn x => case (fn x:int => [true]) of {a=y, b=z} => (3) + ((y) * (z))],
	   +t+ [fn x => case (fn x:int => [true]) of {a=y, b=z} => (3) + ((y) * (z))], 
	   +t+ [fn x => case (x) of {a=y, b=z} => (3) :: ((y) * (z))], 
	   +t+ [fn x => case (x) of {a=y, b=z} => (3) + ((y) :: (z))], 
	   +t+ [fn x => case (x) of {a=y, b=z} => (3) + ((y) * (x))],
	   +t+ [fn x => case (x) of {a=y, b=z} => (3) + ((x) * (z))],
	   +t+ [fn x => case (x) of {a=y, b=z} => (2.0) + ((y) * (z))],
	   +t+ [fn x => case (fn x:int => [true]) of {a=y, b=z} => (3) + ((y) * (z))] *)
	   
	prettyPrintExpression(Expression(toCounterExpr(arithExpr)));
	(* ( fn x:'b => (case (x,[1]) of {a=y, b=z} -> ( (3,[2]) + ( (y,[3]) * (z,[4]), [5]), [6]), [7]), [8]) *)
	   
	prettyPrintFindWitnessList(findWitnessList(fuzzArithExpr));
	(* +w+ [ Witness: v['a0],stuck at expression 1; expression changed was 1; expressions impacted were [8, 7, 1]], 
	   +w+ [ Witness: v['a9],stuck at expression 1; expression changed was 1; expressions impacted were [8, 7, 1]], 
	   /w/ [ Witness: {a=v['a26], b=v['a27]},stuck at expression 5; expression changed was 6; expressions impacted were [7, 2, 5, 6]],
	   +w+ [ Witness: {a=v['a41], b=v['a42]},stuck at expression 5; expression changed was 5; expressions impacted were [2, 6, 3, 4, 5]],
	   +w+ [ Witness: {a=1, b=v['a57]},stuck at expression 4; expression changed was 4; expressions impacted were [3, 5, 4, 1]], 
	   +w+ [ Witness: {a=v['a69], b=v['a70]},stuck at expression 3; expression changed was 3; expressions impacted were [4, 5, 3, 1]], 
	       [ Result: 3.0, with witness: {a=1.0, b=1.0}; expression changed was 2; expressions impacted were [5, 6, 2]], 
	   +w+ [ Witness: v['a97],stuck at expression 1; expression changed was 1; expressions impacted were [7, 1] ] *)
   
	  
(* --- Case & Lists example --- *)
(* fn x:'b => case x of [] -> true | _::_ => false | _ => false *)

	(* Tests if a list empty or not *)
	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PVal(EmptyList),Value(Concrete(B(true)))),
						 (PCons(PWildcard,PWildcard),Value(Concrete(B(false)))),
						 (PWildcard,Value(Concrete(B(false))))])));
	
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c")))))];
			
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	(* +t+ [fn x => case (true) of [ ] => true | _::_ => false | _ => false],
	   +t+ [fn x => case (true) of [ ] => true | _::_ => false | _ => false],
	   +t+ [fn x => case (x) of [ ] => true | _::_ => false | _ => 2],
	   +t+ [fn x => case (x) of [ ] => true | _::_ => 2 | _ => false],
	   +t+ [fn x => case (x) of [ ] => 2 | _::_ => false | _ => false],
	   +t+ [fn x => case (true) of [ ] => true | _::_ => false | _ => false] *)
	   
	prettyPrintExpression(Expression(toCounterExpr(listExpr)));
	(* (fn x:'b => (case (x,[1]) of [ ] -> (true,[2]) | _::_ -> (false,[3]) | _ -> (false,[4]), [5]), [6]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
	(* +w+ [ Witness: v['a106],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	   +w+ [ Witness: v['a111],stuck at expression 1; expression changed was 1; expressions impacted were [6, 5, 1]],
	   +w+ [ Witness: [v['a126]],stuck at expression 4; expression changed was 4; expressions impacted were [5, 3, 2, 4]],
	   +w+ [ Witness: [v['a137]],stuck at expression 3; expression changed was 3; expressions impacted were [5, 2, 4, 3]],
	   /w/ [ Witness: [v['a148]],stuck at expression 3; expression changed was 2; expressions impacted were [5, 3, 4, 2]],
 	   +w+ [ Witness: v['a149],stuck at expression 1; expression changed was 1; expressions impacted were [5, 1] ] *)
   
	   
(* --- Lists example --- *)
(* fn x:'b => case x of {a=y,b=true} -> [y,y] | {a=z,b=false} -> [z] | _ -> [] *)

	val listExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),Case(Variable(Var("x")),
						[(PRecord([(Lab("a"),PVar(Var("y"))),(Lab("b"),PVal(B(true)))]),
						  List([Variable(Var("y")),Variable(Var("y"))])),
						 (PRecord([(Lab("a"),PVar(Var("z"))),(Lab("b"),PVal(B(false)))]),
						  List([Variable(Var("z"))])),
						 (PWildcard,Value(Concrete(EmptyList)))])));
	
	val var_types = [(Var("x"),TList(THole(TypeHole(TypeVar("c"))))),
				     (Var("y"),THole(TypeHole(TypeVar("c")))),
					 (Var("z"),THole(TypeHole(TypeVar("c"))))];
						 
	val SOME(fuzzListExpr) = fuzz(toCounterExpr(listExpr),var_types);
	(* +t+ [fn x => case (true) of {a=y, b=true} => [y, y] | {a=z, b=false} => [z] | _ => [ ]],
       +t+ [fn x => case (true) of {a=y, b=true} => [y, y] | {a=z, b=false} => [z] | _ => [ ]],
	   +t+ [fn x => case (x) of {a=y, b=true} => [y, y] | {a=z, b=false} => [z] | _ => {}],
	   +t+ [fn x => case (x) of {a=y, b=true} => [y, y] | {a=z, b=false} => {a0=z} | _ => [ ]],
	   +t+ [fn x => case (x) of {a=y, b=true} => [y, y] | {a=z, b=false} => [x] | _ => [ ]],
	   +t+ [fn x => case (x) of {a=y, b=true} => {a0=y, a1=y} | {a=z, b=false} => [z] | _ => [ ]],
	   +t+ [fn x => case (x) of {a=y, b=true} => [y, x] | {a=z, b=false} => [z] | _ => [ ]],
	   +t+ [fn x => case (x) of {a=y, b=true} => [x, y] | {a=z, b=false} => [z] | _ => [ ]],
	   +t+ [fn x => case (true) of {a=y, b=true} => [y, y] | {a=z, b=false} => [z] | _ => [ ]] *)
	
	prettyPrintExpression(Expression(toCounterExpr(listExpr)));
	(* (fn x:'b => (case (x, [1]) of {a=y, b=true} -> ( [(y,[2]), (y,[3])],[4]) | {a=z, b=false} -> ([(z,[5])], [6]) | _ -> ([ ],[7]), [8]), [9]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzListExpr));
	(* +w+ [ Witness: v['a154],stuck at expression 1; expression changed was 1; expressions impacted were [9, 8, 1]], 
	   +w+ [ Witness: v['a161],stuck at expression 1; expression changed was 1; expressions impacted were [9, 8, 1]], 
	   +w+ [ Witness: {a=v['a180], b=true},stuck at expression 7; expression changed was 7; expressions impacted were [8, 6, 4, 7]], 
	   +w+ [ Witness: {a=v['a193], b=true},stuck at expression 6; expression changed was 6; expressions impacted were [8, 4, 7, 6]], 
	   +w+ [ Witness: {a=v['a206], b=true},stuck at expression 5; expression changed was 5; expressions impacted were [6, 5, 1]], 
	   /w/ [ Witness: {a=v['a220], b=true},stuck at expression 6; expression changed was 4; expressions impacted were [8, 6, 7, 4]], 
	   +w+ [ Witness: {a=v['a234], b=true},stuck at expression 3; expression changed was 3; expressions impacted were [4, 2, 3, 1]], 
	   /w/ [ Witness: {a=v['a245], b=true},stuck at expression 3; expression changed was 2; expressions impacted were [4, 3, 2, 1]], 
	   +w+ [ Witness: v['a252],stuck at expression 1; expression changed was 1; expressions impacted were [8, 1] ] *)
	  
(* --- Currying example --- *)
(* fn x:int => fn y:int => x+y *)

	val currExpr = Value(Fun(Var("x"),Int,Value(Fun(Var("y"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))));
	
	val var_types = [(Var("x"),Int),(Var("y"),Int)];
	
	val SOME(fuzzCurrExpr) = fuzz(toCounterExpr(currExpr),var_types);
	(* +t+ [fn x:real => fn y:int => (x) + (y)],
  	   +t+ [fn x:int => fn y:real => (x) + (y)],
	   +t+ [fn x:int => fn y:int => (x) :: (y)],
	   +t+ [fn x:int => fn y:int => (x) + (1.0)],
	   +t+ [fn x:int => fn y:int => (1.0) + (y)] *)
	
	prettyPrintExpression(Expression(toCounterExpr(currExpr)));
	(* (fn x:int => (fn y:int => ( (x,[1]) + (y,[2]), [3]), [4]), [5]) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzCurrExpr));
	(* |w| [ Witness: 1.0, 1,stuck at expression 2; expression changed was 5; expressions impacted were [5]], 
	   |w| [ Witness: 1, 1.0,stuck at expression 2; expression changed was 4; expressions impacted were [5, 4]], 
	   /w/ [ Witness: 1, 1,stuck at expression 2; expression changed was 3; expressions impacted were [4, 1, 2, 3]], 
	   +w+ [ Witness: 1, 1,stuck at expression 2; expression changed was 2; expressions impacted were [1, 3, 2]], 
	   /w/ [ Witness: 1, 1,stuck at expression 2; expression changed was 1; expressions impacted were [2, 3, 1] ] *)
	   
(* --- Cons example --- *)
(* fn x => fn y => x::x::y *)

	val consExpr = Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
				   Cons(Variable(Var("x")),Cons(Variable(Var("x")),Variable(Var("y"))))))));
			   
	val var_types = [(Var("x"),Int),(Var("y"),TList(Int))];

	val SOME(fuzzConsExpr) = fuzz(toCounterExpr(consExpr),var_types);
	(* +t+ [fn x => fn y => (x) = ((x) :: (y))], 
	   +t+ [fn x => fn y => (x) = ((x) :: (y))],
	   +t+ [fn x => fn y => (x) = ((x) :: (y))],
	   +t+ [fn x => fn y => (x) :: ((x) = (y))],
	   +t+ [fn x => fn y => (x) :: ((x) :: (x))],
	   +t+ [fn x => fn y => (x) :: ((y) :: (y))],
	   +t+ [fn x => fn y => (y) :: ((x) :: (y))] *)
	
	prettyPrintExpression(Expression(toCounterExpr(consExpr)));
	(* ( fn x => ( fn y => ( (( x, [1] )) :: (( (( x, [2] )) :: (( y, [3] )), [4] )), [5] ), [6] ), [7] ) *)
	
	prettyPrintFindWitnessList(findWitnessList(fuzzConsExpr));
	(* |w| [ Witness: [v[''a371]], v['a354],stuck at expression 2; expression changed was 5; expressions impacted were [7, 6, 1, 4, 5]],
 	   +w+ [ Witness: v['a438], v['a426],stuck at expression 4; expression changed was 4; expressions impacted were [1, 5, 2, 3, 4]], 
	   +w+ [ Witness: v['a456], v['a444],stuck at expression 3; expression changed was 3; expressions impacted were [2, 4, 3, 1, 2]],
	   /w/ [ Witness: v['a474], v['a474],stuck at expression 3; expression changed was 2; expressions impacted were [3, 4, 2, 3]],
	   /w/ [ Witness: v['a492], v['a492],stuck at expression 3; expression changed was 1; expressions impacted were [4, 5, 1, 3] ] *)
   
(* --- Filter example --- *) 

	val c' = THole(TypeHole(TypeVar("c")));	

	(* Function which takes a list and filters the elements of the list based on predicate (fn x:int => x = 10) *)
	val filterExpr = 
		Value(Fun(Var("argList"),THole(TypeHole(TypeVar("b"))),
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
			Variable(Var("argList"))))));
	(* fn list:'b => let val rec filter : ('c->bool)->'c list -> 'c list = fn p:('c->bool) => fn l:'c list => 
							case l of [] -> [] | x::xs -> if (p x) then x::(filter p xs) else (filter p xs)
					 in filter (fn y:int => y=10) list *)
				
	val var_types = [(Var("argList"),TList(Int)),
					 (Var("filter"),TFun(TFun(Int,Bool),TFun(TList(Int),TList(Int)))),
					 (Var("p"),TFun(Int,Bool)),
					 (Var("l"),TList(Int)),
					 (Var("x"),Int),
					 (Var("xs"),TList(Int)),
					 (Var("y"),Int)];
				
	val SOME(fuzzFilterExpr) = fuzz(toCounterExpr(filterExpr),var_types);
	(* +t+ [fn argList => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) + (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (filter)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) + (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:real => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (2.0))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (argList) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((argList) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:{a:int} => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:bool => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (filter) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) + (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) + (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (filter))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) + (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (argList)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((argList) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) = (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) + (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (filter))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) + (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (argList)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((argList) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((argList) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) + (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((p) (argList)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => [ ] | x::xs => if ((argList) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (l) of [ ] => {} | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end],
	   +t+ [fn argList => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case (filter) of [ ] => [ ] | x::xs => if ((p) (x)) then  ((x) :: (((filter) (p)) (xs))) else (((filter) (p)) (xs))) in (((filter) (fn y:int => (y) = (10))) (argList)) end] *)
  
	prettyPrintExpression(Expression(toCounterExpr(filterExpr)));
	(* ( fn argList => ( let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (( fn p:('c -> bool) => ( fn l:'c list => ( case (( l, [1] )) of [ ] => ( [ ], [2] ) | x::xs => ( if (( (( p, [3] )) (( x, [4] )), [5] )) then  (( (( x, [6] )) :: (( (( (( filter, [7] )) (( p, [8] )), [9] )) (( xs, [10] )), [11] )), [12] )) else (( (( (( filter, [13] )) (( p, [14] )), [15] )) (( xs, [16] )), [17] )), [18] ), [19] ), [20] ), [21] )) in (( (( (( filter, [22] )) (( fn y:int => ( (( y, [23] )) = (( 10, [24] )), [25] ), [26] )), [27] )) (( argList, [28] )), [29] )) end, [30] ), [31] ) *)
   
	prettyPrintFindWitnessList(findWitnessList(fuzzFilterExpr));
	(* +w+ [ Witness: v['a493],stuck at expression 30; expression changed was 30; expressions impacted were [31, 30]],
	   +w+ [ Witness: v['a496],stuck at expression 30; expression changed was 30; expressions impacted were [31, 30]], 
	   +w+ [ Witness: v['''a605],stuck at expression 29; expression changed was 29; expressions impacted were [30, 27, 28, 29]], 
	   +w+ [ Witness: v['a646],stuck at expression 28; expression changed was 28; expressions impacted were [27, 29, 28, 7, 13, 22]], 
	   +w+ [ Witness: v['a687],stuck at expression 27; expression changed was 27; expressions impacted were [28, 29, 22, 26, 27]], 
	   /w/ [ Witness: [1.0],stuck at expression 27; expression changed was 26; expressions impacted were [22, 27, 26]], 
	   |w| [ Witness: [1],stuck at expression 27; expression changed was 25; expressions impacted were [26, 23, 24, 25]], 
	   |w| [ Witness: [1],stuck at expression 27; expression changed was 24; expressions impacted were [23, 25, 24]], 
	   |w| [ Witness: [1],stuck at expression 27; expression changed was 23; expressions impacted were [24, 25, 23, 28]], 
	   +w+ [ Witness: fn x:(int -> bool) => v['a1069],stuck at expression 22; expression changed was 22; expressions impacted were [26, 27, 22, 28]], 
	   /w/ [ Witness: v['a1077],stuck at expression 30; expression changed was 21; expressions impacted were [30, 21]], 
	   |w| [ Witness: v['a1080],stuck at expression 30; expression changed was 20; expressions impacted were [21, 20]], 
	   |w| [ Witness: v['a1085],stuck at expression 30; expression changed was 1; expressions impacted were [20, 19, 1, 7, 13, 22]], 
	   +w+ [ Witness: [1],stuck at expression 5; expression changed was 5; expressions impacted were [19, 2, 18, 3, 4, 5]], 
	   +w+ [ Witness: [1],stuck at expression 17; expression changed was 17; expressions impacted were [12, 18, 15, 16, 17]], 
	   +w+ [ Witness: [1],stuck at expression 16; expression changed was 16; expressions impacted were [15, 17, 16, 7, 13, 22]], 
	   +w+ [ Witness: [1],stuck at expression 15; expression changed was 15; expressions impacted were [16, 17, 13, 14, 15]], 
	   +w+ [ Witness: [1],stuck at expression 14; expression changed was 14; expressions impacted were [13, 15, 14, 28]], 
	   +w+ [ Witness: [1],stuck at expression 13; expression changed was 13; expressions impacted were [14, 15, 13, 28]], 
	   +w+ [ Witness: [1],stuck at expression 12; expression changed was 12; expressions impacted were [17, 18, 6, 11, 12]], 
	   +w+ [ Witness: [1],stuck at expression 11; expression changed was 11; expressions impacted were [6, 12, 9, 10, 11]], 
	   +w+ [ Witness: [1],stuck at expression 10; expression changed was 10; expressions impacted were [9, 11, 10, 7, 13, 22]], 
	   +w+ [ Witness: [1],stuck at expression 9; expression changed was 9; expressions impacted were [10, 11, 7, 8, 9]], 
	   +w+ [ Witness: [1],stuck at expression 8; expression changed was 8; expressions impacted were [7, 9, 8, 28]], 
	   +w+ [ Witness: [1],stuck at expression 7; expression changed was 7; expressions impacted were [8, 9, 7, 28]], 
	       [ Result: [ ], with witness: [1]; expression changed was 6; expressions impacted were [11, 12, 6, 28]], 
	   +w+ [ Witness: [1],stuck at expression 5; expression changed was 5; expressions impacted were [18, 3, 4, 5]], 
	   +w+ [ Witness: [1],stuck at expression 4; expression changed was 4; expressions impacted were [3, 5, 4, 28]], 
	   +w+ [ Witness: [1],stuck at expression 3; expression changed was 3; expressions impacted were [4, 5, 3, 28]], 
	   |w| [ Witness: [1],stuck at expression 12; expression changed was 2; expressions impacted were [19, 18, 2]], 
	   |w| [ Witness: v['a4825],stuck at expression 30; expression changed was 1; expressions impacted were [19, 1, 7, 13, 22] ] *)
   

(* --- Merge example --- *)
(* fn l1:'a => fn l2:'b => let val rec merge:'d = fn record:'c => case record of {a=[],b=ss} => ss | {a=sss,b=[]} => sss | {a=x::xs,b=y::ys} => if x<=y then x::(merge {a=xs,b=y::ys}) else y::(marge {a=x::xs,b=ys})
						   in merge {a=l1,b=l2} end *)
	
	val mergeExpr = Value(Fun(Var("l1"),THole(TypeHole(TypeVar("a"))),Value(Fun(Var("l2"),THole(TypeHole(TypeVar("b"))),
		LetRec(Var("merge"),THole(TypeHole(TypeVar("d"))),
				Value(Fun(Var("record"),THole(TypeHole(TypeVar("c"))),Case(Variable(Var("record")),
					  [(PRecord([(Lab("a"),PVal(EmptyList)),(Lab("b"),PVar(Var("ss")))]),Variable(Var("ss"))),
					   (PRecord([(Lab("a"),PVar(Var("sss"))),(Lab("b"),PVal(EmptyList))]),Variable(Var("sss"))),
					   (PRecord([(Lab("a"),PCons(PVar(Var("x")),PVar(Var("xs")))),(Lab("b"),PCons(PVar(Var("y")),PVar(Var("ys"))))]),
						Condition(BoolExpr(LESS_EQ,Variable(Var("x")),Variable(Var("y"))),
								  Cons(Variable(Var("x")),
									   App(Variable(Var("merge")),
										   Record([(Lab("a"),Variable(Var("xs"))),
												   (Lab("b"),Cons(Variable(Var("y")),Variable(Var("ys"))))]))),
								  Cons(Variable(Var("y")),
									   App(Variable(Var("merge")),
										   Record([(Lab("a"),Cons(Variable(Var("x")),Variable(Var("xs")))),
												   (Lab("b"),Variable(Var("ys")))])))))]))),
				App(Variable(Var("merge")),Record([(Lab("a"),Variable(Var("l1"))),(Lab("b"),Variable(Var("l2")))])))))));
								
	val var_types = [(Var("l1"),TList(Int)),(Var("l2"),TList(Int)),
					 (Var("merge"),TFun(TRecord([(Lab("a"),TList(Int)),(Lab("b"),TList(Int))]),TList(Int))),
					 (Var("record"),TRecord([(Lab("a"),TList(Int)),(Lab("b"),TList(Int))])),
					 (Var("ss"),TList(Int)),(Var("sss"),TList(Int)),
					 (Var("x"),Int),(Var("xs"),TList(Int)),
					 (Var("y"),Int),(Var("ys"),TList(Int))];
				
	val SOME(fuzzMergeExpr) = fuzz(toCounterExpr(mergeExpr),var_types);
(* +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) + ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) + ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) + ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) + ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=merge})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=merge, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((l1) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (l1) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (l1) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) = ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) + ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=merge})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) = (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (merge), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(l1) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((l1) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((l1) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) = ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) + ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) = (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (merge)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(l1) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=merge, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((l1) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((l1) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (l1)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((l1) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => merge | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end], 
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (record) of {a=[ ], b=ss} => merge | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end],
   +t+ [fn l1 => fn l2 => let val rec merge = (fn record => case (l1) of {a=[ ], b=ss} => ss | {a=sss, b=[ ]} => sss | {a=x::xs, b=y::ys} => if ((x) <= (y)) then  ((x) :: ((merge) ({a=xs, b=(y) :: (ys)}))) else ((y) :: ((merge) ({a=(x) :: (xs), b=ys})))) in ((merge) ({a=l1, b=l2})) end] *)
 
	prettyPrintExpression(Expression(toCounterExpr(mergeExpr)));
	(* ( fn l1 => ( fn l2 => ( let val rec merge = (( fn record => ( case (( record, [1] )) of {a=[ ], b=ss} => ( ss, [2] ) | {a=sss, b=[ ]} => ( sss, [3] ) | {a=x::xs, b=y::ys} => ( if (( (( x, [4] )) <= (( y, [5] )), [6] )) then  (( (( x, [7] )) :: (( (( merge, [8] )) (( {a=( xs, [9] ), b=( (( y, [10] )) :: (( ys, [11] )), [12] )}, [13] )), [14] )), [15] )) else (( (( y, [16] )) :: (( (( merge, [17] )) (( {a=( (( x, [18] )) :: (( xs, [19] )), [20] ), b=( ys, [21] )}, [22] )), [23] )), [24] )), [25] ), [26] ), [27] )) in (( (( merge, [28] )) (( {a=( l1, [29] ), b=( l2, [30] )}, [31] )), [32] )) end, [33] ), [34] ), [35] ) *)
   
	prettyPrintFindWitnessList(findWitnessList(fuzzMergeExpr)); 
	(* /w/ [ Witness: v['a4832], v['a4837],stuck at expression 28; expression changed was 32; expressions impacted were [35, 34, 33, 28, 31, 32]],
 	   /w/ [ Witness: v['a4924], v['a4929],stuck at expression 28; expression changed was 32; expressions impacted were [35, 34, 33, 28, 31, 32]], 
	   /w/ [ Witness: v['a5016], v['a5021],stuck at expression 28; expression changed was 32; expressions impacted were [34, 33, 28, 31, 32]], 
	   /w/ [ Witness: v['a5108], v['a5113],stuck at expression 28; expression changed was 32; expressions impacted were [33, 28, 31, 32]], 
	   +w+ [ Witness: v['a5200], v['a5205],stuck at expression 31; expression changed was 31; expressions impacted were [28, 32, 31]], 
	   +w+ [ Witness: [v['''a5399]], v['a5307],stuck at expression 30; expression changed was 30; expressions impacted were [31, 29, 30, 8, 17, 28]], 
	   +w+ [ Witness: v['a5408], v['a5413],stuck at expression 29; expression changed was 29; expressions impacted were [31, 30, 29, 8, 17, 28]], 
	   +w+ [ Witness: v['a5510], v['a5515],stuck at expression 28; expression changed was 28; expressions impacted were [31, 32, 28, 29]],
	   /w/ [ Witness: {a=[v['''a5699]], b=[v['''a5699]]}, v['a5605],stuck at expression 29; expression changed was 1; expressions impacted were [33, 27, 26, 1, 29]], 
	   /w/ [ Witness: {a=[v['''a5809]], b=[v['''a5809]]}, v['a5715],stuck at expression 29; expression changed was 1; expressions impacted were [27, 26, 1, 29]], 
	   /w/ [ Witness: v['a5820], v['a5825],stuck at expression 4; expression changed was 6; expressions impacted were [26, 3, 2, 25, 4, 5, 6]],
	   +w+ [ Witness: v['a5924], v['a5929],stuck at expression 24; expression changed was 24; expressions impacted were [15, 25, 16, 23, 24]], 
	   +w+ [ Witness: v['a6016], v['a6021],stuck at expression 23; expression changed was 23; expressions impacted were [16, 24, 17, 22, 23]], 
	   +w+ [ Witness: v['a6108], v['a6113],stuck at expression 22; expression changed was 22; expressions impacted were [17, 23, 22]],
	   +w+ [ Witness: v['a6208], v['a6213],stuck at expression 21; expression changed was 21; expressions impacted were [22, 20, 21, 8, 17, 28]],
	   +w+ [ Witness: v['a6310], v['a6315],stuck at expression 20; expression changed was 20; expressions impacted were [22, 21, 18, 19, 20]],
	   +w+ [ Witness: v['a6410], v['a6415],stuck at expression 19; expression changed was 19; expressions impacted were [18, 20, 19, 8, 17, 28]],
	   /w/ [ Witness: v['''a6607], v['a6517],stuck at expression 29; expression changed was 18; expressions impacted were [19, 20, 18, 29]],
	   /w/ [ Witness: fn x:{a:'''a6732 list, b:'''a6732 list} => [v['''a6732]], v['a6619],stuck at expression 29; expression changed was 17; expressions impacted were [22, 23, 17, 29]],
	   /w/ [ Witness: v['''a6841], v['a6751],stuck at expression 29; expression changed was 16; expressions impacted were [23, 24, 16, 29]], 
	   +w+ [ Witness: v['a6848], v['a6853],stuck at expression 15; expression changed was 15; expressions impacted were [24, 25, 7, 14, 15]],
	   +w+ [ Witness: v['a6936], v['a6941],stuck at expression 14; expression changed was 14; expressions impacted were [7, 15, 8, 13, 14]],
	   +w+ [ Witness: v['a7024], v['a7029],stuck at expression 13; expression changed was 13; expressions impacted were [8, 14, 13]], 
	   +w+ [ Witness: v['a7124], v['a7129],stuck at expression 12; expression changed was 12; expressions impacted were [13, 9, 10, 11, 12]], 
	   +w+ [ Witness: v['a7224], v['a7229],stuck at expression 11; expression changed was 11; expressions impacted were [10, 12, 11, 8, 17, 28]], 
	   /w/ [ Witness: v['''a7421], v['a7331],stuck at expression 29; expression changed was 10; expressions impacted were [11, 12, 10, 29]], 
	   +w+ [ Witness: v['a7428], v['a7433],stuck at expression 9; expression changed was 9; expressions impacted were [13, 12, 9, 8, 17, 28]], 
	   /w/ [ Witness: fn x:{a:'''a7648 list, b:'''a7648 list} => [v['''a7648]], v['a7535],stuck at expression 29; expression changed was 8; expressions impacted were [13, 14, 8, 29]], 
	   /w/ [ Witness: v['''a7759], v['a7667],stuck at expression 29; expression changed was 7; expressions impacted were [14, 15, 7, 29]],
	   /w/ [ Witness: v['a7766], v['a7771],stuck at expression 4; expression changed was 6; expressions impacted were [25, 4, 5, 6]],
	   /w/ [ Witness: v['''a7965], v['a7875],stuck at expression 29; expression changed was 5; expressions impacted were [4, 6, 5, 29]],
	   /w/ [ Witness: v['''a8069], v['a7977],stuck at expression 29; expression changed was 4; expressions impacted were [5, 6, 4, 29]], 
	   +w+ [ Witness: v['a8076], v['a8081],stuck at expression 3; expression changed was 3; expressions impacted were [26, 2, 25, 3, 8, 17, 28]],
	   /w/ [ Witness: v['a8164], v['a8169],stuck at expression 3; expression changed was 2; expressions impacted were [26, 3, 25, 2, 8, 17, 28]],
	   /w/ [ Witness: {a=[v['''a8363]], b=[v['''a8363]]}, v['a8269],stuck at expression 29; expression changed was 1; expressions impacted were [26, 1, 29] ] *)
						

(* --- Exists example --- *)
(* fn pred => fn argList => 
   let val rec exists = fn p => fn l => case l of [] => false | x::xs => x orelse (exists p xs)
   in exists pred argList end *)

	val existsExpr = Value(Fun(Var("pred"),THole(TypeHole(TypeVar("a"))),Value(Fun(Var("argList"),THole(TypeHole(TypeVar("b"))),
					 LetRec(Var("exists"),THole(TypeHole(TypeVar("c"))),
							Value(Fun(Var("p"),THole(TypeHole(TypeVar("d"))),
							Value(Fun(Var("l"),THole(TypeHole(TypeVar("e"))),
							Case(Variable(Var("l")),
							[(PVal(EmptyList),Value(Concrete(B(false)))),
							 (PCons(PVar(Var("x")),PVar(Var("xs"))),
							  Condition(App(Variable(Var("p")),Variable(Var("x"))),
										Value(Concrete(B(true))),
										App(App(Variable(Var("exists")),Variable(Var("p"))),Variable(Var("xs")))))]))))),
					 App(App(Variable(Var("exists")),Variable(Var("pred"))),Variable(Var("argList"))))))));
				 
	val var_types = [(Var("pred"),TFun(Int,Bool)),
					 (Var("argList"),TList(Int)),
					 (Var("exists"),TFun(TFun(Int,Bool),TFun(TList(Int),Bool))),
					 (Var("p"),TFun(Int,Bool)),
					 (Var("l"),TList(Int)),
					 (Var("x"),Int),
					 (Var("xs"),TList(Int))];
	
	val SOME(fuzzExistsExpr) = fuzz(toCounterExpr(existsExpr),var_types);
	(* +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) + (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) + (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) + (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) + (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (pred)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) + (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (argList)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((pred) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (pred) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (pred) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (pred) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) + (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) + (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (pred))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) + (p)) (xs))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (argList)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((pred) (p)) (xs))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (x)) then  (2) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) + (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((p) (pred)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => false | x::xs => if ((argList) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end],
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (l) of [ ] => 2 | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end], 
	   +t+ [fn pred => fn argList => let val rec exists = (fn p => fn l => case (pred) of [ ] => false | x::xs => if ((p) (x)) then  (true) else (((exists) (p)) (xs))) in (((exists) (pred)) (argList)) end] *)
	
	prettyPrintExpression(Expression(toCounterExpr(existsExpr)));
	(* ( fn pred => ( fn argList => ( let val rec exists = (( fn p => ( fn l => ( case (( l, [1] )) of [ ] => ( false, [2] ) | x::xs => ( if (( (( p, [3] )) (( x, [4] )), [5] )) then  (( true, [6] )) else (( (( (( exists, [7] )) (( p, [8] )), [9] )) (( xs, [10] )), [11] )), [12] ), [13] ), [14] ), [15] )) in (( (( (( exists, [16] )) (( pred, [17] )), [18] )) (( argList, [19] )), [20] )) end, [21] ), [22] ), [23] ) *)
  
	prettyPrintFindWitnessList(findWitnessList(fuzzExistsExpr)); 
	(* +w+ [ Witness: v['a8494], v['''a8491],stuck at expression 20; expression changed was 20; expressions impacted were [23, 22, 21, 18, 19, 20]], 
	   +w+ [ Witness: v['a8644], v['''a8641],stuck at expression 20; expression changed was 20; expressions impacted were [23, 22, 21, 18, 19, 20]], 
	   +w+ [ Witness: v['a8794], v['''a8791],stuck at expression 20; expression changed was 20; expressions impacted were [22, 21, 18, 19, 20]],
	   +w+ [ Witness: v['a8944], v['''a8941],stuck at expression 20; expression changed was 20; expressions impacted were [21, 18, 19, 20]], 
	   |w| [ Witness: [v['a9099]], v['a8979],stuck at expression 3; expression changed was 19; expressions impacted were [18, 20, 19, 17]],
	   +w+ [ Witness: v['a9100], v['a9105],stuck at expression 18; expression changed was 18; expressions impacted were [19, 20, 16, 17, 18]],
	   |w| [ Witness: v['a9158], [v['a9283]],stuck at expression 3; expression changed was 17; expressions impacted were [16, 18, 17, 19]], 
	   +w+ [ Witness: v['a9284], v['a9289],stuck at expression 16; expression changed was 16; expressions impacted were [17, 18, 16, 17]], 
	   |w| [ Witness: [v['a9467]], v['a9347],stuck at expression 3; expression changed was 1; expressions impacted were [21, 15, 14, 13, 1, 17]],
	   |w| [ Witness: [v['a9593]], v['a9473],stuck at expression 3; expression changed was 1; expressions impacted were [15, 14, 13, 1, 17]],
	   |w| [ Witness: [v['a9719]], v['a9599],stuck at expression 3; expression changed was 1; expressions impacted were [14, 13, 1, 17]], 
	   +w+ [ Witness: v['a9720], [v['a9837]],stuck at expression 5; expression changed was 5; expressions impacted were [13, 2, 12, 3, 4, 5]], 
	   +w+ [ Witness: fn x => true, [v['a9987]],stuck at expression 11; expression changed was 11; expressions impacted were [6, 12, 9, 10, 11]],
	   +w+ [ Witness: fn x => true, [v['a10165]],stuck at expression 10; expression changed was 10; expressions impacted were [9, 11, 10, 17]], 
	   +w+ [ Witness: fn x => true, [v['a10346]],stuck at expression 9; expression changed was 9; expressions impacted were [10, 11, 7, 8, 9]], 
	   +w+ [ Witness: fn x => true, [v['a10534]],stuck at expression 8; expression changed was 8; expressions impacted were [7, 9, 8, 19]],
	   +w+ [ Witness: fn x => true, [v['a10707]],stuck at expression 7; expression changed was 7; expressions impacted were [8, 9, 7, 17]],
	   +w+ [ Witness: fn x => true, [v['a10869]],stuck at expression 6; expression changed was 6; expressions impacted were [11, 12, 6]], 
	   +w+ [ Witness: v['a10872], [v['a10989]],stuck at expression 5; expression changed was 5; expressions impacted were [12, 3, 4, 5]],
	   /w/ [ Witness: v['a10990], [v['a11121]],stuck at expression 3; expression changed was 4; expressions impacted were [3, 5, 4, 17]],
	   +w+ [ Witness: v['a11122], [v['a11247]],stuck at expression 3; expression changed was 3; expressions impacted were [4, 5, 3, 19]],
	   |w| [ Witness: fn x => true, [v['a11405]],stuck at expression 6; expression changed was 2; expressions impacted were [13, 12, 2]], 
	   |w| [ Witness: [v['a11533]], v['a11413],stuck at expression 3; expression changed was 1; expressions impacted were [13, 1, 17] ] *)
   
(* --- Matrix Dot Product Example --- *)

val matrixExpr = Value(Fun(Var("matA"),THole(TypeHole(TypeVar("a"))),Value(Fun(Var("matB"),THole(TypeHole(TypeVar("b"))),
				 LetRec(Var("dotprod"),THole(TypeHole(TypeVar("c"))),
				 Value(Fun(Var("matrices"),THole(TypeHole(TypeVar("d"))),Case(Variable(Var("matrices")),
				 [(PRecord([(Lab("a"),PVal(EmptyList)),(Lab("b"),PVal(EmptyList))]),Value(Concrete(R(0.0)))),
				  (PRecord([(Lab("a"),PCons(PVar(Var("x")),PVar(Var("xs")))),(Lab("b"),PCons(PVar(Var("y")),PVar(Var("ys"))))]),
				   ArithExpr(PLUS,
							 ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
							 App(Variable(Var("dotprod")),Record([(Lab("a"),Variable(Var("xs"))),(Lab("b"),Variable(Var("ys")))]))))]))),
				 App(Variable(Var("dotprod")),Record([(Lab("a"),Variable(Var("matA"))),(Lab("b"),Variable(Var("matB")))])))))));
				 
(* fn matA => fn matB => 
   let val rec dotprod = fn matrices => case matrices of {a=[],b=[]} => 0.0 | {a=x::xs,b=y::ys} => x*y + (dotprod {a=xs,b=ys{)
   in dotprod {a=matA,b=matB} *)
   
val var_types = [(Var("matA"),TList(Real)),(Var("matB"),TList(Real)),
				 (Var("dotprod"),TFun(TRecord([(Lab("a"),TList(Real)),(Lab("b"),TList(Real))]),Real)),
				 (Var("matrices"),TRecord([(Lab("a"),TList(Real)),(Lab("b"),TList(Real))])),
				 (Var("x"),Real),(Var("xs"),TList(Real)),
				 (Var("y"),Real),(Var("ys"),TList(Real))];
				
val SOME(fuzzMatrixExpr) = fuzz(toCounterExpr(matrixExpr),var_types);
(* +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) + ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) + ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) + ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) + ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({b=matB})) end], 
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=dotprod})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=dotprod, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((matA) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matA) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matA) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) :: ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) + ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=dotprod}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=dotprod, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((matA) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end], 
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) :: (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end], 
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (matA)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end], 
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((matA) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end], 
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matrices) of {a=[ ], b=[ ]} => false | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end],
   +t+ [fn matA => fn matB => let val rec dotprod = (fn matrices => case (matA) of {a=[ ], b=[ ]} => 0.0 | {a=x::xs, b=y::ys} => ((x) * (y)) + ((dotprod) ({a=xs, b=ys}))) in ((dotprod) ({a=matA, b=matB})) end] *)
   
	
prettyPrintExpression(Expression(toCounterExpr(matrixExpr)));
(* ( fn matA => ( fn matB => ( let val rec dotprod = (( fn matrices => ( case (( matrices, [1] )) of {a=[ ], b=[ ]} => ( 0.0, [2] ) | {a=x::xs, b=y::ys} => ( (( (( x, [3] )) * (( y, [4] )), [5] )) + (( (( dotprod, [6] )) (( {a=( xs, [7] ), b=( ys, [8] )}, [9] )), [10] )), [11] ), [12] ), [13] )) in (( (( dotprod, [14] )) (( {a=( matA, [15] ), b=( matB, [16] )}, [17] )), [18] )) end, [19] ), [20] ), [21] ) *)

prettyPrintFindWitnessList(findWitnessList(fuzzMatrixExpr)); 
(* /w/ [ Witness: v['a11534], v['a11539],stuck at expression 14; expression changed was 18; expressions impacted were [21, 20, 19, 14, 17, 18]],
   /w/ [ Witness: v['a11616], v['a11621],stuck at expression 14; expression changed was 18; expressions impacted were [21, 20, 19, 14, 17, 18]],
   /w/ [ Witness: v['a11698], v['a11703],stuck at expression 14; expression changed was 18; expressions impacted were [20, 19, 14, 17, 18]],
   /w/ [ Witness: v['a11780], v['a11785],stuck at expression 14; expression changed was 18; expressions impacted were [19, 14, 17, 18]],
   +w+ [ Witness: v['a11862], v['a11867],stuck at expression 17; expression changed was 17; expressions impacted were [14, 18, 17]],
   +w+ [ Witness: [1.0], v['a11957],stuck at expression 16; expression changed was 16; expressions impacted were [17, 15, 16, 6, 14]],
   +w+ [ Witness: v['a12046], v['a12051],stuck at expression 15; expression changed was 15; expressions impacted were [17, 16, 15, 6, 14]],
   +w+ [ Witness: v['a12136], v['a12141],stuck at expression 14; expression changed was 14; expressions impacted were [17, 18, 14, 15]],   
   /w/ [ Witness: {a=[1.0], b=[1.0]}, v['a12221],stuck at expression 15; expression changed was 1; expressions impacted were [19, 13, 12, 1, 15]],
   /w/ [ Witness: {a=[1.0], b=[1.0]}, v['a12319],stuck at expression 15; expression changed was 1; expressions impacted were [13, 12, 1, 15]],
   +w+ [ Witness: v['a12412], v['a12417],stuck at expression 11; expression changed was 11; expressions impacted were [12, 2, 5, 10, 11]],  
   /w/ [ Witness: v['a12494], v['a12499],stuck at expression 6; expression changed was 10; expressions impacted were [5, 11, 6, 9, 10]],
   +w+ [ Witness: v['a12572], v['a12577],stuck at expression 9; expression changed was 9; expressions impacted were [6, 10, 9]],
   +w+ [ Witness: v['a12662], v['a12667],stuck at expression 8; expression changed was 8; expressions impacted were [9, 7, 8, 6, 14]],
   +w+ [ Witness: v['a12754], v['a12759],stuck at expression 7; expression changed was 7; expressions impacted were [9, 8, 7, 6, 14]]
   /w/ [ Witness: fn x:{a:real list, b:real list} => 1.0, v['a12851],stuck at expression 15; expression changed was 6; expressions impacted were [9, 10, 6, 15]],
   +w+ [ Witness: v['a12960], v['a12965],stuck at expression 5; expression changed was 5; expressions impacted were [10, 11, 3, 4, 5]],
   /w/ [ Witness: 1.0, v['a13045],stuck at expression 15; expression changed was 4; expressions impacted were [3, 5, 4, 15]], 
   /w/ [ Witness: 1.0, v['a13135],stuck at expression 15; expression changed was 3; expressions impacted were [4, 5, 3, 15]],
   /w/ [ Witness: v['a13220], v['a13225],stuck at expression 11; expression changed was 2; expressions impacted were [12, 11, 2]], 
   /w/ [ Witness: {a=[1.0], b=[1.0]}, v['a13305],stuck at expression 15; expression changed was 1; expressions impacted were [12, 1, 15] ] *)
	
(* --- map Examples --- *)

val a' = THole(TypeHole(TypeVar("a")));
val b' = THole(TypeHole(TypeVar("b")));

val mapExpr = Value(Fun(Var("func"),TFun(a',b'),Value(Fun(Var("list"),TList(a'),
		      LetRec(Var("map"),TFun(TFun(a',b'),TFun(TList(a'),TList(b'))),
			  Value(Fun(Var("f"),TFun(a',b'),Value(Fun(Var("l"),TList(a'),
			  Case(Variable(Var("l")),
			  [(PVal(EmptyList),Value(Concrete(EmptyList))),
			   (PCons(PVar(Var("x")),PVar(Var("xs"))),
			   Cons(App(Variable(Var("f")),Variable(Var("x"))),
			        App(App(Variable(Var("map")),Variable(Var("f"))),Variable(Var("xs")))))]))))),
			  App(App(Variable(Var("map")),Variable(Var("func"))),Variable(Var("list"))))))));
			  
val var_types = [(Var("func"),TFun(Int,Bool)),
				 (Var("list"),TList(Int)),
				 (Var("map"),TFun(TFun(Int,Bool),TFun(TList(Int),TList(Bool)))),
				 (Var("f"),TFun(Int,Bool)),
				 (Var("l"),TList(Int)),
				 (Var("x"),Int),
				 (Var("xs"),TList(Int))];
(* fn func:('a->'b) => fn list:('a list) =>
   let val rec map : ('a->'b) -> 'a list -> 'b list = 
   fn f:('a->'b) => fn l:('a list) => case l of [] => [] | x::xs => (f x)::(map f xs)
   in map func list end *)   
		
val SOME(fuzzMapExpr) = fuzz(toCounterExpr(mapExpr),var_types);		
(* +t+ [fn func:{a:int} => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:bool => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:{a:int} = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end], 
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) + (list)) end], 
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (func)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) + (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (list)) (list)) end], 
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((func) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:{a:int} => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:bool => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end], 
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (func) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) = (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) + (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (func))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) + (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (list)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (x)) :: (((func) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) + (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((f) (func)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => [ ] | x::xs => ((list) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (l) of [ ] => {} | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end],
   +t+ [fn func:('a -> 'b) => fn list:'a list => let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (fn f:('a -> 'b) => fn l:'a list => case (func) of [ ] => [ ] | x::xs => ((f) (x)) :: (((map) (f)) (xs))) in (((map) (func)) (list)) end] *)
  
prettyPrintExpression(Expression(toCounterExpr(mapExpr)));
(* ( fn func:('a -> 'b) => ( fn list:'a list => ( let val rec map:(('a -> 'b) -> ('a list -> 'b list)) = (( fn f:('a -> 'b) => ( fn l:'a list => ( case (( l, [1] )) of [ ] => ( [ ], [2] ) | x::xs => ( (( (( f, [3] )) (( x, [4] )), [5] )) :: (( (( (( map, [6] )) (( f, [7] )), [8] )) (( xs, [9] )), [10] )), [11] ), [12] ), [13] ), [14] )) in (( (( (( map, [15] )) (( func, [16] )), [17] )) (( list, [18] )), [19] )) end, [20] ), [21] ), [22] ) *)

prettyPrintFindWitnessList(findWitnessList(fuzzMapExpr)); 	
(* /w/ [ Witness: {a=1}, [v['a]],stuck at expression 16; expression changed was 22; expressions impacted were [22, 16]], 
   /w/ [ Witness: fn x => v['b], true,stuck at expression 18; expression changed was 21; expressions impacted were [22, 21, 18]], 
   +w+ [ Witness: fn x => v['b], [v['a]],stuck at expression 20; expression changed was 20; expressions impacted were [21, 20]], 
   /w/ [ Witness: fn x => v['b], [v['a]],stuck at expression 18; expression changed was 19; expressions impacted were [20, 17, 18, 19]], 
   +w+ [ Witness: fn x => v['b], [v['a]],stuck at expression 18; expression changed was 18; expressions impacted were [17, 19, 18, 16]], 
   +w+ [ Witness: fn x => v['b], [v['a]],stuck at expression 17; expression changed was 17; expressions impacted were [18, 19, 15, 16, 17]],
   +w+ [ Witness: fn x => v['b], [v['a]],stuck at expression 16; expression changed was 16; expressions impacted were [15, 17, 16, 18]], 
   /w/ [ Witness: fn x => fn x:'a13807 list => v['a13793], [v['a13807]],stuck at expression 16; expression changed was 15; expressions impacted were [16, 17, 15, 16]], 
   /w/ [ Witness: fn x => v['b], [v['a]],stuck at expression 20; expression changed was 14; expressions impacted were [20, 14, 3, 7]], 
   |w| [ Witness: fn x => v['b], [v['a]],stuck at expression 20; expression changed was 13; expressions impacted were [14, 13, 1]],
   |w| [ Witness: fn x => v['b], [v['a]],stuck at expression 20; expression changed was 1; expressions impacted were [13, 12, 1, 16]], 
   +w+ [ Witness: fn x => v['b], [v['a14061]],stuck at expression 11; expression changed was 11; expressions impacted were [12, 2, 5, 10, 11]], 
   +w+ [ Witness: fn x => v['b], [v['a14213]],stuck at expression 10; expression changed was 10; expressions impacted were [5, 11, 8, 9, 10]],
   +w+ [ Witness: fn x => v['b], [v['a14395]],stuck at expression 9; expression changed was 9; expressions impacted were [8, 10, 9, 16]],
   +w+ [ Witness: fn x => v['b], [v['a14583]],stuck at expression 8; expression changed was 8; expressions impacted were [9, 10, 6, 7, 8]], 
   +w+ [ Witness: fn x => v['b], [v['a14759]],stuck at expression 7; expression changed was 7; expressions impacted were [6, 8, 7, 18]], 
   /w/ [ Witness: fn x => v['b], [v['a15003]],stuck at expression 7; expression changed was 6; expressions impacted were [7, 8, 6, 16]], 
   /w/ [ Witness: fn x => v['b], [v['a15155]],stuck at expression 3; expression changed was 5; expressions impacted were [10, 11, 3, 4, 5]], 
   +w+ [ Witness: fn x => v['b], [v['a15321]],stuck at expression 4; expression changed was 4; expressions impacted were [3, 5, 4, 16]], 
   +w+ [ Witness: fn x => v['b], [v['a15463]],stuck at expression 3; expression changed was 3; expressions impacted were [4, 5, 3, 18]], 
   /w/ [ Witness: fn x => v['b], [v['a15604]],stuck at expression 11; expression changed was 2; expressions impacted were [12, 11, 2]], 
   |w| [ Witness: fn x => v['b], [v['a]],stuck at expression 20; expression changed was 1; expressions impacted were [12, 1, 16] ] *)
		
(* --- Remove head example from paper --- *)

	val facExpr = Value(Fun(Var("x"),Int,
				  LetRec(Var("fac"),TFun(Int,Int),
				  Value(Fun(Var("n"),Int,
						Condition(BoolExpr(LESS_EQ,Variable(Var("n")),Value(Concrete(N(1)))),
								  Value(Concrete(N(1))),
								  ArithExpr(TIMES,Variable(Var("n")),
											App(Variable(Var("fac")),
												ArithExpr(SUBTRACT,Variable(Var("n")),Value(Concrete(N(1))))))))),
				   App(Variable(Var("fac")),Variable(Var("x"))))));

	(* fn x:int => let val rec fac:(int->int) = fn n:int => if n<=1 then 1 else n * fac (n-1) in fac x end *)			   
			
	val var_types = [(Var("x"),Int),
					 (Var("fac"),TFun(Int,Int)),
					 (Var("n"),Int)];
					 
	prettyPrintConfig(evaluate(Config(Expression(App(facExpr,Value(Concrete(N(6))))),[],[]),0));	
					 
	val SOME(fuzzFacExpr) = fuzz(toCounterExpr(facExpr),var_types);
	(* +t+ [fn x:real => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:{a:int} = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) + (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (fac)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((x) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:real => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) :: ((fac) ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) + ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) :: (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((n) - (2.0))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((fac) ((fac) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((n) * ((x) ((n) - (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (1) else ((fac) * ((fac) ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (1)) then  (2.0) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end],
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((n) <= (2.0)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end], 
	   +t+ [fn x:int => let val rec fac:(int -> int) = (fn n:int => if ((fac) <= (1)) then  (1) else ((n) * ((fac) ((n) - (1))))) in ((fac) (x)) end] *)

	prettyPrintExpression(Expression(toCounterExpr(facExpr)));
	(* ( fn x:int => ( let val rec fac:(int -> int) = (( fn n:int => ( if (( (( n, [1] )) <= (( 1, [2] )), [3] )) then  (( 1, [4] )) else (( (( n, [5] )) * (( (( fac, [6] )) (( (( n, [7] )) - (( 1, [8] )), [9] )), [10] )), [11] )), [12] ), [13] )) in (( (( fac, [14] )) (( x, [15] )), [16] )) end, [17] ), [18] ) *)
			
	prettyPrintFindWitnessList(findWitnessList(fuzzFacExpr)); 
	(* /w/ [ Witness: 1.0,stuck at expression 15; expression changed was 18; expressions impacted were [18, 15]],
	   +w+ [ Witness: 1,stuck at expression 17; expression changed was 17; expressions impacted were [18, 17]], 
	   /w/ [ Witness: 1,stuck at expression 14; expression changed was 16; expressions impacted were [17, 14, 15, 16]], 
	   +w+ [ Witness: 1,stuck at expression 15; expression changed was 15; expressions impacted were [14, 16, 15, 6, 14]], 
	   +w+ [ Witness: 1,stuck at expression 14; expression changed was 14; expressions impacted were [15, 16, 14, 15]], 
	   /w/ [ Witness: 1,stuck at expression 17; expression changed was 13; expressions impacted were [17, 13, 1, 5, 7]], 
	   |w| [ Witness: 1,stuck at expression 17; expression changed was 3; expressions impacted were [13, 12, 1, 2, 3]], 
	   +w+ [ Witness: 1,stuck at expression 11; expression changed was 11; expressions impacted were [4, 12, 5, 10, 11]], 
	   /w/ [ Witness: 1,stuck at expression 6; expression changed was 10; expressions impacted were [5, 11, 6, 9, 10]], 
	   +w+ [ Witness: 1,stuck at expression 9; expression changed was 9; expressions impacted were [6, 10, 7, 8, 9]], 
	   +w+ [ Witness: 1,stuck at expression 8; expression changed was 8; expressions impacted were [7, 9, 8]], 
	   +w+ [ Witness: 1,stuck at expression 7; expression changed was 7; expressions impacted were [8, 9, 7, 6, 14]], 
	   +w+ [ Witness: 1,stuck at expression 6; expression changed was 6; expressions impacted were [9, 10, 6, 15]], 
	   +w+ [ Witness: 1,stuck at expression 5; expression changed was 5; expressions impacted were [10, 11, 5, 6, 14]], 
	   +w+ [ Witness: 1,stuck at expression 4; expression changed was 4; expressions impacted were [11, 12, 4]], 
	   |w| [ Witness: 1,stuck at expression 17; expression changed was 3; expressions impacted were [12, 1, 2, 3]],
	   |w| [ Witness: 1,stuck at expression 17; expression changed was 2; expressions impacted were [1, 3, 2]], 
	   |w| [ Witness: 1,stuck at expression 17; expression changed was 1; expressions impacted were [2, 3, 1, 6, 14] ] *)

		
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)