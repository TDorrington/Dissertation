(* ------------------------------------------ GEN WITNESS TESTING ----------------------------------- *)

findWitness(Value(Fun(Var("x"),Int,ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("x"))))));
(* 1 *)
	
findWitness(Value(Fun(Var("x"),Int,Condition(Variable(Var("x")),Value(Concrete(N(1))),Value(Concrete(N(2)))))));
(* 1 *)

findWitness(Value(Fun(Var("x"),Real,Condition(Value(Concrete(B(true))),Value(Concrete(N(1))),Variable(Var("x"))))));
(* 1.0 *)
   
findWitness(Value(Fun(Var("x"),TRecord([(Lab("a"),THole(TypeHole(TypeVar("b")))),(Lab("b"),Real)]),
	Case(Variable(Var("x")),
  [(PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVar(Var("x")))]),
	List([Variable(Var("x")),Value(Concrete(N(1)))]))]))));
(* {a=1,b=1.0} *)
 
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PVal(N(1)),ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(N(1))))),
	    (PVal(N(2)),ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(R(2.0))))),
		(PWildcard,Value(Concrete(N(3))))]))));
(* 1 *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(Variable(Var("x")),
		ArithExpr(TIMES,Value(Concrete(N(1))),Value(Concrete(R(2.0))))))));
(* fn x:int => v['a13] *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("c"))),
	App(Variable(Var("x")),
		BoolExpr(EQ,Value(Concrete(R(1.0))),Value(Concrete(N(1))))))));
(* fn x:bool => v['a18] *)
		
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	Case(Variable(Var("x")),
	   [(PRecord([(Lab("1"),PVar(Var("f"))),(Lab("2"),PCons(PVar(Var("a")),PWildcard))]),
		 App(Variable(Var("f")),ArithExpr(TIMES,Variable(Var("a")),Value(Concrete(B(true))))))]))));
(* {1=fn x:'''a32 => v['22], 2=[v['''a32]]} *)
					  
findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(App(Variable(Var("x")),Value(Concrete(N(1)))),
	    Value(Fun(Var("y"),Bool,Cons(Variable(Var("y")),Value(VList([Concrete(N(2))])))))))));
(* fn x:int => fn x:(bool->bool list) => v['a37] *)

findWitness(Value(Fun(Var("x"),THole(TypeHole(TypeVar("b"))),
	App(App(Variable(Var("x")),Value(Concrete(N(1)))),
	    Value(Fun(Var("y"),Bool,Cons(Variable(Var("x")),Value(Concrete(EmptyList)))))))));
(* fn x:int => v['a44]: occurs check *)

(* Pretty printer *)
fun prettyPrintFuzzList(e) = (case fuzz(e) of 

	  NONE   => "FAIL"
	| SOME l => 
		
		let fun iterPrintList(l) = (case l of 
			
				  []          => ""
				| [(e,_)]     => "[" ^ prettyPrintExpression(Expression(e)) ^ "]"
				| (e,_)::rest => "[" ^ prettyPrintExpression(Expression(e)) ^ "], " ^ iterPrintList(rest))
				
		in iterPrintList(iterDropCounterExpr(l)) end);
		
(* 
	Write down expression
	Generate CounterExpression version
	Call fuzz() to generate a list of possible fuzzed versions of the expression
	Call iterFindWitness to generate a list of witness strings
	Call prettyPrintFindWitnessList to print those strings
*)

val c' = THole(TypeHole(TypeVar("c")));	

(* Function which takes a list and filters the elements of the list based on predicate = 10 *)
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
			Value(Fun(Var("x"),Int,BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(10))))))),
		Variable(Var("list"))))));
(* fn list:'b => let val rec filter : ('c->bool)->'c list -> 'c list = fn p:('c->bool) => fn l:'c list => 
						case l of [] -> [] | x::xs -> if (p x) then x::(filter p xs) else (filter p xs)
				 in filter (fn x:int => x=10) list *)
			
val SOME((e1,_)::fuzzFilterExpr) = fuzz(toCounterExpr(filterExpr));
(* [fn list:'b => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:{a:int} = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in (filter) (fn x:int => x = 10) + list end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (3) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in (filter + fn x:int => x = 10) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:real => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => (x) (10))) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 2.0)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => 3 = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((3) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:{a:int} => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:bool => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case 3 of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x = ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else (filter) (p) + xs) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (3)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else (filter + p) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (3)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((3) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x = ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: (filter) (p) + xs else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (3) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: (filter + p) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (3)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((3) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (x) then  3 :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end],
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if p + x then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (p) (3) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> [ ] | x::xs -> if (3) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case l of [ ] -> {} | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end], 
   [fn list:'b => let val rec filter:(('c -> bool) -> ('c list -> 'c list)) = (fn p:('c -> bool) => fn l:'c list => case 3 of [ ] -> [ ] | x::xs -> if (p) (x) then  x :: ((filter) (p)) (xs) else ((filter) (p)) (xs)) in ((filter) (fn x:int => x = 10)) (list) end] *)
   
findWitness(e1);
   
prettyPrintFindWitnessList(findWitnessList(fuzzFilterExpr));
(* [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: [1.0]], 
   [ Witness: [1]], 
   [ Witness: [1]], 
   [ Result :[ ], with witness: [1]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]],
   [ Witness: v['a]],
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Result :[ ], with witness: [1]], 
   [ Witness: v['a]], 
   [ Result :[ ], with witness: [1]], 
   [ Witness: v['a]], 
   [ Witness: v['a]], 
   [ Witness: v['a] ] *)

		
(* use "C:/Users/Tom/Documents/GitHub/Dissertation/include-all.sml"; *)