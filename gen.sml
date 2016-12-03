(* gen dynamically performs hole-filling 	
   gen takes as input a type t and returns value of that type *)

fun gen (t, theta:typeSub) = case t of

	(* For base types, returns arbitrary value of that type 
	   Currently not arbitrary, but fixed so deterministic *)
	  Bool => B(true)
	| Int => N(1)
    | Real => R(1.0)
	
	| Fun(t1,t2) => Func(Var("x"^Int.toString(getCounterAndUpdate())),t1,Value(gen(t2,theta)))
	
	(* For pairs, recursively call gen to get values for components *)
	| Pair(t1,t2) => ValuePair(gen(t1,theta),gen(t2,theta))

	(* For unconstrained types, yields fresh hole constrained to that type *)
	| THole(TypeHole(a)) => if Substitution.contains(TypeHole(a),theta) 
							then gen(resolveChainTheta(THole(TypeHole(a)),theta), theta)
							else VHole(SimpleHole(ValueHole(a))); 
								