(* gen dynamically performs hole-filling 	
   gen takes as input a type t and returns value of that type.
   For base types, returns arbitrary value of that type
   gen : v * t -> v *)

fun gen (t, theta:typeSub) = case t of

	(* For base types, returns arbitrary value of that type *)	
	  Bool => B(true)
	| Int => N(1)
    | Real => R(1.0)

	(* For unconstrained types, yields fresh hole constrained to that type *)
	| THole(TypeHole(a)) => if Substitution.contains(TypeHole(a),theta) 
								then gen(Substitution.get(TypeHole(a),theta), theta)
								else VHole(ValueHole(a)); (* v[a] for v fresh *)
								
(* *** work: v fresh?? *** *)