(* values datatype *)

datatype 'a values = N of int	(* integer *)
  	 | B of bool		(* boolean *)
	 | Hole of 'a; 		(* unconstrainted value replacable by any value of type 'a *)

(* types datatype *)

datatype 'a type = Bool 	(* boolean *)
	 | Int 			(* integer *)
	 | Typevar of 'a;	(* type variable *)

(* value substitutions sigma: (values holes, values) list *)
(* type substitutions theta:  (type holes, types) list *)

(* 'narrow' dynamically performs type-checking 
    narrow : v * t * sigma * theta -> <vUstuck, sigma, theta>
    takes a value v, type t and current values & type substitutions
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	

narrow(v, t, sigma, theta) = case v of

	  N(integer) = (integer, sigma, theta)

	| B(boolean) = (boolean, sigma, theta)

	| Hole(a) = 
		if inDomain(Hole(a), sigma) then
			let val v = sigma(Hole(a)); 
			    val (theta, success) = unify( [a, t, typeof(v)], theta) in
				if(success) then (v,sigma,theta')
					    else (stuck, theta, sigma)	
			end
		else
			let val (theta', success) = unify( [a, t], theta);
			    val v = gen(t, theta') in	
				(v, unionSubstituion(sigma,Hole(a),v), theta')
			end;
		
(* 'gen' takes as input a type t and returns value of that type.
   For base types, returns arbitrary value of that type
   gen : v * t -> v *)

gen(v, t) = case v of
	
	  n(integer) =
 
	| b(boolean) =

	| hole(a) = if inDomain(a, 


	

