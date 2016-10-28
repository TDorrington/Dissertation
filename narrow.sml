(* values datatype *)

datatype 'a values = n of int	(* integer *)
  	 | b of bool		(* boolean *)
	 | hole of 'a; 		(* unconstrainted value replacable by any value of type 'a *)
	

(*	'narrow' dynamically performs type-checking 
	narrow : v * t * sigma * theta -> <vUstuck, sigma, theta>
	takes a value v, type t and current values & type substitutions
	refines v to have type t by yielding triple of either same value
	and substitutions, or yields stuck state if not possible		

narrow(v, t, sigma, theta) = case v of

	  n(integer) = (integer, sigma, theta)

	| b(boolean) = (boolean, sigma, theta)

	| hole(a) = 
		if inDomain(hole(a), sigma) then
			let val v = sigma(hole(a)) in 
				let val (theta, success) = unify( [a, t, typeof(v)], theta) in
					if(success) then (v,sigma,theta')
						    else (stuck, theta, sigma)	
				end
			end
		else
			let val (theta', success) = unify( [a, t], theta);
			    val v = gen(t, theta') in	
				(v, unionSigma(sigma,hole(a),v), theta')
			end;
		



	

