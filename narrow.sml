(* datatypes *) -------------------------------------------------------

(* value datatype *) 
datatype 'a v =
	   N of int		(* integer *)
  	 | B of bool		(* boolean *)
	 | Hole of 'a; 		(* unconstrainted value replacable by any value of type 'a *)

(* types datatype *)
datatype 'a t =
	   Bool			(* boolean *)
	 | Int 			(* integer *)
	 | Typevar of 'a;	(* type variable *)

(* variables datatype *)
datatype var = X | Y | Z;

(* non-stuck expression datatype *)
datatype 'a e =
	  Value of 'a v
	| Variable of var
	| Plus of ('a e) * ('a e)
	| LessThan of ('a e) * ('a e)
	| MoreThan of ('a e) * ('a e)
	| LessThanEqual of ('a e) * ('a e)
	| MoreThanEqual of ('a e) * ('a e)
	| Times of ('a e) * ('a e)
	| Subtract of ('a e) * ('a e)
	| Divide of ('a e) * ('a e)
	| Condition of ('a e) * ('a e) * ('a e);

(* possibly stuck expression datatype *)
datatype 'a expression = Stuck | 'a e; 

(* narrow and generate functions *) -------------------------------------------------------

(* 'narrow' dynamically performs type-checking 
    narrow : v * t * sigma * theta -> <vUstuck, sigma, theta>
    takes a value v, type t and current values & type substitutions
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	

narrow(v, t, sigma, theta) = case v of

	  N(integer) => (integer, sigma, theta)

	| B(boolean) => (boolean, sigma, theta)

	(* When v a hole, check in given sigma first if hole already instantiated
	   and if so, return existing instantiation *)
	| Hole(a) =>
		if inDomain(Hole(a), sigma) then
			let val v = sigma(Hole(a)); 
			    val (theta, success) = unify( [a, t, typeof(v)], theta) in
				if(success) then (v,sigma,theta')
					    else (stuck, theta, sigma)	
			end
		else
			let val (theta', success) = unify( [a, t], theta);
			    val v = gen(t, theta') in	
				(v, unionSubstitution(sigma,Hole(a),v), theta')

				end

	(* Any other case, evalaute to stuck term *)
	| _ => (Stuck, sigma, theta)
		
(* 'gen' takes as input a type t and returns value of that type.
   For base types, returns arbitrary value of that type
   gen : v * t -> v *)

gen(t, theta) = case t of

	(* For base types, returns arbitrary value of that type *)	

	  Bool = true 
 
	| Int = 1

	(* For unconstrainted types, yields fresh hole constrainted to that type *)

	| Typevar(a) = if inDomain(a,theta) then gen(getSubstituion(a),theta)
			  		    else Hole(a); 

	

