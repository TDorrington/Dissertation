(* datatypes *) -------------------------------------------------------

(* value datatype *) 
datatype 'a v =
	   N of int		(* integer *)
  	 | B of bool		(* boolean *)
	 | Hole of 'a; 		(* unconstrained value replaceable by any value of type 'a *)

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
	| Equal of ('a e) * ('a e)
	| Times of ('a e) * ('a e)
	| Subtract of ('a e) * ('a e)
	| Divide of ('a e) * ('a e)
	| Condition of ('a e) * ('a e) * ('a e);

(* possibly stuck expression datatype *)
datatype 'a expression = Stuck | Expression of 'a e; 

(* substitution types *)
type (''a, 'b) valueSubstitution = Substitution.Map(

(* configuration datatype *)
datatype 'a config = Config of 'a expression * 

(* typeof function ------------------------------------------------------- *)
(* typeof returns dynamic type of a value *)

fun typeof (v) = case v of
	  N(_) => Int
	| B(_) => Bool
	| Hole(a) => a;

(* narrow function ------------------------------------------------------- *)
(* 'narrow' dynamically performs type-checking 
    narrow : v * t * sigma * theta -> <v union stuck, sigma, theta>
    takes a value v, type t and current values & type substitutions
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	

fun narrow (v, t, sigma, theta) = case v of

	  N(integer) => (integer, sigma, theta)

	| B(boolean) => (boolean, sigma, theta)

	(* When v a hole, check in given sigma first if hole already instantiated
	   and if so, return existing instantiation *)
	| Hole(a) =>
		if Substitution.contains(Hole(a), sigma) then
			let val v = Substitution.get(Hole(a), sigma); 
			    val (theta', success) = unify( [a, t, typeof(v)], theta) in
				if(success) then (Expression(v),sigma,theta')
					        else (Stuck, theta, sigma)	
			end
		else
			let val (theta', success) = unify( [a, t], theta);
			    val v = gen(t, theta') in
				if(success) 
					then (Expression(v), Substitution.union(sigma,Hole(a),v), theta')
					else (Stuck, theta, sigma)
				end

	(* Any other case, evaluate to stuck term *)
	| _ => (Stuck, sigma, theta)
	
(* gen function ---------------------------------------------------------- *)	
(* 'gen' takes as input a type t and returns value of that type.
   For base types, returns arbitrary value of that type
   gen : v * t -> v *)

fun gen (t, theta) = case t of

	(* For base types, returns arbitrary value of that type *)	
	  Bool => true
	| Int => 1

	(* For unconstrainted types, yields fresh hole constrainted to that type *)
	| Typevar(a) => if Substitution.contains(a,theta) 
						then gen(Substitution.get(a,theta), theta)
			  		    else Hole(a); 

	

