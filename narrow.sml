(* datatypes *) -------------------------------------------------------
	 
(* type variables datatype *)
datatype typeVar = Alpha1 | Alpha2 | Alpha3 | Alpha4 | Alpha5 | Alpha6;

(* type hole datatype *)
datatype typeHole = TypeHole of typeVar;
	
(* types datatype *)
datatype t =
	   Bool					(* boolean *)
	 | Int 					(* integer *)
	 | THole of typeHole;	(* type variable *)

(* value hole datatype *)
datatype valHole = ValueHole of typeVar;
	 
(* value datatype *) 
datatype v =
	   N of int					(* integer *)
  	 | B of bool				(* boolean *)
	 | VHole of valHole; 	(* unconstrained value replaceable by any value of type t *)
	 
(* variables datatype *)
datatype var = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 ;

(* non-stuck expression datatype *)
datatype e =
	  Value of v
	| Variable of var
	| Plus of e * e
	| LessThan of e * e
	| MoreThan of e * e
	| LessThanEqual of e * e
	| MoreThanEqual of e * e
	| Equal of e * e
	| Times of e * e
	| Subtract of e * e
	| Divide of e * e
	| Condition of e * e * e;

(* possibly stuck expression datatype *)
datatype expression = Stuck | Expression of e; 

(* substitution datatypes, theta and sigma *)
type valSub  = (valHole,  v) Substitution.map;
type typeSub = (typeHole, t) Substitution.map;

(* configuration datatype *)
type config = expression * valSub * typeSub;

(* typeof function ------------------------------------------------------- *)
(* typeof returns dynamic type of a value *)

fun typeof (v) = case v of
	  N(_) => Int
	| B(_) => Bool
	| VHole(ValueHole(a)) => THole(TypeHole(a));

(* gen function ---------------------------------------------------------- *)	
(* 'gen' takes as input a type t and returns value of that type.
   For base types, returns arbitrary value of that type
   gen : v * t -> v *)

fun gen (t, theta:typeSub) = case t of

	(* For base types, returns arbitrary value of that type *)	
	  Bool => B(true)
	| Int => N(1)

	(* For unconstrained types, yields fresh hole constrained to that type *)
	| THole(TypeHole(a)) => if Substitution.contains(TypeHole(a),theta) 
							then gen(Substitution.get(TypeHole(a),theta), theta)
							else VHole(ValueHole(a)); 
	
(* narrow function ------------------------------------------------------- *)
(* 'narrow' dynamically performs type-checking 
    narrow : v * t * sigma * theta -> <v union stuck, sigma, theta>
    takes a value v, type t and current values & type substitutions
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow (v, t, sigma:valSub, theta:typeSub) : config = case v of

	  N(integer) => Config(Expression(Value(N(integer))), sigma, theta)

	| B(boolean) => Config(Expression(Value(B(boolean))), sigma, theta)

	(* When v a hole, check in given sigma first if hole already instantiated
	   and if so, return existing instantiation *)
	| VHole(ValueHole(a)) =>
	
		if Substitution.contains(ValueHole(a), sigma) then
			let val v = Substitution.get(ValueHole(a), sigma); 
			    val (theta', success) = unify( [THole(TypeHole(a)), t, typeof(v)], theta) in
				if(success) then Config(Expression(Value(v)),sigma,theta')
					        else Config(Stuck, sigma, theta')	
			end
		else
			let val (theta', success) = unify( [THole(TypeHole(a)), t], theta);
			    val v = gen(t, theta') in
				if(success) 
					then Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(a),v), theta')
					else Config(Stuck, sigma, theta)
				end

	(* Any other case, evaluate to stuck term *)
	| _ => Config(Stuck, sigma, theta)
	
