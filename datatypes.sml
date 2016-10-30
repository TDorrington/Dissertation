(* type variables datatype *)
datatype typeVar = Alpha1 | Alpha2 | Alpha3 | Alpha4 | Alpha5 | Alpha6;

(* type hole datatype *)
datatype typeHole = TypeHole of typeVar;
	
(* types datatype *)
datatype t =
	   Bool					(* boolean *)
	 | Int 					(* integer *)
	 | Real					(* real    *)
	 | THole of typeHole;	(* type variable *)

(* value hole datatype *)
datatype valHole = ValueHole of typeVar;
	 
(* value datatype *) 
datatype v =
	   N of int					(* integer *)
  	 | B of bool				(* boolean *)
	 | R of real				(* real    *)
	 | VHole of valHole; 		(* unconstrained value replaceable by any value of type t *)
	 
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
datatype config = Config of expression * valSub * typeSub;