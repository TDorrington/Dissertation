(* Type variables datatype 
   EqualityTypeVar and ArithTypeVar are both subsets of TypeVar 
   Intersection of EqualityTypeVar and ArithTypeVar is only type Int 
   Notation: 'a = TypeVar("a"), ''b = EqualityTypeVar("b"), and '''c = ArithTypeVar("c") *)
datatype typeVar = 
				(* Type Variables *)
				  TypeVar of string			

				 (* Equality Type Variables: int, bool, string and pairs & lists of these *)
				 | EqualityTypeVar of string	
				 
				 (* New Arithmetic Type Variable: int or real and pairs & lists of these *)
				 | ArithTypeVar of string;  
				 
(* Type hole datatype *)
datatype typeHole = TypeHole of typeVar;	

(* Types datatype *)
datatype t =
	   Bool					   	    (* boolean   *)
	 | Int 					   	    (* integer   *)
     | Real					   	    (* real      *)
	 | Pair of t * t		   	    (* pairs     *)
	 | THole of typeHole;	        (* type variable *)

(* Value hole datatype *)
datatype valHole = ValueHole of typeVar
	 
(* Value datatype *) 
datatype v =
	   N of int					(* integer   *)
  	 | B of bool				(* boolean   *)
     | R of real			    (* real      *)
	 | ValuePair of v * v		(* pairs 	 *)
	 | VHole of valHole; 		(* unconstrained value replaceable by any value of type t *)
	 
(* Integer that is used to generate fresh type variables in the evaluate method
   as well as fresh variables in alpha-invariant method
   Fresh type variables will be the string "a" with the global counter appended onto it
   and fresh variables will be the variable name appended with counter *)
val globalCounter = ref 0;

(* Function to get the latest global counter, and increment it at the same time 
   to ensure the same value is never returned *)
fun getCounterAndUpdate() = 
	let val current = !globalCounter
	in (globalCounter := !globalCounter + 1);current end;
	
(* variable datatype *)
datatype var = Var of string;	 

(* non-stuck expression datatype *)
(* +,-,* are of type (int * int -> int) OR (real * real -> real)
   / is of type (real * real -> real)
   <,<=,>,>= are of type (int * int -> bool) OR (real * real -> bool) 
   = is of type (int *int -> bool) *)
datatype e =
	  Value of v
	| Plus of e * e	
	| Times of e * e
	| Subtract of e * e
    | Divide of e * e 	
	| LessThan of e * e
	| MoreThan of e * e
	| LessThanEqual of e * e
	| MoreThanEqual of e * e
	| Equal of e * e
	| Condition of e * e * e
	| ExpressionPair of e * e
	| Case of e * e * e
	| Variable of var;
	
(* possibly stuck expression datatype *)
datatype expression = Stuck | Expression of e; 

(* substitution datatypes *)
type valSub  = (valHole,  v) Substitution.map; (* sigma: value holes -> values *)
type typeSub = (typeHole, t) Substitution.map; (* theta: type holes -> types *)
type variableSub = (var, e)  Substitution.map; (* gamma: variables -> values *)

(* configuration datatype *)
datatype config = Config of expression * valSub * typeSub;