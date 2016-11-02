(* type variables datatype 
   EqualityTypeVar and ArithTypeVar both subsets of TypeVar 
   Intersection of EqualityTypeVar and ArithTypeVar is only type Int 
   Notation: 'a = TypeVar("a"), ''b = EqualityTypeVar("b"), and '''c = ArithTypeVar("c") *)
datatype typeVar = 
				(* Type Variables *)
				  TypeVar of string			

				 (* Equality Type Variables: int, bool, string, char, and records/lists containing any of these types
					As well as datatype with certain restrictions on constructor parameters *)
				 | EqualityTypeVar of string	
				 
				 (* New Arithmetic Type Variable: int or real *)
				 | ArithTypeVar of string;  

(* Integer that is used to generate fresh type variables
   Type variables will also be "a" with the current global counter appended onto it *)
val globalCounter = ref 0;

(* Function to get the latest global counter, and increment at the same time *)
fun getCounterAndUpdate() = 
	let val current = !globalCounter
	in (globalCounter := !globalCounter + 1);current end;
				 
(* type hole datatype *)
datatype typeHole = TypeHole of typeVar;
	
(* types datatype *)
datatype t =
	   Bool					(* boolean *)
	 | Int 					(* integer *)
     | Real					(* real    *)
	 | Pair of t * t		(* pairs   *)
	 | THole of typeHole;	(* type variable *)

(* value hole datatype *)
datatype valHole = ValueHole of typeVar;
	 
(* value datatype *) 
datatype v =
	   N of int					(* integer  *)
  	 | B of bool				(* boolean  *)
     | R of real			    (* real     *)
	 | ValuePair of v * v		(* pairs 	*)
	 | VHole of valHole; 		(* unconstrained value replaceable by any value of type t *)
	 
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
	| Variable of var
	| Case of e * e * e; 
	
(* possibly stuck expression datatype *)
datatype expression = Stuck | Expression of e; 

(* substitution datatypes *)
type valSub  = (valHole,  v) Substitution.map; (* sigma *)
type typeSub = (typeHole, t) Substitution.map; (* theta *)

(* configuration datatype *)
datatype config = Config of expression * valSub * typeSub;