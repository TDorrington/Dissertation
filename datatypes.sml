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
			
datatype typeVarEnum = TYPE_VAR | EQUALITY_TYPE_VAR | ARITH_TYPE_VAR;
	
(* Type hole datatype *)
datatype typeHole = TypeHole of typeVar;	

(* Types datatype *)
datatype t =
	   Bool					   	    (* boolean   *)
	 | Int 					   	    (* integer   *)
     | Real					   	    (* real      *)
	 | Pair of t * t		   	    (* pairs     *)
	 | THole of typeHole;	        (* type variable *)

(* Integer that is used to generate fresh type variables or fresh variables
   Fresh type variables will be the string "a" with the global counter appended onto it
   and fresh variables will be the variable name appended with counter *)
val globalCounter = ref 0;

(* Function to get the latest global counter, and increment it at the same time 
   to ensure the same value is never returned *)
fun getCounterAndUpdate() = 
	let val current = !globalCounter
	in (globalCounter := !globalCounter + 1);current end;


(* datatype for operations *) 
datatype arithOper = PLUS | SUBTRACT | TIMES | DIVIDE;
datatype boolOper  = LESS | MORE | LESS_EQ | MORE_EQ | EQ;
datatype binaryOper = ArithOper of arithOper 
				    | BoolOper  of boolOper 
				    | EXPR_PAIR;
datatype ternaryOper = CASE | CONDITION;

(* variable datatype *)
datatype var = Var of string;	 

(* pattern datatype 
   Currently only allow variable pair, used in case-pair expressions,
   i.e. case e of (x,x) -> e *)
datatype pattern = VariablePair of var * var;

(* datatype of a basic value hole, v['a] *)
datatype simpleValueHole = ValueHole of typeVar;
	  
(* non-stuck expression datatype *)
(* +,-,* are of type (int * int -> int) OR (real * real -> real)
   / is of type (real * real -> real)
   <,<=,>,>= are of type (int * int -> bool) OR (real * real -> bool) 
   = is of type (int *int -> bool) *)
datatype e =
	  Value of v
	| Variable of var
	| ArithExpr of arithOper * e * e
	| BoolExpr of boolOper * e * e
	| ExpressionPair of e * e
	| Case of e * pattern * e
	| Condition of e * e * e
and valHole = 
	  SimpleHole of simpleValueHole
	| BinaryOp of binaryOper * valHole * valHole
	| ConditionHole of valHole * e * e
	| CaseHole of valHole * pattern * e
and v =
	   N of int				
  	 | B of bool			
     | R of real			  
	 | ValuePair of v * v		
	 | VHole of valHole; 	
	
(* possibly stuck expression datatype *)
datatype expression = Stuck | Expression of e; 

(* substitution datatypes *)
type valSub  = (simpleValueHole,  v) Substitution.map; (* sigma: simple value holes -> values *)
type typeSub = (typeHole, t) Substitution.map; 		   (* theta: type holes -> types 		  *)
type variableSub = (var, e)  Substitution.map; 		   (* gamma: variables -> expressions 	  *)

(* configuration datatype *)
datatype config = Config of expression * valSub * typeSub;