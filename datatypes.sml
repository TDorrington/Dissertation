(* Type variables datatype 
   EqualityTypeVar and ArithTypeVar are both subsets of TypeVar 
   Intersection of EqualityTypeVar and ArithTypeVar is only type Int 
   Notation: 'a = TypeVar("a"), ''b = EqualityTypeVar("b"), and '''c = ArithTypeVar("c") *)
datatype typeVar = 
				(* Type Variables *)
				  TypeVar of string			

				 (* Equality Type Variables: int, bool, and lists & records over these *)
				 | EqualityTypeVar of string	
				 
				 (* New Arithmetic Type Variable: int or real, NOT lists or records over these *)
				 | ArithTypeVar of string;  
			
(* Enumeration associated with type variables *)
datatype typeVarEnum = TYPE_VAR | EQUALITY_TYPE_VAR | ARITH_TYPE_VAR;
	
(* Type hole datatype *)
datatype typeHole = TypeHole of typeVar;	

(* Variable datatype *)
datatype var = Var of string;

(* thrown if we try to evaluate OR calculate type of OR narrow a free variable
   i.e. not substituted for in its containing expression *)
exception FreeVariable;	

(* Types datatype *)
datatype t =
	  Bool					   	     (* boolean    	  *)
	| Int 					   	     (* integer   	  *)
    | Real					   	     (* real      	  *)
	| TFun of t * t					 (* function  	  *)
	| TRecord of t Record.dictionary (* record 		  *)
	| THole of typeHole      	     (* type variable *)
	| TList of t;					 (* list          *)
	
(* datatype for operations *) 
datatype arithOper = PLUS | SUBTRACT | TIMES | DIVIDE;
datatype boolOper  = LESS | MORE | LESS_EQ | MORE_EQ | EQ;
datatype binaryOper = ArithOper of arithOper 
				    | BoolOper  of boolOper;

(* datatype of a simple value hole, v['a] *)
datatype simpleValueHole = ValueHole of typeVar;
	  
(* non-stuck expression datatype *)
datatype e =
	  Value of v
	| Variable of var 
	| ArithExpr of arithOper * e * e     
	| BoolExpr of boolOper * e * e 
	| Case of e * (pat * e) list		
	| Condition of e * e * e
	| App of e * e
	| Record of e Record.dictionary	
	| Let of var * t * e * e
	| LetRec of var * t * v * e
	| List of e list

(* value hole datatype *)
and valhole = 
	  SimpleHole of simpleValueHole
	| BinaryOpHole of binaryOper * v * v
	| ConditionHole of v * e * e
	| CaseHole of v * (pat * e) list
	| AppHole of v * v
	| RecordHole of v Record.dictionary    
	| ListHole of v list
	  
(* concrete values datatype: integers, reals and booleans, or tuples over these *)
and concretev =
	  N of int
	| B of bool
	| R of real
	| EmptyList
	
(* value datatype *)
and v =
	  Concrete of concretev
	| Fun of var * t * e 
	| VHole of valhole
	| VRecord of v Record.dictionary
	| VList of v list
	
(* pattern datatype *)
and pat = 
	  PWildcard							(* _ wildcard        *)
	| PVar of var						(* variable          *)
	| PVal of concretev					(* concrete value    *)
	| PRecord of pat Record.dictionary	(* record pattern    *)
	| PCons of pat * pat;				(* cons pattern x::l *)
		
(* possibly stuck expression datatype *)
datatype expression = Stuck | Expression of e; 

(* substitution datatypes *)
type valSub  = (simpleValueHole,  v) Substitution.map; (* sigma: simple value holes -> values *)
type typeSub = (typeHole, t) Substitution.map; 		   (* theta: type holes -> types 		  *)
type variableSub = (var, e)  Substitution.map; 		   (* gamma: variables -> expressions 	  *)

(* configuration datatype *)
datatype config = Config of expression * valSub * typeSub;

(* Integer that is used to generate fresh type variables or fresh variables
   Fresh type variables will be the string "a" with the global counter appended onto it
   and fresh variables will be the variable name appended with counter *)
val globalCounter = ref 0;

(* Function to get the latest global counter, and increment it at the same time 
   to ensure the same value is never returned *)
fun getCounterAndUpdate() = 
	let val current = !globalCounter
	in (globalCounter := !globalCounter + 1);current end;