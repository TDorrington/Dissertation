(* Gets the string attached to type variable datatype
   We use the notation that 'a = TypeVar("a"), ''b = EqualityTypeVar("b"), and 
   '''c = ArithmeticTypeVar("c") *)
fun getTypeVariableString(TypeVar(s)) = "'" ^ s
|  	getTypeVariableString(EqualityTypeVar(s)) = "''" ^ s
|	getTypeVariableString(ArithTypeVar(s)) = "'''" ^ s;

(* Gets the string associated with a value
   Very simple for primitive values (e.g. int, real, bool), and 
   pairs is done on recursive application
   but for value hole, we wrap into v[alpha], for unconstrained value of type alpha *)
fun prettyPrintValue (v) =
	case v of N(n) => Int.toString(n)
		    | R(r) => Real.toString(r)
			| B(b) => Bool.toString(b)
			| ValuePair(v1,v2)    => "(" ^ prettyPrintValue(v1) ^ "," ^ prettyPrintValue(v2) ^ ")"
			| VHole(ValueHole(t)) => "v[" ^ getTypeVariableString(t) ^ "]";

(* Get string representing the type
   Very similar to pretty printing values *)
fun prettyPrintType (t) =
	case t of Int  => "int"
			| Real => "real"
			| Bool => "bool"
			| Pair(t1,t2) => "(" ^ prettyPrintType(t1) ^ " * " ^ prettyPrintType(t2) ^ ")"
			| THole(TypeHole(t')) => getTypeVariableString(t');

(* Pretty print for (value hole -> value) substitution, sigma 
   The output string is not surrounded by [ ... ] - this is done in top level pretty print
   function to avoid declaring a recursive function within this one
   Simply returns comma separated list of form v[alpha] -> v *)
fun prettyPrintSigma ([]) = ""

|   prettyPrintSigma ([(ValueHole(a),b)]) = 
	(* Handles last element so we don't add a comma at end of list *)
		prettyPrintValue(VHole(ValueHole(a))) ^ " -> " ^ prettyPrintValue(b)
		
|	prettyPrintSigma ((ValueHole(a),b)::l) =
		prettyPrintValue(VHole(ValueHole(a))) ^ " -> " ^ prettyPrintValue(b) ^
		", " ^ prettyPrintSigma(l);

(* Pretty print for (type hole -> type) substitution, theta
   Declared very similarly to sigma pretty print *)
fun prettyPrintTheta([]) = ""

| 	prettyPrintTheta([(TypeHole(a),b)]) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b)
		
|	prettyPrintTheta((TypeHole(a),b)::l) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b) ^
		", " ^ prettyPrintTheta(l);
		
(* Pretty prints a possibly stuck expression *)
fun prettyPrintExpression(Stuck) = "Stuck"
| 	prettyPrintExpression(Expression(e)) = case e of 
	
	  Value(v) => prettyPrintValue(v)
	| Plus(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " + " ^ prettyPrintExpression(Expression(e2))
	| Times(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " * " ^ prettyPrintExpression(Expression(e2))
	| Subtract(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " - " ^ prettyPrintExpression(Expression(e2))
    | Divide(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " / " ^ prettyPrintExpression(Expression(e2))
	| LessThan(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " < " ^ prettyPrintExpression(Expression(e2))
	| MoreThan(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " > " ^ prettyPrintExpression(Expression(e2))
	| LessThanEqual(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " <= " ^ prettyPrintExpression(Expression(e2))
	| MoreThanEqual(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " >= " ^ prettyPrintExpression(Expression(e2))
	| Equal(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " = " ^ prettyPrintExpression(Expression(e2))
	| Condition(e1,e2,e3) => 
		"if " ^ prettyPrintExpression(Expression(e1)) ^ " then  " ^ prettyPrintExpression(Expression(e2)) ^ " else " ^
		prettyPrintExpression(Expression(e3))
	| ExpressionPair(e1,e2) => "(" ^ prettyPrintExpression(Expression(e1)) ^ "," ^ prettyPrintExpression(Expression(e2)) ^ ")"
	| Case(e1,e2,e3) => 
		"case " ^ prettyPrintExpression(Expression(e1)) ^ " of " ^ prettyPrintExpression(Expression(e2)) ^
		" -> " ^ prettyPrintExpression(Expression(e3))
	| Variable(Var(s)) => s;

		
(* Top level pretty print to print out a configuration
   in a user-readable format *)
fun prettyPrintConfig(Config(e,sigma,theta)) =
		"\n\nFinal expression = " ^ prettyPrintExpression(e) ^ "\n" ^ 
		"Final value substitution = [" ^ prettyPrintSigma(sigma) ^ "]\n" ^ 
		"Final type substitution = [" ^ prettyPrintTheta(theta) ^ "]"