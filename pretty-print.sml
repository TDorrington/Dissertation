(* Gets the string attached to type variable datatype
   We use the notation that 'a = TypeVar("a"), ''b = EqualityTypeVar("b"), and 
   '''c = ArithmeticTypeVar("c") *)
fun getTypeVariableString(TypeVar(s)) = "'" ^ s
|  	getTypeVariableString(EqualityTypeVar(s)) = "''" ^ s
|	getTypeVariableString(ArithTypeVar(s)) = "'''" ^ s;

(* Returns string representation of operation as infix for boolean and arithmetic operators
   in the binaryOper datatype (no clause for EXPR_PAIR)
   with formatting, i.e. space either side of operator *)
fun getOperString(ArithOper(oper)) = (case oper of 
	  PLUS => " + "
	| SUBTRACT => " - "
	| TIMES => " * "
	| DIVIDE => " / ")

|	getOperString(BoolOper(oper)) = (case oper of
	  LESS => " < "
	| MORE => " > "
	| LESS_EQ => " <= "
	| MORE_EQ => " >= "
	| EQ => " = ")
	
|	getOperString(_) = " ";

(* Get string representing the type *)
fun prettyPrintType (t) =
	case t of Int  => "int"
			| Real => "real"
			| Bool => "bool"
			| Fun(t1,t2) => "(" ^ prettyPrintType(t1) ^ " -> " ^ prettyPrintType(t2) ^ ")"
			| Pair(t1,t2) => "(" ^ prettyPrintType(t1) ^ " * " ^ prettyPrintType(t2) ^ ")"
			| THole(TypeHole(t')) => getTypeVariableString(t');

fun prettyPrintPattern(VariablePair(Var(x1),Var(x2))) = "( " ^ x1 ^ " , " ^ x2 ^ " )"; 
		
(* Gets the string representation of a value *)
fun prettyPrintValue (v) =
	(case v of N(n) => Int.toString(n)
		     | R(r) => Real.toString(r)
			 | B(b) => Bool.toString(b)
			 | Func(Var(s),t,e) => "fn " ^ s ^ " : " ^ prettyPrintType(t) ^ " => " ^ prettyPrintExpression(Expression(e))
			 | ValuePair(v1,v2) => "(" ^ prettyPrintValue(v1) ^ "," ^ prettyPrintValue(v2) ^ ")"
			 | VHole(SimpleHole(ValueHole(t))) => "v[" ^ getTypeVariableString(t) ^ "]"
			 | VHole(BinaryOp(EXPR_PAIR,v1,v2)) =>
				"v[ (" ^ prettyPrintValue(v1) ^ " , " 
					   ^ prettyPrintValue(v2) ^ " ]"
			 | VHole(BinaryOp(oper,v1,v2)) =>
				"v[ " ^ prettyPrintValue(v1) ^ getOperString(oper)
				      ^ prettyPrintValue(v2) ^ " ]"
			 | VHole(ConditionHole(v,e1,e2)) =>
				"v[ if " ^ prettyPrintValue(v) ^ " then " ^
					prettyPrintExpression(Expression(e1)) ^ " else " ^ prettyPrintExpression(Expression(e2)) ^ " ]"
			 | VHole(CaseHole(v,pat,e)) =>
				"v[ case " ^ prettyPrintValue(v) ^ " of " ^
					prettyPrintPattern(pat) ^ " -> " ^ prettyPrintExpression(Expression(e)) ^ " ]"
			| VHole(AppHole(v1,v2)) =>
				"v[ " ^ prettyPrintValue(v1) ^ " " ^ prettyPrintValue(v2) ^ " ]")
		
(* Pretty prints a possibly stuck expression *)
and prettyPrintExpression(Stuck) = "Stuck"

| 	prettyPrintExpression(Expression(e)) = (case e of 
	
	  Value(v) => prettyPrintValue(v)
	| Variable(Var(s)) => s
	| ArithExpr(oper,e1,e2) => prettyPrintExpression(Expression(e1)) ^ getOperString(ArithOper(oper)) ^ 
							   prettyPrintExpression(Expression(e2))
	| BoolExpr(oper,e1,e2) =>  prettyPrintExpression(Expression(e1)) ^ getOperString(BoolOper(oper))  ^ 
							   prettyPrintExpression(Expression(e2))
	| ExpressionPair(e1,e2) => "( " ^ prettyPrintExpression(Expression(e1)) ^ " , " ^ prettyPrintExpression(Expression(e2)) ^ " )"
	| Case(e1,pat,e3) => 
		"case " ^ prettyPrintExpression(Expression(e1)) ^ " of " ^ prettyPrintPattern(pat) ^
		" -> " ^ prettyPrintExpression(Expression(e3))
	| App(e1,e2) => "( " ^ prettyPrintExpression(Expression(e1)) ^ " ) ( " ^ prettyPrintExpression(Expression(e2)) ^ " )"
	| Condition(e1,e2,e3) => 
		"if " ^ prettyPrintExpression(Expression(e1)) ^ " then  " ^ prettyPrintExpression(Expression(e2)) ^ " else " ^
		prettyPrintExpression(Expression(e3)));
	
	
(* Pretty print for (value hole -> value) substitution, sigma 
   The output string is not surrounded by [ ... ] - this is done in top level pretty print
   function to avoid declaring a recursive function within this one
   Simply returns comma separated list of form v[alpha] -> v *)
fun prettyPrintSigma ([]) = ""

|   prettyPrintSigma ([(ValueHole(a),b)]) = 
	(* Handles last element so we don't add a comma at end of list *)
		prettyPrintValue(VHole(SimpleHole(ValueHole(a)))) ^ " -> " ^ prettyPrintValue(b)
		
|	prettyPrintSigma ((ValueHole(a),b)::l) =
		prettyPrintValue(VHole(SimpleHole(ValueHole(a)))) ^ " -> " ^ prettyPrintValue(b) ^
		", " ^ prettyPrintSigma(l);	

(* Pretty print for (type hole -> type) substitution, theta *)
fun prettyPrintTheta([]) = ""

| 	prettyPrintTheta([(TypeHole(a),b)]) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b)
		
|	prettyPrintTheta((TypeHole(a),b)::l) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b) ^
		", " ^ prettyPrintTheta(l);
		
(* Top level pretty print to print out a configuration
   in a user-readable format *)
fun prettyPrintConfig(Config(e,sigma,theta)) =
		"\n\nFinal expression = " ^ prettyPrintExpression(e) ^ "\n" ^ 
		"Final value substitution = [" ^ prettyPrintSigma(sigma) ^ "]\n" ^ 
		"Final type substitution = [" ^ prettyPrintTheta(theta) ^ "]";