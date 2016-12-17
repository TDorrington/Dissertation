fun getTypeVariableString(TypeVar(s)) = "'" ^ s
|  	getTypeVariableString(EqualityTypeVar(s)) = "''" ^ s
|	getTypeVariableString(ArithTypeVar(s)) = "'''" ^ s;

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
	| EQ => " = ");

fun prettyPrintTRecord([]) = ""
|	prettyPrintTRecord([(Lab(s),t)]) = s ^ ":" ^ prettyPrintType(t)
|	prettyPrintTRecord((Lab(s),t)::r) = s ^ ":" ^ prettyPrintType(t) ^ ", " ^ prettyPrintTRecord(r)
	
and prettyPrintType (t) = (case t of 
	  Int  => "int"
	| Real => "real"
	| Bool => "bool"
	| TFun(t1,t2) => "(" ^ prettyPrintType(t1) ^ " -> " ^ prettyPrintType(t2) ^ ")"
	| TRecord(r) => "{" ^ prettyPrintTRecord(r) ^ "}"
	| THole(TypeHole(t')) => getTypeVariableString(t'));

fun prettyPrintCVRecord([]) = ""
|	prettyPrintCVRecord([(Lab(s),cv)]) = s ^ "=" ^ prettyPrintConcreteVal(cv)
|	prettyPrintCVRecord((Lab(s),cv)::r) = s ^ "=" ^ prettyPrintConcreteVal(cv) ^ ", " ^
										prettyPrintCVRecord(r)
			
and prettyPrintConcreteVal(N(n)) = Int.toString(n)
|	prettyPrintConcreteVal(B(b)) = Bool.toString(b)
|	prettyPrintConcreteVal(R(r)) = Real.toString(r);
			
fun prettyPrintPRecord([]) = ""
|	prettyPrintPRecord([(Lab(s),pat)]) = s ^ "=" ^ prettyPrintPattern(pat)
|	prettyPrintPRecord((Lab(s),pat)::r) = s ^ "=" ^ prettyPrintPattern(pat) ^ ", " ^ 
											  prettyPrintPRecord(r)
		
and prettyPrintPattern(PVar(Var(s))) = s
|	prettyPrintPattern(PVal(cv)) = prettyPrintConcreteVal(cv)
|	prettyPrintPattern(PRecord(r)) = "{" ^ prettyPrintPRecord(r) ^ "}"
|	prettyPrintPattern(PWildcard) = "_";

fun prettyPrintHole(hole) = (case hole of 

	   SimpleHole(ValueHole(t)) => "v[" ^ getTypeVariableString(t) ^ "]"
	 | BinaryOpHole(oper,v1,v2) =>
		"v[ " ^ prettyPrintValue(v1) ^ getOperString(oper) ^ prettyPrintValue(v2) ^ " ]"
	 | ConditionHole(v,e1,e2) =>
		"v[ if " ^ prettyPrintValue(v) ^ " then " ^ prettyPrintExpression(Expression(e1)) ^ 
		" else " ^ prettyPrintExpression(Expression(e2)) ^ " ]"
	 | CaseHole(v,pat,e) =>
		"v[ case " ^ prettyPrintValue(v) ^ " of " ^
			prettyPrintPattern(pat) ^ " -> " ^ prettyPrintExpression(Expression(e)) ^ " ]"
	| AppHole(v1,v2) =>
		"v[ " ^ prettyPrintValue(v1) ^ " " ^ prettyPrintValue(v2) ^ " ]"
	| RecordHole(r) => "v[ {" ^ prettyPrintVRecord(r) ^ "} ]")

and prettyPrintVRecord([]) = ""
|	prettyPrintVRecord([(Lab(s),v)]) = s ^ "=" ^ prettyPrintValue(v)
|	prettyPrintVRecord((Lab(s),v)::r) = s ^ "=" ^ prettyPrintValue(v) ^ ", " ^ 
										  prettyPrintVRecord(r)
	
and prettyPrintValue(Concrete(cv)) = prettyPrintConcreteVal(cv)
|	prettyPrintValue(Fun(Var(s),t,e)) = "fn " ^ s ^ ":" ^ prettyPrintType(t) ^ " => " ^ prettyPrintExpression(Expression(e))
|	prettyPrintValue(VHole(hole)) = prettyPrintHole(hole)
|	prettyPrintValue(VRecord(r)) = "{" ^ prettyPrintVRecord(r) ^ "}"
		
and prettyPrintExpression(Stuck) = "Stuck"
| 	prettyPrintExpression(Expression(e)) = (case e of 
	
	  Value(v) => prettyPrintValue(v)
	| Variable(Var(s)) => s
	| ArithExpr(oper,e1,e2) => prettyPrintExpression(Expression(e1)) ^ getOperString(ArithOper(oper)) ^ 
							   prettyPrintExpression(Expression(e2))
	| BoolExpr(oper,e1,e2) =>  prettyPrintExpression(Expression(e1)) ^ getOperString(BoolOper(oper))  ^ 
							   prettyPrintExpression(Expression(e2))
	| Case(e1,pat,e3) => 
		"case " ^ prettyPrintExpression(Expression(e1)) ^ " of " ^ prettyPrintPattern(pat) ^
		" -> " ^ prettyPrintExpression(Expression(e3))
	| Condition(e1,e2,e3) => 
		"if " ^ prettyPrintExpression(Expression(e1)) ^ " then  " ^ prettyPrintExpression(Expression(e2)) ^ " else " ^
		prettyPrintExpression(Expression(e3))
	| App(e1,e2) => "(" ^ prettyPrintExpression(Expression(e1)) ^ ") (" ^ prettyPrintExpression(Expression(e2)) ^ ")"
	| Record(r) => "{" ^ prettyPrintERecord(r) ^ "}")
	
and prettyPrintERecord([]) = ""
|	prettyPrintERecord([(Lab(s),e)]) = s ^ "=" ^ prettyPrintExpression(Expression(e))
|	prettyPrintERecord((Lab(s),e)::r) = s ^ "=" ^ prettyPrintExpression(Expression(e)) ^ ", " ^ 
									  prettyPrintERecord(r);
	
fun prettyPrintSigma ([]) = ""
|   prettyPrintSigma ([(ValueHole(a),b)]) = 
		prettyPrintHole(SimpleHole(ValueHole(a))) ^ " -> " ^ prettyPrintValue(b)
|	prettyPrintSigma ((ValueHole(a),b)::l) =
		prettyPrintHole(SimpleHole(ValueHole(a))) ^ " -> " ^ prettyPrintValue(b) ^
		", " ^ prettyPrintSigma(l);	

fun prettyPrintTheta([]) = ""
| 	prettyPrintTheta([(TypeHole(a),b)]) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b)
|	prettyPrintTheta((TypeHole(a),b)::l) =
		prettyPrintType(THole(TypeHole(a))) ^ " -> " ^ prettyPrintType(b) ^
		", " ^ prettyPrintTheta(l);

fun prettyPrintGamma([]) = ""
|	prettyPrintGamma([(Var(x),e)]) =
		x ^ " -> " ^ prettyPrintExpression(Expression(e))
| 	prettyPrintGamma((Var(x),e)::l) =
		x ^ " -> " ^ prettyPrintExpression(Expression(e)) ^ ", " ^ prettyPrintGamma(l);
	
fun prettyPrintConfig(Config(e,sigma,theta)) =
		"\n\nFinal expression = " ^ prettyPrintExpression(e) ^ "\n" ^ 
		"Final value substitution = [" ^ prettyPrintSigma(sigma) ^ "]\n" ^ 
		"Final type substitution = [" ^ prettyPrintTheta(theta) ^ "]";