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
	| TRecord(r)  => "{" ^ prettyPrintTRecord(r) ^ "}"
	| THole(TypeHole(t')) => getTypeVariableString(t')
	| TList(t1)  => prettyPrintType(t1) ^ " list");

fun prettyPrintCVRecord([]) = ""
|	prettyPrintCVRecord([(Lab(s),cv)]) = s ^ "=" ^ prettyPrintConcreteVal(cv)
|	prettyPrintCVRecord((Lab(s),cv)::r) = s ^ "=" ^ prettyPrintConcreteVal(cv) ^ ", " ^
										prettyPrintCVRecord(r)
			
and prettyPrintConcreteVal(N(n)) = Int.toString(n)
|	prettyPrintConcreteVal(B(b)) = Bool.toString(b)
|	prettyPrintConcreteVal(R(r)) = Real.toString(r)
|	prettyPrintConcreteVal(EmptyList) = "[ ]";
			
fun prettyPrintPRecord([]) = ""
|	prettyPrintPRecord([(Lab(s),pat)]) = s ^ "=" ^ prettyPrintPattern(pat)
|	prettyPrintPRecord((Lab(s),pat)::r) = s ^ "=" ^ prettyPrintPattern(pat) ^ ", " ^ prettyPrintPRecord(r)
		
and prettyPrintPattern(PVar(Var(s))) = s
|	prettyPrintPattern(PVal(cv)) = prettyPrintConcreteVal(cv)
|	prettyPrintPattern(PRecord(r)) = "{" ^ prettyPrintPRecord(r) ^ "}"
|	prettyPrintPattern(PWildcard) = "_"
|	prettyPrintPattern(PCons(pat1,pat2)) = prettyPrintPattern(pat1) ^ "::" ^ prettyPrintPattern(pat2);

fun prettyPrintPatExprList([]) = " " (* Should never occur *)
|	prettyPrintPatExprList([(pat1,e1)]) = prettyPrintPattern(pat1) ^ " -> " ^ prettyPrintExpression(Expression(e1))
|	prettyPrintPatExprList((pat1,e1)::l1) = 
	prettyPrintPattern(pat1) ^ " -> " ^ prettyPrintExpression(Expression(e1)) ^ " | " ^ prettyPrintPatExprList(l1)

and prettyPrintHole(hole) = (case hole of 

	   SimpleHole(ValueHole(t)) => "v[" ^ getTypeVariableString(t) ^ "]"
	 | BinaryOpHole(oper,v1,v2) =>
		"v[ " ^ prettyPrintValue(v1) ^ getOperString(oper) ^ prettyPrintValue(v2) ^ " ]"
	 | ConditionHole(v,e1,e2) =>
		"v[ if " ^ prettyPrintValue(v) ^ " then " ^ prettyPrintExpression(Expression(e1)) ^ 
		" else " ^ prettyPrintExpression(Expression(e2)) ^ " ]"
	 | CaseHole(v,patExprList) =>
		"v[ case " ^ prettyPrintValue(v) ^ " of " ^ prettyPrintPatExprList(patExprList) ^ " ]"
	| AppHole(v1,v2) =>
		"v[ " ^ prettyPrintValue(v1) ^ " " ^ prettyPrintValue(v2) ^ " ]"
	| RecordHole(r) => "v[ {" ^ prettyPrintVRecord(r) ^ "} ]"
	| ListHole(l) => "v[ [" ^ prettyPrintVList(l) ^ "] ]"
	| ConsHole(v1,v2) => "v[ " ^ prettyPrintValue(v1) ^ " :: " ^ prettyPrintValue(v2) ^ " ]")

and prettyPrintVRecord([]) = ""
|	prettyPrintVRecord([(Lab(s),v)]) = s ^ "=" ^ prettyPrintValue(v)
|	prettyPrintVRecord((Lab(s),v)::r) = s ^ "=" ^ prettyPrintValue(v) ^ ", " ^ 
										  prettyPrintVRecord(r)
										  
and prettyPrintVList([]) = ""
|	prettyPrintVList([v]) = prettyPrintValue(v)
|	prettyPrintVList(v1::rest) = prettyPrintValue(v1) ^ ", " ^ prettyPrintVList(rest)
	
and prettyPrintValue(Concrete(cv)) = prettyPrintConcreteVal(cv)
|	prettyPrintValue(Fun(Var(s),t,e)) = "fn " ^ s ^ ":" ^ prettyPrintType(t) ^ " => " ^ prettyPrintExpression(Expression(e))
|	prettyPrintValue(VHole(hole)) = prettyPrintHole(hole)
|	prettyPrintValue(VRecord(r)) = "{" ^ prettyPrintVRecord(r) ^ "}"
|	prettyPrintValue(VList(l)) = "[" ^ prettyPrintVList(l) ^"]"
		
and prettyPrintExpression(Stuck(i)) = "Stuck at " ^ Int.toString(i)
| 	prettyPrintExpression(Expression(e)) = (case e of 
	
	  Value(v) => prettyPrintValue(v)
	| Variable(Var(s)) => s
	| ArithExpr(oper,e1,e2) => prettyPrintExpression(Expression(e1)) ^ getOperString(ArithOper(oper)) ^ 
							   prettyPrintExpression(Expression(e2))
	| BoolExpr(oper,e1,e2) =>  prettyPrintExpression(Expression(e1)) ^ getOperString(BoolOper(oper))  ^ 
							   prettyPrintExpression(Expression(e2))
	| Case(e1,patExprList) => 
		"case " ^ prettyPrintExpression(Expression(e1)) ^ " of " ^ prettyPrintPatExprList(patExprList)
	| Condition(e1,e2,e3) => 
		"if " ^ prettyPrintExpression(Expression(e1)) ^ " then  " ^ prettyPrintExpression(Expression(e2)) ^ " else " ^
		prettyPrintExpression(Expression(e3))
	| App(e1,e2) => "(" ^ prettyPrintExpression(Expression(e1)) ^ ") (" ^ prettyPrintExpression(Expression(e2)) ^ ")"
	| Record(r) => "{" ^ prettyPrintERecord(r) ^ "}"
	| Let(Var(s),t,e1,e2) => "let val " ^ s ^ ":" ^ prettyPrintType(t) ^ " = (" ^ prettyPrintExpression(Expression(e1))
							   ^ ") in " ^ prettyPrintExpression(Expression(e2)) ^ " end"
	| LetRec(Var(s),t,e1,e2) => "let val rec " ^ s ^ ":" ^ prettyPrintType(t) ^ " = (" ^ prettyPrintExpression(Expression(e1))
							   ^ ") in " ^ prettyPrintExpression(Expression(e2)) ^ " end"
	| List(l) => "[" ^ prettyPrintEList(l) ^ "]"
	| Cons(e1,e2) => prettyPrintExpression(Expression(e1)) ^ " :: " ^ prettyPrintExpression(Expression(e2))
	| CounterExpr(e,i) => "( " ^ prettyPrintExpression(Expression(e)) ^ ", [" ^ Int.toString(i) ^ "] )")
	
and prettyPrintERecord([]) = ""
|	prettyPrintERecord([(Lab(s),e)]) = s ^ "=" ^ prettyPrintExpression(Expression(e))
|	prettyPrintERecord((Lab(s),e)::r) = s ^ "=" ^ prettyPrintExpression(Expression(e)) ^ ", " ^ 
									  prettyPrintERecord(r)
									  
and prettyPrintEList([]) = ""
|	prettyPrintEList([e]) = prettyPrintExpression(Expression(e))
|	prettyPrintEList(e1::rest) = prettyPrintExpression(Expression(e1)) ^ ", " ^ prettyPrintEList(rest);
	
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