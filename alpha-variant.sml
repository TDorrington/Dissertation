(* Takes an expression and returns an alpha-variant version
   Also takes a list of variables, vars, which we only need change; we cannot change all variables
   as we only want to change those variables bound in the closest fun/case binding *)

fun alphaVariable(Var(s),n,vars) = 

	if element(vars,Var(s)) 
	then Var(s^Int.toString(n))
	else Var(s);

fun alphaValue(v,n,vars) = (case v of 

	  ValuePair(v1,v2) => 
		ValuePair(alphaValue(v1,n,vars),alphaValue(v2,n,vars))
			  
	| VHole(BinaryOp(oper,v1,v2)) => 
		VHole(BinaryOp(oper,alphaValue(v1,n,vars),alphaValue(v2,n,vars)))
					
	| VHole(ConditionHole(v1,e1,e2)) => 
		VHole(ConditionHole(alphaValue(v1,n,vars),alphaVariant(e1,n,vars),alphaVariant(e2,n,vars))) 
					
	| VHole(CaseHole(v1,VariablePair(x,y),e)) =>
		VHole(CaseHole(v1,VariablePair(alphaVariable(x,n,vars),alphaVariable(y,n,vars)),alphaVariant(e,n,vars)))
	
	| VHole(AppHole(v1,v2)) =>
		VHole(AppHole(alphaValue(v1,n,vars),alphaValue(v2,n,vars)))
	
	| VHole(SimpleHole(_)) => v
	
	| Func(x,t,e) => Func(alphaVariable(x,n,vars),t,alphaVariant(e,n,vars))
		
	| _ => v (* int, bool or real *))
	
and alphaVariant(e,n,vars) = (case e of 
	
	  Value(v) => Value(alphaValue(v,n,vars))
	| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,alphaVariant(e1,n,vars),alphaVariant(e2,n,vars))
	| BoolExpr (boolOper, e1,e2) => BoolExpr (boolOper, alphaVariant(e1,n,vars),alphaVariant(e2,n,vars))
	| ExpressionPair(e1,e2) => ExpressionPair(alphaVariant(e1,n,vars),alphaVariant(e2,n,vars))
	| Condition(e1,e2,e3) => Condition(alphaVariant(e1,n,vars),alphaVariant(e2,n,vars),alphaVariant(e3,n,vars))
	| Case(e1,VariablePair(x,y),e2) =>
		(* do not touch the expression e1, as we may want to substitute free variables in there *)
		Case(e1,VariablePair(alphaVariable(x,n,vars),alphaVariable(y,n,vars)),alphaVariant(e2,n,vars))
	| App(e1,e2) => App(alphaVariant(e1,n,vars),alphaVariant(e2,n,vars))
	| Variable(x) => Variable(alphaVariable(x,n,vars)));
						