(* Takes an expression and returns an alpha-variant version
   Also takes a list of variables, vars, which we only need change; we cannot change all variables
   as we only want to change those variables bound in the closest fun/case binding
   clashing with the current variable->expression substitution, gamma *)

fun alphaVariant(a,n,vars) =

	let fun alphaVariable(Var(s)) = 

			if element(vars,Var(s)) 
			then Var(s^Int.toString(n))
			else Var(s)
	
		and alphaPat(pat) = (case pat of 
		
			  PWildcard => PWildcard
			| PVar(x) => PVar(alphaVariable(x))
			| PVal(_) => pat
			| PRecord(r) => PRecord(alphaPRecord(r)))
		
		and alphaVRecord(r) = (case r of 
			  [] 			=> r
			| (lab1,v1)::r1 => (lab1,alphaValue(v1))::alphaVRecord(r1))
		
		and alphaERecord(r) = (case r of 
			  [] 			=> r
			| (lab1,e1)::r1 => (lab1,alphaExpr(e1))::alphaERecord(r1))
			
		and alphaPRecord(r) = (case r of 
			  [] 			  => r
			| (lab1,pat1)::r1 => (lab1,alphaPat(pat1))::alphaPRecord(r1))
	
		and alphaHole(hole) = (case hole of 
		
			  SimpleHole(_) => VHole(hole)
			| BinaryOpHole(oper,v1,v2) => VHole(BinaryOpHole(oper,alphaValue(v1),alphaValue(v2)))
			| ConditionHole(v1,e1,e2) => VHole(ConditionHole(alphaValue(v1),alphaExpr(e1),alphaExpr(e2))) 
			(* don't alpha value v1 *)
			| CaseHole(v1,pat,e) => VHole(CaseHole(v1,alphaPat(pat),alphaExpr(e)))
			| AppHole(v1,v2) => VHole(AppHole(alphaValue(v1),alphaValue(v2)))
			| RecordHole(r) => VHole(RecordHole(alphaVRecord(r))))
	
		and alphaValue(v) = (case v of 
			  
			  Concrete(_) => v
			| Fun(x,t,e) => Fun(alphaVariable(x),t,alphaExpr(e))
			| VHole(hole) => alphaHole(hole)
			| VRecord(r) => VRecord(alphaVRecord(r)))
		
		and alphaExpr(e) = (case e of 
	
			  Value(v) => Value(alphaValue(v))
			| Variable(x) => Variable(alphaVariable(x))
			| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,alphaExpr(e1),alphaExpr(e2))
			| BoolExpr(boolOper,e1,e2) => BoolExpr(boolOper,alphaExpr(e1),alphaExpr(e2))
			(* don't alpha expr e1 *)
			| Case(e1,pat,e2) => Case(e1,alphaPat(pat),alphaExpr(e2))
			| Condition(e1,e2,e3) => Condition(alphaExpr(e1),alphaExpr(e2),alphaExpr(e3))
			| App(e1,e2) => App(alphaExpr(e1),alphaExpr(e2))
			| Record(r) => Record(alphaERecord(r)))
			
	in alphaExpr(a) end;
