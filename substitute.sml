fun substituteValue(v,gamma:variableSub) = (case v of 

	  ValuePair(v1,v2) => 
		ValuePair(substituteValue(v1,gamma),substituteValue(v2,gamma)) 
	
	| VHole(BinaryOp(oper,v1,v2)) =>
		VHole(BinaryOp(oper,substituteValue(v1,gamma),substituteValue(v2,gamma)))
	
	| VHole(ConditionHole(v1,e1,e2)) =>
		VHole(ConditionHole(substituteValue(v1,gamma),substitute(e1,gamma),substitute(e2,gamma)))
	
	| hole as VHole(CaseHole(v1,VariablePair(x,y),e)) => 
		(* must be capture avoiding *)
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma))
		in 	if ((element(dom,x) orelse element(dom,y)) orelse 
			    (element(fvRan,x) orelse element(fvRan,y)))
			then substituteValue(alphaValue(hole,getCounterAndUpdate(),[x,y]),gamma)
			else VHole(CaseHole(substituteValue(v1,gamma),VariablePair(x,y),substitute(e,gamma)))	
		end
	
	| VHole(SimpleHole(_)) => v
	
	| _ => v (* int, bool or real *))
 
and substitute(e,gamma) = case e of 

	  Value(v) => Value(substituteValue(v,gamma))
	
	| Variable(a) => if Substitution.contains(a,gamma)
					 then Substitution.get(a,gamma)
					 else Variable(a)

	| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,substitute(e1,gamma),substitute(e2,gamma))
	| BoolExpr (boolOper, e1,e2) => BoolExpr (boolOper, substitute(e1,gamma),substitute(e2,gamma))
	| ExpressionPair(e1,e2) 	 => ExpressionPair(substitute(e1,gamma),substitute(e2,gamma)) 
	| Condition(e1,e2,e3)		 => Condition(substitute(e1,gamma),substitute(e2,gamma),substitute(e3,gamma))
	
	| c as Case (e1,VariablePair(x,y),e2) => 
		(* Need to perform a capture-avoiding substitution, otherwise perform alpha conversion *)
		   
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma))
		in
			if ((element(dom,x) orelse element(dom,y)) orelse 
				(element(fvRan,x) orelse element(fvRan,y)))
			then(* Need to generate an alpha invariant version of the case expression
				   and then call substitute again
				   We pass in the unique integer we want to append to all variable names
				   and the list of variables to change is the binding variables in the case
				   expression: x and y *)
				substitute(alphaVariant(c,getCounterAndUpdate(),[x,y]),gamma)
			
			else(* Substitution is capture-avoiding
				   Recursively substitute in body of case expression *)
				Case(substitute(e1,gamma),VariablePair(x,y),substitute(e2,gamma))
				
		end;
		