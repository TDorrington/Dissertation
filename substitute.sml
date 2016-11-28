(* Takes an expression e, a map of variables to expressions, gamma,
   and substitutes all variables occurring in expression e, for which there is a map
   to some value in gamma, with that value. Denoted 'gamma e'
   Simply implemented by pattern matching on the structure of the expression
   and recursively calling substitute on its sub-expressions 
   Only non-trivial case is case expression. Need to perform a capture-avoiding substitution *)

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
	
	| _ => v(* int, bool or real *))
 
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
		(* Need to perform a capture-avoiding substitution for 'case e1 of (x,y) -> e2'
		   To check if we need to perform alpha conversion, 
		   check that x and y are not in the domain of gamma
		   and x and y are not in the set of free variables of the range of gamma
		   If these side conditions are not met, replace all occurrences of x and y
		   within e2 and the expression pair by xn and yn, respectively,
		   for a unique integer n (i.e. an alpha invariant of the expression) *)
		   
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
		