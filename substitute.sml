(* Takes expression and variable -> expression map, gamma 
   If there is a mapping x->e in gamma, it substitutes all occurrences of x for e in the 
   expression passed to the function,
   ensuring such substitutions are capture-avoiding in the case & function clauses *)

fun substitute(a,[]) = a

|	substitute(a,gamma:variableSub) =

	let fun substVal(v) = (case v of 

			  Concrete(_) => v 
			| Fun(x,t,e) => 
				(* must be capture avoiding *)
				if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))
				then substVal(alphaValue(v,getCounterAndUpdate(),[x]))
				else Fun(x,t,substExpr(e))
				
			| VHole(hole) => substHole(hole)
			| VRecord(r) => VRecord(substVRecord(r)))
			
		and substHole(hole) = (case hole of 
			 
			  SimpleHole(_) => VHole(hole)
			| BinaryOpHole(oper,v1,v2) => VHole(BinaryOpHole(oper,substVal(v1),substVal(v2)))
			| ConditionHole(v1,e1,e2) => VHole(ConditionHole(substVal(v1),substExpr(e1),substExpr(e2)))
			| CaseHole(v1,patExprList) => VHole(CaseHole(substVal(v1),substPatExprList(patExprList)))
			| AppHole(v1,v2) => VHole(AppHole(substVal(v1),substVal(v2)))
			| RecordHole(r) => VHole(RecordHole(substVRecord(r))))
		
		and substPatExprList(l) = (case l of 
				   
			  [] => []
			  
			(* must be capture avoiding: do on a pattern-expression pair by pair basis *)
			| (pat1,e1)::l1 => 
			
				let val dom = Substitution.domain(gamma);
					val fvRan = fv(Substitution.range(gamma));
					val fvPattern = fvPat(pat1)
				(* only change variable names for those clashing in gamma *)
				in (case union(listElement(dom,fvPattern),listElement(fvRan,fvPattern)) of
				
					  [] => (pat1,substExpr(e1))::substPatExprList(l1)
					  
					| l  => let val counter = getCounterAndUpdate() 
							(* Generate alpha-variant versions of the pattern & expression pair,
							   then re-call this method
							   Must pass same integer to pattern & expression alpha-variant methods *)
							in substPatExprList((alphaPat(pat1,counter,l),alphaExpr(e1,counter,l))::l1) end)
				end)
			
		and substExpr(e) = (case e of 
		
			  Value(v) => Value(substVal(v))
			| Variable(x) => substVariable(x)
			| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,substExpr(e1),substExpr(e2))
			| BoolExpr(boolOper,e1,e2) => BoolExpr(boolOper,substExpr(e1),substExpr(e2))
			| Case(e1,patExprList) => Case(substExpr(e1),substPatExprList(patExprList))
			| Condition(e1,e2,e3) => Condition(substExpr(e1),substExpr(e2),substExpr(e3))
			| App(e1,e2) => App(substExpr(e1),substExpr(e2))
			| Record(r) => Record(substERecord(r)))
				
	and substVariable(x) = if Substitution.contains(x,gamma)
						   then Substitution.get(x,gamma)
					       else Variable(x)
						   
	and substERecord(r) = (case r of 
			  []		    => r
			| (lab1,e1)::r1 => (lab1,substExpr(e1))::substERecord(r1))
			
	and substVRecord(r) = (case r of 
			  [] 			=> r
			| (lab1,v1)::r1 => (lab1,substVal(v1))::substVRecord(r1))
		
	in substExpr(a) end;
		