(* Takes the top-level expression e we are fuzzing, 
   and returns all the expression numbers of the occurences of variable x in e 
   ASSUME ALL VARIABLES UNIQUE *)
   
(* Thrown if input expression not in the correct format, 
   i.e. all expressions should be wrapped in a CounterExpr datatype *)
exception CounterExpression;
   
fun getVariableExpressions(e,x) = 

	let fun getVariableExpr(e) = (case e of 
	
			  CounterExpr(Variable(y),n) => if x=y then [n] else []
			| CounterExpr(e,_) 			 => getVariableExpr(e)
	
			| Value(v)             => getVariableValue(v)
			| Variable(_)          => raise CounterExpression
			| ArithExpr(_,e1,e2)   => append(getVariableExpr(e1),getVariableExpr(e2))
			| BoolExpr(_,e1,e2)    => append(getVariableExpr(e1),getVariableExpr(e2))
			| Case(e1,patExprList) => append(getVariableExpr(e1),getVariablePatExpr(patExprList))
			| Condition(e1,e2,e3)  => append(getVariableExpr(e1),append(getVariableExpr(e2),getVariableExpr(e3)))
			| App(e1,e2)		   => append(getVariableExpr(e1),getVariableExpr(e2))
			| Record(r)			   => getVariableERecord(r)
			| Let(_,_,e1,e2)       => append(getVariableExpr(e1),getVariableExpr(e2))
			| LetRec(_,_,e1,e2)    => append(getVariableExpr(e1),getVariableExpr(e2))
			| List(l)			   => getVariableEList(l)
			| Cons(e1,e2)          => append(getVariableExpr(e1),getVariableExpr(e2)))
		
		and getVariableEList(l) = (case l of 
		
			  []       => []
			| e1::rest => append(getVariableExpr(e1),getVariableEList(rest)))
		
		and getVariableERecord(r) = (case r of 
		
			  []           => []
			| (_,e1)::rest => append(getVariableExpr(e1),getVariableERecord(rest)))
			
		and getVariableVList(l) = (case l of 
		
			  []       => []
			| v1::rest => append(getVariableValue(v1),getVariableVList(rest)))
		
		and getVariableVRecord(r) = (case r of 
		
			  []           => []
			| (_,v1)::rest => append(getVariableValue(v1),getVariableVRecord(rest)))
		
		and getVariablePatExpr(patExprList) = (case patExprList of 
		
			  []           => []
			| (_,e1)::rest => append(getVariableExpr(e1),getVariablePatExpr(rest))) 
	
		and getVariableValue(v) = (case v of 
		
			  Concrete(_) => []
			| Fun(_,_,e)  => getVariableExpr(e)
			| VHole(h)    => getVariableHole(h)
			| VRecord(r)  => getVariableVRecord(r)
			| VList(l)    => getVariableVList(l))
		
		and getVariableHole(h) = (case h of 
		
			  SimpleHole(_)            => []
			| BinaryOpHole(_,v1,v2)    => append(getVariableValue(v1),getVariableValue(v2))
			| ConditionHole(v1,e1,e2)  => append(getVariableValue(v1),append(getVariableExpr(e1),getVariableExpr(e2)))
			| CaseHole(v1,patExprList) => append(getVariableValue(v1),getVariablePatExpr(patExprList))
			| AppHole(v1,v2)           => append(getVariableValue(v1),getVariableValue(v2))
			| RecordHole(r)			   => getVariableVRecord(r)
			| ListHole(l)			   => getVariableVList(l)
			| ConsHole(v1,v2)		   => append(getVariableValue(v1),getVariableValue(v2)))
			
	in getVariableExpr(e) end;
		