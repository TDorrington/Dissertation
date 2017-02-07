(* Function to generate a list of variables->types 
   Assumes all variables names are unique, and no shadowing *)
fun genVarTypes(expr) = 

	let fun genVarExpr(e) = (case e of 

		  Value(Concrete(_))  => []
		| Value(Fun(x,t,e))   => (x,t)::genVarExpr(e)
		| Value(VHole(hole))  => genVarHole(hole)
		| Value(VRecord(r))   => genVRecord(r)
		| Value(VList(l))     => genVList(l)
		| Variable(_) 		  => [] (* Added to list at binding site, not use site *)
		| ArithExpr(_,e1,e2)  => append(genVarExpr(e1),genVarExpr(e2))
		| BoolExpr(_,e1,e2)   => append(genVarExpr(e1),genVarExpr(e2))
		| Case(e,patExprList) => genVarCase(e,patExprList)
		| Condition(e1,e2,e3) => append(genVarExpr(e1),append(genVarExpr(e2),genVarExpr(e3)))
		| App(e1,e2)  		  => append(genVarExpr(e1),genVarExpr(e2))
		| Record(r)			  => genERecord(r)
		| Let(x,t,e1,e2)	  => (x,t)::append(genVarExpr(e1),genVarExpr(e2))
		| LetRec(x,t,v1,e2)	  => (x,t)::append(genVarExpr(Value(v1)),genVarExpr(e2))
		| List(l)			  => genEList(l)
		| Cons(e1,e2)		  => append(genVarExpr(e1),genVarExpr(e2))
		| CounterExpr(e1,_)	  => genVarExpr(e1))
	
	and fun genVarHole(h) = (case h of 
	
		  SimpleHole(_) 		  => []
		| BinaryOpHole(_,v1,v2)   => append(genVarExpr(Value(v1)),genVarExpr(Value(v2)))
		| ConditionHole(v,e1,e2)  => append(genVarExpr(Value(v)),append(genVarExpr(e1),genVarExpr(e2)))
		| CaseHole(v,patExprList) => genVarCase(Value(v),patExprList)
		| AppHole(v1,v2)		  => append(genVarExpr(Value(v1)),genVarExpr(Value(v2)))
		| RecordHole(r)		 	  => genVRecord(r)
		| ListHole(l)		  	  => genVList(l)
		| ConsHole(v1,v2)		  => append(genVarExpr(Value(v1)),genVarExpr(Value(v2))))
		
	and fun genVarCase(e,patExprList) = 
	
		let fun varTypeToGamma(typeList) = (case typeList of 
		
			  []          => []
			| (x,t)::rest => (x,
		
	and fun genERecord(r) = (case r of 
	
		  [] 		   => []
		| (_,e1)::rest => append(genVarExpr(e1),genERecord(rest)))
		
	and fun genEList(l) = (case l of 
	
		  [] 	   => []
		| e1::rest => append(genVarExpr(e1),genEList(rest)))
		
	and fun genVRecord(r) = (case r of 
	
		  []		   => []
		| (_,v1)::rest => append(genVarExpr(Value(v1)),genVRecord(rest)))
		
	and fun genVList(l) = (case l of 
	
		  []       => []
		| v1::rest => append(genVarExpr(Value(v1)),genVList(rest)))
		
	in genVarExpr(expr) end;	