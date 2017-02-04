(* Function to generate a list of variables->types 
   Assumes all variables names are unique, and no shadowing *)
fun genVarTypes(e) = 

	let fun genVarExpr = (case e of 

		  Value(Concrete(_))  => []
		| Value(Fun(x,t,e))   => (x,t)::getVarTypes(e)
		| Value(VHole(hole))  => genVarHole(hole)
		| Value(VRecord(r))   => unionVRecord(r)
		| Value(VList(l))     => unionVList(l)
		| Variable(_) 		  => [] (* Added to list at binding site, not use site *)
		| ArithExpr(_,e1,e2)  => append(getVarTypes(e1),getVarTypes(e2))
		| BoolExpr(_,e1,e2)   => append(getVarTypes(e1),getVarTypes(e2))
		| Case(e,patExprList) => genVarCase(e,patExprList)
		| Condition(e1,e2,e3) => append(getVarTypes(e1),append(getVarTypes(e2),getVarTypes(e3)))
		| App(e1,e2)  		  => append(getVarTypes(e1),getVarTypes(e2))
		| Record(r)			  => unionERecord(r)
		| Let(x,t,e1,e2)	  => (x,t)::append(getVarTypes(e1),getVarTypes(e2))
		| LetRec(x,t,v1,e2)	  => (x,t)::append(getVarTypes(Value(v1)),getVarTypes(e2))
		| List(l)			  => unionEList(l)
		| Cons(e1,e2)		  => append(getVarTypes(e1),getVarTypes(e2))
		| CounterExpr(e1,_)	  => getVarTypes(e1))
	
	and fun genVarHole(h) = (case h of 
	
		  SimpleHole(_) 		  => []
		| BinaryOpHole(_,v1,v2)   => append(getVarTypes(Value(v1)),getVarTypes(Value(v2)))
		| ConditionHole(v,e1,e2)  => append(getVarTypes(Value(v)),append(getVarTypes(e1),getVarTypes(e2)))
		| CaseHole(v,patExprList) => genVarCase(Value(v),patExprList)
		| AppHole(v1,v2)		  => append(getVarTypes(Value(v1)),getVarTypes(Value(v2)))
		| RecordHole(r)		 	  => unionVRecord(r)
		| ListHole(l)		  	  => unionVList(l)
		| ConsHole(v1,v2)		  => append(getVarTypes(Value(v1)),getVarTypes(Value(v2))))
		
	and fun genVarCase(e,patExprList) = 
		
	and fun unionERecord(r) = (case r of 
	
		  [] 		   => []
		| (_,e1)::rest => append(getVarTypes(e1),unionERecord(rest)))
		
	and fun unionEList(l) = (case l of 
	
		  [] 	   => []
		| e1::rest => append(getVarTypes(e1),unionEList(rest)))
		
	and fun unionVRecord(r) = (case r of 
	
		  []		   => []
		| (_,v1)::rest => append(getVarTypes(Value(v1)),unionVRecord(rest)))
		
	and fun unionVList(l) = (case l of 
	
		  []       => []
		| v1::rest => append(getVarTypes(Value(v1)),unionVList(rest)))
		
	in genVarExpr(e) end;	