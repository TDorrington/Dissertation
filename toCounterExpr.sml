fun toCounterExpr(expr) =

		(* Start counter at 1, so top level app(f,v['a]) can be expression 0 *)
	let val cntr = ref 1;
	
		fun getCntr() = let val current = !cntr;
						in (cntr := !cntr + 1);current end;
						
		fun cntrExpr(e) = (case e of 
		
			  Value(v) 				=> CounterExpr(Value(cntrValue(v)),getCntr())
			| Variable(_) 			=> CounterExpr(e,getCntr())
			| ArithExpr(oper,e1,e2) => CounterExpr(ArithExpr(oper,cntrExpr(e1),cntrExpr(e2)),getCntr())
			| BoolExpr(oper,e1,e2)  => CounterExpr(BoolExpr(oper,cntrExpr(e1),cntrExpr(e2)),getCntr())
			| Case(e1,patExprList)  => CounterExpr(Case(cntrExpr(e1),cntrPatExprList(patExprList)),getCntr())
			| Condition(e1,e2,e3)   => CounterExpr(Condition(cntrExpr(e1),cntrExpr(e2),cntrExpr(e3)),getCntr())
			| App(e1,e2)			=> CounterExpr(App(cntrExpr(e1),cntrExpr(e2)),getCntr())
			| Record(r)				=> CounterExpr(Record(cntrERecord(r)),getCntr())
			| Let(x,t,e1,e2)		=> CounterExpr(Let(x,t,cntrExpr(e1),cntrExpr(e2)),getCntr())
			| LetRec(x,t,v1,e2)		=> CounterExpr(LetRec(x,t,cntrValue(v1),cntrExpr(e2)),getCntr())
			| List(exprList)		=> CounterExpr(List(cntrEList(exprList)),getCntr())
			| Cons(e1,e2)			=> CounterExpr(Cons(cntrExpr(e1),cntrExpr(e2)),getCntr())
			
			| CounterExpr(e,i)		=> (case cntrExpr(e) of 
					  CounterExpr(e',_) => CounterExpr(e',i)
					| c 				=> CounterExpr(c,i)))
			
		and cntrValue(v) = (case v of 
		
			  Concrete(_) => v
			| Fun(x,t,e)  => Fun(x,t,cntrExpr(e))
			| VHole(h)    => VHole(cntrHole(h))
			| VRecord(r)  => VRecord(cntrVRecord(r))
			| VList(l)    => VList(cntrVList(l)))
			
		and cntrHole(h) = (case h of 
		
			  SimpleHole(_)			   => h
			| BinaryOpHole(oper,v1,v2) => BinaryOpHole(oper,cntrValue(v1),cntrValue(v2))
			| ConditionHole(v1,e1,e2)  => ConditionHole(cntrValue(v1),cntrExpr(e1),cntrExpr(e2))
			| CaseHole(v1,patExprList) => CaseHole(cntrValue(v1),cntrPatExprList(patExprList))
			| AppHole(v1,v2)		   => AppHole(cntrValue(v1),cntrValue(v2))
			| RecordHole(r)			   => RecordHole(cntrVRecord(r))
			| ListHole(l)			   => ListHole(cntrVList(l))
			| ConsHole(v1,v2)	       => ConsHole(cntrValue(v1),cntrValue(v2)))
			
		and cntrEList(l) = (case l of 
		
			  []	 => []
			| e1::l1 => cntrExpr(e1)::cntrEList(l1))
			
		and cntrVList(l) = (case l of 
			
			  []	 => []
			| v1::l1 => cntrValue(v1)::cntrVList(l1))
			
		and cntrPatExprList(l) = (case l of 
			
			  []			=> []
			| (pat1,e1)::l1 => (pat1,cntrExpr(e1))::cntrPatExprList(l1))
			
		and cntrERecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,e1)::r1 => (lab1,cntrExpr(e1))::cntrERecord(r1))
			
		and cntrVRecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,v1)::r1 => (lab1,cntrValue(v1))::cntrVRecord(r1))
			
	in cntrExpr(expr) end;
			 
fun getExpression(expr,n) = 

	let fun orSome(s1,s2) = (case (s1,s2) of 
	
			  (SOME(_),_) => s1
			| (_,SOME(_)) => s2
			| (_,_)		  => NONE)
	
		fun getExpr(e) = (case e of 
	
			  Value(v) 				=> getValue(v)
			| Variable(_) 			=> NONE
			| ArithExpr(oper,e1,e2) => orSome(getExpr(e1),getExpr(e2))
			| BoolExpr(oper,e1,e2)  => orSome(getExpr(e1),getExpr(e2))
			| Case(e1,patExprList)  => orSome(getExpr(e1),getPatExprList(patExprList))
			| Condition(e1,e2,e3)   => orSome(getExpr(e1),orSome(getExpr(e2),getExpr(e3)))
			| App(e1,e2)			=> orSome(getExpr(e1),getExpr(e2))
			| Record(r)				=> getERecord(r)
			| Let(x,t,e1,e2)		=> orSome(getExpr(e1),getExpr(e2))
			| LetRec(x,t,v1,e2)		=> orSome(getValue(v1),getExpr(e2))
			| List(exprList)		=> getEList(exprList)
			| Cons(e1,e2)			=> orSome(getExpr(e1),getExpr(e2))
			| CounterExpr(e,i)		=> if n=i 
									   then SOME (prettyPrintExpression(Expression(e)))
									   else getExpr(e))
									   
		and getValue(v) = (case v of 
		
			  Concrete(_) => NONE
			| Fun(x,t,e)  => getExpr(e)
			| VHole(h)    => getHole(h)
			| VRecord(r)  => getVRecord(r)
			| VList(l)    => getVList(l))
			
		and getHole(h) = (case h of 
		
			  SimpleHole(_)			   => NONE
			| BinaryOpHole(oper,v1,v2) => orSome(getValue(v1),getValue(v2))
			| ConditionHole(v1,e1,e2)  => orSome(getValue(v1),orSome(getExpr(e1),getExpr(e2)))
			| CaseHole(v1,patExprList) => orSome(getValue(v1),getPatExprList(patExprList))
			| AppHole(v1,v2)		   => orSome(getValue(v1),getValue(v2))
			| RecordHole(r)			   => getVRecord(r)
			| ListHole(l)			   => getVList(l)
			| ConsHole(v1,v2)	       => orSome(getValue(v1),getValue(v2)))
			
		and getVList(l) = (case l of 
		
			  [] 	 => NONE
			| v1::l1 => (case getValue(v1) of 
			
				  NONE    => getVList(l1)
				| SOME(s) => SOME(s)))
				
		and getEList(l) = (case l of 
		
			  []	 => NONE
			| e1::l1 => (case getExpr(e1) of 
			
				  NONE    => getEList(l1)
				| SOME(s) => SOME(s)))
				
		and getVRecord(r) = (case r of 
		
			  []	     => NONE
			| (_,v1)::r1 => (case getValue(v1) of 
			
				  NONE    => getVRecord(r1)
				| SOME(s) => SOME(s)))
				
		and getERecord(r) = (case r of 
		
			  []		 => NONE
			| (_,e1)::r1 => (case getExpr(e1) of 
			
				  NONE    => getERecord(r1)
				| SOME(s) => SOME(s)))
				
		and getPatExprList(l) = (case l of 
		
			  []         => NONE
			| (_,e1)::l1 => (case getExpr(e1) of 
			
				  NONE    => getPatExprList(l1)
				| SOME(s) => SOME(s)))
				
		in getExpr(expr) end;
		
			