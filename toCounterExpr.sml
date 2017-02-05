fun toCounterExpr(expr) =

	let
	
		(* Start counter at 1, so top level app(f,v['a]) can be expression 0 *)
		val cntr = ref 1;
	
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
			| LetRec(x,t,e1,e2)		=> CounterExpr(LetRec(x,t,cntrExpr(e1),cntrExpr(e2)),getCntr())
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
