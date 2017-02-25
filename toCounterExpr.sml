(* Function to uniquely number each sub expression by wrapping each in a CounterExpr datatype, which has an associated number
   Begin counting at 1, and end counting with the maximum number being associated with the most top-level CounterExpr wrapper *)
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
	
(* Function to drop all counter expression wrapper datatypes (ignoring unique sub-expression numbers),
   for pretty printing and testing purposes *)
fun dropCounterExpr(expr) = 

	let fun dropExpr(e) = (case e of 
		
			  Value(v) 				=> Value(dropValue(v))
			| Variable(_) 			=> e
			| ArithExpr(oper,e1,e2) => ArithExpr(oper,dropExpr(e1),dropExpr(e2))
			| BoolExpr(oper,e1,e2)  => BoolExpr(oper,dropExpr(e1),dropExpr(e2))
			| Case(e,patExprList)   => Case(dropExpr(e),dropPatExprList(patExprList))
			| Condition(e1,e2,e3)   => Condition(dropExpr(e1),dropExpr(e2),dropExpr(e3))
			| App(e1,e2)			=> App(dropExpr(e1),dropExpr(e2))
			| Record(r)				=> Record(dropERecord(r))
			| Let(x,t,e1,e2)		=> Let(x,t,dropExpr(e1),dropExpr(e2))
			| LetRec(x,t,e1,e2)		=> LetRec(x,t,dropExpr(e1),dropExpr(e2))
			| List(l)				=> List(dropEList(l))
			| Cons(e1,e2)			=> Cons(dropExpr(e1),dropExpr(e2))
			| CounterExpr(e,_)		=> dropExpr(e))
		
		and dropValue(v) = (case v of 
		
			  Concrete(_) => v
			| Fun(x,t,e)  => Fun(x,t,dropExpr(e))
			| VHole(h)    => VHole(dropHole(h))
			| VRecord(r)  => VRecord(dropVRecord(r))
			| VList(l)    => VList(dropVList(l)))
			
		and dropHole(h) = (case h of 
		
			  SimpleHole(_)			   => h
			| BinaryOpHole(oper,v1,v2) => BinaryOpHole(oper,dropValue(v1),dropValue(v2))
			| ConditionHole(v1,e1,e2)  => ConditionHole(dropValue(v1),dropExpr(e1),dropExpr(e2))
			| CaseHole(v1,patExprList) => CaseHole(dropValue(v1),dropPatExprList(patExprList))
			| AppHole(v1,v2)		   => AppHole(dropValue(v1),dropValue(v2))
			| RecordHole(r)			   => RecordHole(dropVRecord(r))
			| ListHole(l)			   => ListHole(dropVList(l))
			| ConsHole(v1,v2)	       => ConsHole(dropValue(v1),dropValue(v2)))
			
		and dropEList(l) = (case l of 
		
			  []	 => []
			| e1::l1 => dropExpr(e1)::dropEList(l1))
			
		and dropVList(l) = (case l of 
			
			  []	 => []
			| v1::l1 => dropValue(v1)::dropVList(l1))
			
		and dropPatExprList(l) = (case l of 
			
			  []			=> []
			| (pat1,e1)::l1 => (pat1,dropExpr(e1))::dropPatExprList(l1))
			
		and dropERecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,e1)::r1 => (lab1,dropExpr(e1))::dropERecord(r1))
			
		and dropVRecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,v1)::r1 => (lab1,dropValue(v1))::dropVRecord(r1))
			
	in dropExpr(expr) end;
	
(* Function to take a list of expressions, and drops the CounterExpr datatype from each one *)
fun iterDropCounterExpr(l) = (case l of 

	  []                  => []
	| (e1,i,errors)::rest => (dropCounterExpr(e1),i,errors)::iterDropCounterExpr(rest));
	