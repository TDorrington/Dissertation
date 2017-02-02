(* Takes an expression and returns an alpha-variant version
   Also takes a list of variables, vars, which we only need change; we cannot change all variables
   as we only want to change those variables bound in the closest fun/case binding
   clashing with the current variable->expression substitution, gamma *)

fun alphaVariable(Var(s),n,vars) = 
	if element(vars,Var(s)) 
	then Var(s^Int.toString(n))
	else Var(s)
    
fun alphaPat(pat,n,vars) = 

	let fun alphaPRecord(r) = (case r of 
		  [] 			  => []
		| (lab1,pat1)::r1 => (lab1,alphaPat(pat1,n,vars))::alphaPRecord(r1))

	in (case pat of 
		  PWildcard    => PWildcard
		| PVar(x)      => PVar(alphaVariable(x,n,vars))
		| PVal(_)      => pat
		| PRecord(r)   => PRecord(alphaPRecord(r))
		| PCons(p1,p2) => PCons(alphaPat(p1,n,vars),alphaPat(p2,n,vars)))
	end;
   
fun alphaValue(v,n,vars) =  (case v of 
			  
	  Concrete(_) => v
	| Fun(x,t,e)  => Fun(alphaVariable(x,n,vars),t,alphaExpr(e,n,vars))
	| VHole(hole) => alphaHole(hole,n,vars)
	| VRecord(r)  => VRecord(alphaVRecord(r,n,vars))
	| VList(l)    => VList(alphaVList(l,n,vars)))
	
and alphaVList(l,n,vars) = (case l of 
	  []     => []
	| v1::l1 => alphaValue(v1,n,vars)::alphaVList(l1,n,vars))
	
and alphaEList(l,n,vars) = (case l of 
	  []     => []
	| e1::l1 => alphaExpr(e1,n,vars)::alphaEList(l1,n,vars))
	
and alphaVRecord(r,n,vars) = (case r of 
	  [] 			=> []
	| (lab1,v1)::r1 => (lab1,alphaValue(v1,n,vars))::alphaVRecord(r1,n,vars))
		
and alphaERecord(r,n,vars) = (case r of 
	  [] 			=> []
	| (lab1,e1)::r1 => (lab1,alphaExpr(e1,n,vars))::alphaERecord(r1,n,vars))
	
and alphaPatExprList(l,n,vars) = (case l of  
	  [] 			=> []
	| (pat1,e1)::l1 => (alphaPat(pat1,n,vars),alphaExpr(e1,n,vars))::alphaPatExprList(l1,n,vars))
	
and alphaHole(hole,n,vars) = (case hole of 
	
	  SimpleHole(_)            => VHole(hole)
	| BinaryOpHole(oper,v1,v2) => VHole(BinaryOpHole(oper,alphaValue(v1,n,vars),alphaValue(v2,n,vars)))
	| ConditionHole(v1,e1,e2)  => VHole(ConditionHole(alphaValue(v1,n,vars),alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))) 
	| CaseHole(v1,patExprList) => VHole(CaseHole(alphaValue(v1,n,vars),alphaPatExprList(patExprList,n,vars)))
	| AppHole(v1,v2)           => VHole(AppHole(alphaValue(v1,n,vars),alphaValue(v2,n,vars)))
	| RecordHole(r)            => VHole(RecordHole(alphaVRecord(r,n,vars)))
	| ListHole(l)              => VHole(ListHole(alphaVList(l,n,vars)))
	| ConsHole(v1,v2)		   => VHole(ConsHole(alphaValue(v1,n,vars),alphaValue(v2,n,vars))))
		
and alphaExpr(e,n,vars) = (case e of 
	
	  Value(v)                   => Value(alphaValue(v,n,vars))
	| Variable(x)                => Variable(alphaVariable(x,n,vars))
	| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))
	| BoolExpr(boolOper,e1,e2)   => BoolExpr(boolOper,alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))
	| Case(e1,patExprList)       => Case(alphaExpr(e1,n,vars),alphaPatExprList(patExprList,n,vars))
	| Condition(e1,e2,e3)        => Condition(alphaExpr(e1,n,vars),alphaExpr(e2,n,vars),alphaExpr(e3,n,vars))
	| App(e1,e2)                 => App(alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))
	| Record(r)                  => Record(alphaERecord(r,n,vars))
	| Let(x,t,e1,e2)             => Let(alphaVariable(x,n,vars),t,alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))
	| LetRec(x,t,v1,e2)          => LetRec(alphaVariable(x,n,vars),t,alphaValue(v1,n,vars),alphaExpr(e2,n,vars))
	| List(l)                    => List(alphaEList(l,n,vars))
	| Cons(e1,e2)				 => Cons(alphaExpr(e1,n,vars),alphaExpr(e2,n,vars))
	| CounterExpr(e1,i)			 => CounterExpr(alphaExpr(e1,n,vars),i));