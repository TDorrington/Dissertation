(* Gets the bottom value for a value substitution by following chains in the
   value hole/type hole substitutions, sigma and theta *)
     
fun resolveChainSigma(a,sigma:valSub) =

	let fun resolveExpr(e) = (case e of 
	
			  Value(v)    			=> Value(resolveVal(v))
			| Variable(_)		    => e
			| ArithExpr(oper,e1,e2) => ArithExpr(oper,resolveExpr(e1),resolveExpr(e2))
			| BoolExpr(oper,e1,e2)  => BoolExpr(oper,resolveExpr(e1),resolveExpr(e2))
			| Case(e1,patExprList)  => Case(resolveExpr(e1),resolvePatExprList(patExprList))
			| Condition(e1,e2,e3)	=> Condition(resolveExpr(e1),resolveExpr(e2),resolveExpr(e3))
			| App(e1,e2) 			=> App(resolveExpr(e1),resolveExpr(e2))
			| Record(r)				=> Record(resolveERecord(r))
			| Let(x,t,e1,e2)        => Let(x,t,resolveExpr(e1),resolveExpr(e2))
			| LetRec(x,t,v1,e2)		=> LetRec(x,t,resolveVal(v1),resolveExpr(e2))
			| List(l)				=> List(resolveEList(l)))
	
		and resolveEList(l) = (case l of
			
			  []     => []
			| e1::l1 => resolveExpr(e1)::resolveEList(l1))
		
		and resolveVList(l) = (case l of 
		
			  []     => []
			| v1::l1 => resolveVal(v1)::resolveVList(l1))
	
		and resolvePatExprList(l) = (case l of 
		
			(* no need to resolve pattern, only the expression *)
			  [] 			=> []
			| (pat1,e1)::l1 => (pat1,resolveExpr(e1))::resolvePatExprList(l1))
	
		and resolveERecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,e1)::r1 => (lab1,resolveExpr(e1))::resolveERecord(r1))
			
		and resolveVRecord(r) = (case r of 
		
			  [] 			=> []
			| (lab1,v1)::r1 => (lab1,resolveVal(v1))::resolveVRecord(r1))
			
		and resolveValHole(hole) = (case hole of 
			
			  SimpleHole(ValueHole(tyvar)) => 
				if (Substitution.contains(ValueHole(tyvar),sigma))
				then resolveVal(Substitution.get(ValueHole(tyvar),sigma))
				else VHole(hole)
			
			| BinaryOpHole(oper,v1,v2) => VHole(BinaryOpHole(oper,resolveVal(v1),resolveVal(v2)))
			| ConditionHole(v,e1,e2)   => VHole(ConditionHole(resolveVal(v),resolveExpr(e1),resolveExpr(e2)))
			| CaseHole(v,patExprList)  => VHole(CaseHole(resolveVal(v),resolvePatExprList(patExprList)))
			| AppHole(v1,v2)           => VHole(AppHole(resolveVal(v1),resolveVal(v2)))
			| RecordHole(r)            => VHole(RecordHole(resolveVRecord(r)))
			| ListHole(l) 			   => VHole(ListHole(resolveVList(l))))
			
		and resolveVal(v) = (case v of 
			
			  Concrete(_) => v
			| Fun(x,t,e)  => Fun(x,t,resolveExpr(e))
			| VHole(hole) => resolveValHole(hole)
			| VRecord(r)  => VRecord(resolveVRecord(r))
			| VList(l)    => VList(resolveVList(l)))
			
	in resolveVal(a) end;

(* -------------------------------------------------------------------------------- *)	
(* Gets the bottom type for a type substitution theta *)
   
fun resolveChainTheta(a,theta:typeSub) = 

	let fun resolveTRecord(r) = (case r of 
	
			  [] 			=> []
			| (lab1,t1)::r1 => (lab1,resolveType(t1))::resolveTRecord(r1))
			
		and resolveType(t) = (case t of 

			  Int  => Int
			| Bool => Bool
			| Real => Real
			| TFun(t1,t2) => TFun(resolveType(t1),resolveType(t2))
			| TRecord(r)  => TRecord(resolveTRecord(r))
			| TList(t1)   => TList(resolveType(t1))
			| THole(hole) => 
				if(Substitution.contains(hole,theta)) 
				then resolveType(Substitution.get(hole,theta))
				else t)
	
	in resolveType(a) end;