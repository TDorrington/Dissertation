(* -------------------------------------------------------------------------------- *)
(* Gets the bottom value for a value substitution by following chains in the substitution sigma *)
     
fun resolveChainSigma(a,sigma:valSub) =

	let fun resolveExpr(e) = (case e of 
	
		  Value(v)    => Value(resolveChainSigma(v,sigma))
		| Variable(_) => e
		| ArithExpr(oper,e1,e2) => ArithExpr(oper,resolveExpr(e1),resolveExpr(e2))
		| BoolExpr(oper,e1,e2)  => BoolExpr(oper,resolveExpr(e1),resolveExpr(e2))
		| ExpressionPair(e1,e2) => ExpressionPair(resolveExpr(e1),resolveExpr(e2))
		| Case(e1,p,e2)  		=> Case(resolveExpr(e1),p,resolveExpr(e2))
		| Condition(e1,e2,e3)	=> Condition(resolveExpr(e1),resolveExpr(e2),resolveExpr(e3)))
	
	in case a of 

	  VHole(SimpleHole(ValueHole(typevar))) => 
		if(Substitution.contains(ValueHole(typevar),sigma))
		then resolveChainSigma(Substitution.get(ValueHole(typevar),sigma),sigma)
		else a
			
	| VHole(BinaryOp(oper,v1,v2)) => VHole(BinaryOp(oper,resolveChainSigma(v1,sigma),resolveChainSigma(v2,sigma)))
	
	| VHole(ConditionHole(v,e1,e2)) => VHole(ConditionHole(resolveChainSigma(v,sigma),resolveExpr(e1),resolveExpr(e2)))
	
	| VHole(CaseHole(v,pattern,e)) => VHole(CaseHole(resolveChainSigma(v,sigma),pattern,resolveExpr(e)))
	
	| ValuePair(v1,v2) => ValuePair(resolveChainSigma(v1,sigma),resolveChainSigma(v2,sigma))
	
	| _ => a (* Bottom value of int, real or bool *)

	end;

(* -------------------------------------------------------------------------------- *)	
(* Gets the bottom type for a type substitution theta *)
   
fun resolveChainTheta(a,theta:typeSub) = case a of

	  THole(hole) => 
		if(Substitution.contains(hole,theta)) 
		then resolveChainTheta(Substitution.get(hole,theta),theta)
		else a
	  
	| Pair(t1,t2) => Pair(resolveChainTheta(t1,theta),resolveChainTheta(t2,theta))
	
	| _ => a; (* Bottom type of Int, Real or Bool *)

	