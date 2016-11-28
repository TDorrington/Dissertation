(* 'narrow' dynamically performs type-checking 
    narrow : v * t * valSub * typeSub -> <v union stuck, valSub, typeSub>
    takes a value v, type t and current values & type substitutions and
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta,gamma) = (case (v,t) of

	  (N(integer),Int) => Config(Expression(Value(v)), sigma, theta, gamma)
	| (N(integer),THole(hole)) => 
		if Substitution.contains(hole,theta)
		then narrow(v,resolveChainTheta(THole(hole),theta),sigma,theta,gamma)
		else Config(Expression(Value(v)), sigma, Substitution.union(theta,hole,Int), gamma)
	
	| (B(boolean),Bool) => Config(Expression(Value(v)), sigma, theta, gamma)
	| (B(boolean),THole(TypeHole(TypeVar(hole)))) => 
		if Substitution.contains(TypeHole(TypeVar(hole)),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(TypeVar(hole))),theta),sigma,theta,gamma)
		else Config(Expression(Value(v)), sigma, Substitution.union(theta,TypeHole(TypeVar(hole)),Bool),gamma)
	| (B(boolean),THole(TypeHole(EqualityTypeVar(hole)))) =>
		if Substitution.contains(TypeHole(EqualityTypeVar(hole)),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(EqualityTypeVar(hole))),theta),sigma,theta,gamma)
		else Config(Expression(Value(v)), sigma, Substitution.union(theta,TypeHole(EqualityTypeVar(hole)),Bool),gamma)
	
	| (R(real),Real) => Config(Expression(Value(v)), sigma, theta, gamma)
	| (R(real),THole(TypeHole(TypeVar(hole)))) =>
		if Substitution.contains(TypeHole(TypeVar(hole)),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(TypeVar(hole))),theta),sigma,theta,gamma)
		else Config(Expression(Value(v)), sigma, Substitution.union(theta,TypeHole(TypeVar(hole)),Real),gamma)
	| (R(real),THole(TypeHole(ArithTypeVar(hole)))) => 
		if Substitution.contains(TypeHole(ArithTypeVar(hole)),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(ArithTypeVar(hole))),theta),sigma,theta,gamma)
		else Config(Expression(Value(v)), sigma, Substitution.union(theta,TypeHole(ArithTypeVar(hole)),Real),gamma)
	
	| (ValuePair(v1,v2),Pair(t1,t2)) => 
						
		let val Config(v1narrow,sigma1,theta1,gamma1) = narrow(v1,t1,sigma,theta,gamma);
			val Config(v2narrow,sigma2,theta2,gamma2) = narrow(v2,t2,sigma1,theta1,gamma1)
		in (case (v1narrow,v2narrow) of
			(* return a value pair where we can, but if sub-call to narrow returns an expression
			   return an expression pair. This should be okay in evaluate function
			   since all calls to narrow followed by another call to evaluate -
			   just what about sub-calls to narrow...CHECK BIND EXCEPTIONS *)
		
			  (Expression(Value(va)),Expression(Value(vb))) => Config(Expression(Value(ValuePair(va,vb))),sigma2,theta2,gamma2)
			| (Expression(e1narrow),Expression(e2narrow)) => Config(Expression(ExpressionPair(e1narrow,e2narrow)),sigma2,theta2,gamma2)
			| _ => Config(Stuck,sigma,theta,gamma))
		end
						

	(* We can narrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   narrow((v1,v2),'a) equivalent to narrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   narrow((v1,v2),''a) equivalent to narrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	   
	| (ValuePair(_,_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta,gamma)
	
	(* For general type variable or equality type variable *)
	| (ValuePair(v1,v2),THole(TypeHole(hole))) =>
		let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR
										  | ArithTypeVar(_)	   => ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
			val freshType1 = generateFreshTypeVar(freshTypeVar,theta);
			val freshType2 = generateFreshTypeVar(freshTypeVar,theta);
			val newType = Pair(freshType1,freshType2)
			val theta1 = Substitution.union(theta,TypeHole(hole),newType)
		in
			narrow(ValuePair(v1,v2),newType,sigma,theta1,gamma)
		end
		
	| (VHole(SimpleHole(ValueHole(hole))),t) =>
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(hole), sigma) 
		
		then let val v = resolveChainSigma(VHole(SimpleHole(ValueHole(hole))),sigma)
			 in (case typeof(v,theta,gamma) of
				
				  (NONE,_) => Config(Stuck,sigma,theta,gamma)
				| (SOME(vtype),theta1) => (case unify( [THole(TypeHole(hole)), t, vtype], theta1) of
				
					  NONE => Config(Stuck,sigma,theta,gamma)
					| SOME(theta2) => Config(Expression(Value(v)),sigma,theta2,gamma)))
			 end
			
		else (case unify( [THole(TypeHole(hole)),t], theta) of
			 (* Hole not already instantiated *)
			 (* Generate value of type t and add to existing instantiations *)
			
				  NONE => Config(Stuck,sigma,theta,gamma)
				| SOME(theta1) => (case gen(t,theta1) of 
		
					  (* Prevent adding a map from a value hole to itself as a value *)
					  v as VHole(SimpleHole(ValueHole(vtyVar))) =>
						
						if (hole=vtyVar) 
						then Config(Expression(Value(v)), sigma, theta1,gamma)
						else Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1,gamma)
				
					| v => Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1,gamma)))
				
	| (VHole(BinaryOp(EXPR_PAIR,v1,v2)),t) =>
	  narrowExpr(ExpressionPair(Value(v1),Value(v2)),t,sigma,theta,gamma)
				
	| (VHole(BinaryOp(ArithOper(oper),v1,v2)),t) =>
	  narrowExpr(ArithExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
		
	| (VHole(BinaryOp(BoolOper(oper),v1,v2)),t) =>
	  narrowExpr(BoolExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
	
	| (VHole(CaseHole(v1,pat,e)),t) =>
	   narrowExpr(Case(Value(v1),pat,e),t,sigma,theta,gamma)
	
	| (VHole(ConditionHole(v1,e1,e2)),t) =>
	   narrowExpr(Condition(Value(v1),e1,e2),t,sigma,theta,gamma)
	
	| _  => Config(Stuck, sigma, theta, gamma))
	
(* ----------------------------------------------------------------------------------- *)
(* Takes an expression, a type and value/type hole substitutions
   It refines e to have type t by yielding triple of either same expression 
   and substitutions, or yields stuck state if not possible *)
   
and narrowExpr(e,t,sigma,theta,gamma) = (case (e,t) of 

	  (Value(v),t) => narrow(v,t,sigma,theta,gamma)
	  
	| (Variable(x),t) => 
		(* Assume x always in gamma 
		   To narrow a variable to type t, narrow the underlying expression it refers 
		   to to type t and return the variable x as the expression
		   but with new value & type substitutions, and store the narrowed expression
		   in gamma to use when building back together the case expression *)
		(case narrowExpr(Substitution.get(x,gamma),t,sigma,theta,gamma) of
			
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(eNew),sigma1,theta1,gamma1) => Config(Expression(Variable(x)),sigma1,theta1,
															      Substitution.update(gamma1,x,eNew)))
		
	(* e1/e2 must be of type Real (if concrete type given) since /:real*real->real *)
	| (ArithExpr(DIVIDE,e1,e2),Real) => (case narrowExpr(e1,Real,sigma,theta,gamma) of
	
	      Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
		| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,Real,sigma1,theta1,gamma1) of
			
		    Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
		  | Config(Expression(e2narrow),sigma2,theta2,gamma2) => 
				
			  Config(Expression(ArithExpr(DIVIDE,e1narrow,e2narrow)),sigma2,theta2,gamma2)))
	
	(* can also narrow e1/e2 to a general type variable *)
	| (ArithExpr(DIVIDE,e1,e2),THole(TypeHole(TypeVar(hole)))) =>
		if Substitution.contains(TypeHole(TypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(TypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Real,sigma,Substitution.union(theta,TypeHole(TypeVar(hole)),Real),gamma)
		
	(* can also narrow e1/e2 to an arithmetic type variable *)
	| (ArithExpr(DIVIDE,e1,e2),THole(TypeHole(ArithTypeVar(hole)))) =>
		if Substitution.contains(TypeHole(ArithTypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(ArithTypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Real,sigma,Substitution.union(theta,TypeHole(ArithTypeVar(hole)),Real),gamma)
	
	(* cannot narrow e1/e2 to anything else *)
	| (ArithExpr(DIVIDE,_,_),_) => Config(Stuck,sigma,theta,gamma)
	
	(* For op +,-,*
       t can only be of type Int, Real or an arithmetic type variable 
	   if t an equality type variable, can only be Int
	   if t a general type variable, can be narrowed to an arithmetic type variable *)
	| (ArithExpr(oper,e1,e2),t) => (case t of 
	
		  Bool => Config(Stuck,sigma,theta,gamma)
		| Pair(_,_) => Config(Stuck,sigma,theta,gamma)
		
		| THole(TypeHole(TypeVar(tyvar))) => 
			if Substitution.contains(TypeHole(TypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(THole(TypeHole(TypeVar(tyvar))),theta),sigma,theta,gamma)
			else let val freshArith = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
				 in narrowExpr(e,freshArith,sigma,Substitution.union(theta,TypeHole(TypeVar(tyvar)),freshArith),gamma) end
		
		| THole(TypeHole(EqualityTypeVar(tyvar))) =>  
			if Substitution.contains(TypeHole(EqualityTypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(THole(TypeHole(EqualityTypeVar(tyvar))),theta),sigma,theta,gamma)
			else narrowExpr(e,Int,sigma,Substitution.union(theta,TypeHole(EqualityTypeVar(tyvar)),Int),gamma)
		
		| THole(TypeHole(ArithTypeVar(tyvar))) =>
			if Substitution.contains(TypeHole(ArithTypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(THole(TypeHole(ArithTypeVar(tyvar))),theta),sigma,theta,gamma)
			else let val narrowType = (case typeofexpr(e1,theta,gamma) of
		
					  (NONE,_) => t
					| (SOME t1,theta1) => (case typeofexpr(e2,theta1,gamma) of
					
						  (NONE,_) => t
						| (SOME t2,theta2) => (case (t1,t2) of
						
							  (Int,_)  => Int
							| (_,Int)  => Int
							| (_,Real) => Real
							| (Real,_) => Real
							| _		   => t)));
							
						val narrowTheta = if t = narrowType 
										  then theta 
										  else Substitution.union(theta,TypeHole(ArithTypeVar(tyvar)),narrowType);
					
				in (case narrowExpr(e1,narrowType,sigma,narrowTheta,gamma) of
		
				  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
				| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
				
					  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
					| Config(Expression(e2narrow),sigma2,theta2,gamma2) =>
					
						Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2,gamma2)))
				end
		
		(* int or real *)
		| _ => (case narrowExpr(e1,t,sigma,theta,gamma) of
		
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,t,sigma1,theta1,gamma1) of
			
				  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
				| Config(Expression(e2narrow),sigma2,theta2,gamma2) =>
				
					Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2,gamma2))))
					
	(* For op =, t can only be of type Bool 
	   But sub-expressions can be any type in equality type variables set (fresh)
	   unless concrete type already exists for e1 or e2 then use that if they're the same*)
	| (BoolExpr(EQ,e1,e2),Bool) =>
	
		let val narrowType = (case typeofexpr(e1,theta,gamma) of
		
			  (NONE,_) => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
			| (SOME t1,theta1) => (case typeofexpr(e2,theta1,gamma) of
			
				  (NONE,_) => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
				| (SOME t2,theta2) => (case (t1,t2) of
				
					  (Int,_)  => Int
					| (_,Int)  => Int
					| (_,Bool) => Bool
					| (Bool,_) => Bool
					| (Pair(p1,p2),_) => Pair(p1,p2)
					| (_,Pair(p1,p2)) => Pair(p1,p2)
					| _		   => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta))))
					
		in (case narrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
			
				  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
				| Config(Expression(e2narrow),sigma2,theta2,gamma2) =>
				
					Config(Expression(BoolExpr(EQ,e1narrow,e2narrow)),sigma2,theta2,gamma2)))
		end
	
	(* can also narrow e1=e2 to a general type variable *)
	| (BoolExpr(EQ,e1,e2),THole(TypeHole(TypeVar(hole)))) =>
		if Substitution.contains(TypeHole(TypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(TypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Bool,sigma,Substitution.union(theta,TypeHole(TypeVar(hole)),Bool),gamma)
		
	(* can also narrow e1=e2 to an equality type variable *)
	| (BoolExpr(EQ,e1,e2),THole(TypeHole(EqualityTypeVar(hole)))) =>
		if Substitution.contains(TypeHole(EqualityTypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(EqualityTypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Bool,sigma,Substitution.union(theta,TypeHole(EqualityTypeVar(hole)),Bool),gamma)
	
	(* cannot narrow e1=e2 to anything else *)
	| (BoolExpr(EQ,_,_),_) => Config(Stuck,sigma,theta,gamma)
	
	(* For op <,<=,>=,>
	   t can only be of type Bool
	   but sub-expressions can be any type in arithmetic type variables set (fresh)
       unless concrete type already exists for e1 or e2 then use that *)
	| (BoolExpr(oper,e1,e2),Bool) =>
	
		let val narrowType = (case typeofexpr(e1,theta,gamma) of
		
			  (NONE,_) => generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			| (SOME t1,theta1) => (case typeofexpr(e2,theta1,gamma) of
			
				  (NONE,_) => generateFreshTypeVar(ARITH_TYPE_VAR,theta)
				| (SOME t2,theta2) => (case (t1,t2) of
				
					  (Int,_) => Int
					| (_,Int) => Int
					| (_,Real) => Real
					| (Real,_) => Real
					| _		   => generateFreshTypeVar(ARITH_TYPE_VAR,theta))))
	
		in (case narrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
			
				  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
				| Config(Expression(e2narrow),sigma2,theta2,gamma2) =>
				
					Config(Expression(BoolExpr(oper,e1narrow,e2narrow)),sigma2,theta2,gamma2)))
		end
		
	(* can also narrow e1 op e2 to a general type variable (for op < <= > >=) *)
	| (BoolExpr(oper,e1,e2),THole(TypeHole(TypeVar(hole)))) =>
		if Substitution.contains(TypeHole(TypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(TypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Bool,sigma,Substitution.union(theta,TypeHole(TypeVar(hole)),Bool),gamma)
		
	(* can also narrow e1 op e2 to an equality type variable (for op < <= > >=) *)
	| (BoolExpr(oper,e1,e2),THole(TypeHole(EqualityTypeVar(hole)))) =>
		if Substitution.contains(TypeHole(EqualityTypeVar(hole)),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(EqualityTypeVar(hole))),theta),sigma,theta,gamma)
		else narrowExpr(e,Bool,sigma,Substitution.union(theta,TypeHole(EqualityTypeVar(hole)),Bool),gamma)
	
	| (ExpressionPair(e1,e2),Pair(t1,t2)) => (case narrowExpr(e1,t1,sigma,theta,gamma) of
	
		  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
		| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,t2,sigma1,theta1,gamma1) of
		
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(e2narrow),sigma2,theta2,gamma2) =>
			
				Config(Expression(ExpressionPair(e1narrow,e2narrow)),sigma2,theta2,gamma2)))
	
	(* We can narrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   narrow((v1,v2),'a) equivalent to narrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   narrow((v1,v2),''a) equivalent to narrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   In both cases add map from type variable to pair of new fresh type variables in theta
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	   
	| (ExpressionPair(_,_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta,gamma)
	
	(* For general type variable or equality type variable *)
	| (ExpressionPair(e1,e2),THole(TypeHole(hole))) =>
		let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR
										  | ArithTypeVar(_)    => ARITH_TYPE_VAR;
										  (* arith should never occur as matches above *)
			val freshType1 = generateFreshTypeVar(freshTypeVar,theta);
			val freshType2 = generateFreshTypeVar(freshTypeVar,theta);
			val newType = Pair(freshType1,freshType2)
			val theta1 = Substitution.union(theta,TypeHole(hole),newType)
		in
			narrowExpr(ExpressionPair(e1,e2),newType,sigma,theta1,gamma)
		end
	
	| (Condition(e1,e2,e3),t) => (case narrowExpr(e1,Bool,sigma,theta,gamma) of 
	
		  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
		| Config(Expression(e1narrow),sigma1,theta1,gamma1) => (case narrowExpr(e2,t,sigma1,theta1,gamma1) of
		
			  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
			| Config(Expression(e2narrow),sigma2,theta2,gamma2) => (case narrowExpr(e3,t,sigma2,theta2,gamma2) of
			
				  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
				| Config(Expression(e3narrow),sigma3,theta3,gamma3) => 
				
					Config(Expression(Condition(e1narrow,e2narrow,e3narrow)),sigma3,theta3,gamma3))))
	
	| (c as Case(e1,VariablePair(x,y),e2),t) => 
	
		(* perform capture avoiding substitution *)
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma))
		in  if ((element(dom,x) orelse element(dom,y)) orelse 
				(element(fvRan,x) orelse element(fvRan,y)))
		    then narrowExpr(alphaVariant(c,getCounterAndUpdate(),[x,y]),t,sigma,theta,gamma)
			
			else (case e1 of 
				  (* first expression must be a value pair or expression pair *)
				  
				  ExpressionPair(e11,e12) => (case narrowExpr(e2,t,sigma,theta,(x,e11)::(y,e12)::gamma) of
					
					  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
					| Config(Expression(e2narrow),sigma1,theta1,gamma1) => 
					
						Config(Expression(Case(ExpressionPair(Substitution.get(x,gamma1),Substitution.get(y,gamma1)),VariablePair(x,y),e2narrow)),
								sigma1,theta1,gamma1))
				  
				  
				| Value(ValuePair(v11,v12)) => (case narrowExpr(e2,t,sigma,theta,(x,Value(v11))::(y,Value(v12))::gamma) of
					
					  Config(Stuck,_,_,_) => Config(Stuck,sigma,theta,gamma)
					| Config(Expression(e2narrow),sigma1,theta1,gamma1) => (case Substitution.get(x,gamma1) of 
					
						  Value(updatedV1) => (case Substitution.get(y,gamma1) of 
																  
								  Value(updatedV2) => 
									Config(Expression(Case(Value(ValuePair(updatedV1,updatedV2)),VariablePair(x,y),e2narrow)),sigma1,theta1,gamma1)
									  
								| updatedE2 => 
									Config(Expression(Case(ExpressionPair(Value(updatedV1),updatedE2),VariablePair(x,y),e2narrow)),sigma1,theta1,gamma1))

						| updatedE1 =>
							Config(Expression(Case(ExpressionPair(updatedE1,Substitution.get(y,gamma1)),VariablePair(x,y),e2narrow)),sigma1,theta1,gamma1)))
							 
						
				| _ => Config(Stuck,sigma,theta,gamma))
		end
	
	| _ => Config(Stuck,sigma,theta,gamma))
