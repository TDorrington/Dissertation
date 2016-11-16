(* 'narrow' dynamically performs type-checking 
    narrow : v * t * valSub * typeSub -> <v union stuck, valSub, typeSub>
    takes a value v, type t and current values & type substitutions and
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta) = case (v,t) of

	  (N(integer),Int)  	=> Config(Expression(Value(v)), sigma, theta)
	| (N(integer),THole(_)) => Config(Expression(Value(v)), sigma, theta)
	
	| (B(boolean),Bool)						   		   => Config(Expression(Value(v)), sigma, theta)
	| (B(boolean),THole(TypeHole(TypeVar(_))))		   => Config(Expression(Value(v)), sigma, theta)
	| (B(boolean),THole(TypeHole(EqualityTypeVar(_)))) => Config(Expression(Value(v)), sigma, theta)
	
	| (R(real),Real)  						     => Config(Expression(Value(v)), sigma, theta)
	| (R(real),THole(TypeHole(TypeVar(_))))      => Config(Expression(Value(v)), sigma, theta)
	| (R(real),THole(TypeHole(ArithTypeVar(_)))) => Config(Expression(Value(v)), sigma, theta)
	
	| (ValuePair(v1,v2),Pair(t1,t2)) => 
	
		(case typeof(v1,theta) of
		
			  (NONE,_) => Config(Stuck,sigma,theta)
			| (SOME(v1type),theta1) => (case unify([v1type,t1], theta1) of
		
				  NONE => Config(Stuck,sigma,theta)
				| SOME(theta2) => (case typeof(v2,theta2) of
				
					  (NONE,_) => Config(Stuck,sigma,theta)
					| (SOME(v2type),theta3) => (case unify([v2type,t2],theta3) of
				
						  NONE => Config(Stuck,sigma,theta)
						| SOME(theta4) => Config(Expression(Value(ValuePair(v1,v2))),sigma,theta4)))))
	
	(* We can narrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   narrow((v1,v2),'a) equivalent to narrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   narrow((v1,v2),''a) equivalent to narrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	   
	| (ValuePair(_,_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta)
	
	(* For general type variable or equality type variable *)
	| (ValuePair(v1,v2),THole(TypeHole(hole))) =>
		let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR;
			val freshType1 = generateFreshTypeVar(freshTypeVar,theta);
			val freshType2 = generateFreshTypeVar(freshTypeVar,theta);
			val newType = Pair(freshType1,freshType2)
			val theta1 = Substitution.union(theta,TypeHole(hole),newType)
		in
			narrow(ValuePair(v1,v2),newType,sigma,theta1)
		end
		
	| (VHole(SimpleHole(ValueHole(hole))),t) =>
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(hole), sigma) 
		
		then let val v = resolveChainSigma(VHole(SimpleHole(ValueHole(hole))),sigma)
			 in (case typeof(v,theta) of
				
				  (NONE,_) => Config(Stuck,sigma,theta)
				| (SOME(vtype),theta1) => (case unify( [THole(TypeHole(hole)), t, vtype], theta1) of
				
					  NONE => Config(Stuck,sigma,theta)
					| SOME(theta2) => Config(Expression(Value(v)),sigma,theta2)))
			 end
			
		else(* Hole not already instantiated *)
			(* Generate value of type t and add to existing instantiations *)
			
			(case unify( [THole(TypeHole(hole)),t], theta) of
			
				  NONE => Config(Stuck,sigma,theta)
				| SOME(theta1) => (case gen(t,theta1) of 
		
					  (* Prevent adding a map from a value hole to itself as a value *)
					  v as VHole(SimpleHole(ValueHole(vtyVar))) =>
						
						if (hole=vtyVar) 
						then Config(Expression(Value(v)), sigma, theta1)
						else Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1)
				
					| _ => Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1)))
				
	| (VHole(BinaryOp(EXPR_PAIR,hole1,hole2)),t) =>
	  narrowExpr(ExpressionPair(Value(VHole(hole1)),Value(VHole(hole2))),t,sigma,theta)
				
	| (VHole(BinaryOp(ArithOper(oper),hole1,hole2)),t) =>
	  narrowExpr(ArithExpr(oper,Value(VHole(hole1)),Value(VHole(hole2))),t,sigma,theta)
		
	| (VHole(BinaryOp(BoolOper(oper),hole1,hole2)),t) =>
	  narrowExpr(BoolExpr(oper,Value(VHole(hole1)),Value(VHole(hole2))),t,sigma,theta)
	
	| (VHole(CaseHole(hole,pat,e)),t) =>
	   narrowExpr(Case(Value(VHole(hole)),pat,e),t,sigma,theta)
	
	| (VHole(ConditionHole(hole,e1,e2)),t) =>
	   narrowExpr(Condition(Value(VHole(hole)),e1,e2),t,sigma,theta)
	
	| _  => Config(Stuck, sigma, theta)
	
(* ----------------------------------------------------------------------------------- *)
(* Takes an expression, a type and value/type hole substitutions
   It refines e to have type t by yielding triple of either same expression 
   and substitutions, or yields stuck state if not possible *)
   
and narrowExpr(e,t,sigma,theta) =

	let fun localNarrowExpr(e,t,sigma,theta,gamma) = (case (e,t) of 

	  (Value(v),t) => (narrow(v,t,sigma,theta),gamma)
	  
	| (Variable(x),t) => 
		(* Assume x always in gamma 
		   To narrow a variable to type t, narrow the underlying expression it refers 
		   to to type t and return the variable x as the expression
		   but with new value & type substitutions, and store the narrowed expression
		   in gamma to use when building back together the case expression *)
		(case localNarrowExpr(Substitution.get(x,gamma),t,sigma,theta,gamma) of
			
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(eNew),sigma1,theta1),gamma1) => (Config(Expression(Variable(x)),sigma1,theta1),
															      Substitution.update(gamma1,x,eNew)))
		
	(* e1/e2 must be of type Real since /:real*real->real *)
	| (ArithExpr(DIVIDE,e1,e2),Real) => (case localNarrowExpr(e1,Real,sigma,theta,gamma) of
	
		      (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,Real,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) => 
				
					(Config(Expression(ArithExpr(DIVIDE,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
					
	(* For op +,-,*
       t can only be of type Int, Real or an arithmetic type variable *)
	| (ArithExpr(oper,e1,e2),t) => (case t of 
	
		  Bool => (Config(Stuck,sigma,theta),gamma)
		| Pair(_,_) => (Config(Stuck,sigma,theta),gamma)
		| THole(TypeHole(TypeVar(_))) => (Config(Stuck,sigma,theta),gamma)
		| THole(TypeHole(EqualityTypeVar(_))) => (Config(Stuck,sigma,theta),gamma)
		
		(* int, real or '''a *)
		| _ => (case localNarrowExpr(e1,t,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2),gamma2))))
					
	(* For op =, t can only be of type Bool 
	   But sub-expressions can be any type in equality type variables set (fresh) *)
	| (BoolExpr(EQ,e1,e2),Bool) =>
	
		let val freshType = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (case localNarrowExpr(e1,freshType,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,freshType,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(BoolExpr(EQ,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
		end
		
	(* For op <,<=,>=,>
	   t can only be of type Bool
	   but sub-expressions can be any type in arithmetic type variables set (fresh) *)
	| (BoolExpr(oper,e1,e2),Bool) =>
	
		let val freshType = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
		in (case localNarrowExpr(e1,freshType,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,freshType,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(BoolExpr(oper,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
		end
	
	| (ExpressionPair(e1,e2),Pair(t1,t2)) => (case localNarrowExpr(e1,t1,sigma,theta,gamma) of
	
		  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t2,sigma1,theta1,gamma1) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
			
				(Config(Expression(ExpressionPair(e1narrow,e2narrow)),sigma2,theta2),gamma2)))
	
	(* We can narrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   narrow((v1,v2),'a) equivalent to narrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   narrow((v1,v2),''a) equivalent to narrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   In both cases add map from type variable to pair of new fresh type variables in theta
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	   
	| (ExpressionPair(_,_),THole(TypeHole(ArithTypeVar(_)))) => (Config(Stuck,sigma,theta),gamma)
	
	(* For general type variable or equality type variable *)
	| (ExpressionPair(e1,e2),THole(TypeHole(hole))) =>
		let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR;
			val freshType1 = generateFreshTypeVar(freshTypeVar,theta);
			val freshType2 = generateFreshTypeVar(freshTypeVar,theta);
			val newType = Pair(freshType1,freshType2)
			val theta1 = Substitution.union(theta,TypeHole(hole),newType)
		in
			localNarrowExpr(ExpressionPair(e1,e2),newType,sigma,theta1,gamma)
		end
	
	| (Condition(e1,e2,e3),t) => (case localNarrowExpr(e1,Bool,sigma,theta,gamma) of 
	
		  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t,sigma1,theta1,gamma1) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e2narrow),sigma2,theta2),gamma2) => (case localNarrowExpr(e3,t,sigma2,theta2,gamma2) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e3narrow),sigma3,theta3),gamma3) => 
				
					(Config(Expression(Condition(e1narrow,e2narrow,e3narrow)),sigma3,theta3),gamma3))))
	
	| (c as Case(e1,VariablePair(x,y),e2),t) => 
	
		(* perform capture avoiding substitution *)
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma))
		in  if ((element(dom,x) orelse element(dom,y)) orelse 
				(element(fvRan,x) orelse element(fvRan,y)))
		    then localNarrowExpr(alphaVariant(c,getCounterAndUpdate(),[x,y]),t,sigma,theta,gamma)
			
			else (case e1 of 
				  (* first expression must be a value pair or expression pair *)
				  ExpressionPair(e11,e12) => (case localNarrowExpr(e2,t,sigma,theta,(x,e11)::(y,e12)::gamma) of
					
					  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
					| (Config(Expression(e2narrow),sigma1,theta1),gamma1) => 
						(Config(Expression(Case(ExpressionPair(Substitution.get(x,gamma),Substitution.get(y,gamma)),VariablePair(x,y),e2narrow)),
								sigma1,theta1),gamma1))
				  
				  
				| Value(ValuePair(v11,v12)) => (case localNarrowExpr(e2,t,sigma,theta,(x,Value(v11))::(y,Value(v12))::gamma) of
					
					  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
					| (Config(Expression(e2narrow),sigma1,theta1),gamma1) => 
						let val Value(updatedv1) = Substitution.get(x,gamma);
							val Value(updatedv2) = Substitution.get(y,gamma)
						in
						(Config(Expression(Case(Value(ValuePair(updatedv1,updatedv2)),VariablePair(x,y),e2narrow)),
								sigma1,theta1),gamma1)
						end)
						
				| _ => (Config(Stuck,sigma,theta),gamma))
		end)
				
	in (let val (config,gamma) = localNarrowExpr(e,t,sigma,theta,[]) in config end) end;