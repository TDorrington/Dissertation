(* narrow dynamically performs type-checking 
    takes a value v, type t and current substitutions and
    refines v to have type t, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta) = 

	let fun localNarrow(v,t,sigma,theta,gamma) = (case (v,t) of

	  (N(integer),Int) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (N(integer),THole(hole)) => (case unify([resolveChainTheta(t,theta),Int],theta) of 
	      NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
		
	| (B(boolean),Bool) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (B(boolean),THole(hole)) => (case unify([resolveChainTheta(t,theta),Bool],theta) of 
		  NONE  	  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
	
	| (R(real),Real) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (R(real),THole(hole)) => (case unify([resolveChainTheta(t,theta),Real],theta) of 
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
		
	| (ValuePair(v1,v2),Pair(t1,t2)) => 
						
		let val (Config(v1narrow,sigma1,theta1),gamma1) = localNarrow(v1,t1,sigma,theta,gamma);
			val (Config(v2narrow,sigma2,theta2),gamma2) = localNarrow(v2,t2,sigma1,theta1,gamma1)
			
		in (case (v1narrow,v2narrow) of
			(* return a value pair where we can, but if sub-call to localNarrow returns an expression
			   return an expression pair. This is okay in evaluate function
			   since all calls to localNarrow followed by another call to evaluate *)
		
			  (Expression(Value(va)),Expression(Value(vb))) => (Config(Expression(Value(ValuePair(va,vb))),sigma2,theta2),gamma2)
			| (Expression(e1narrow),Expression(e2narrow)) => (Config(Expression(ExpressionPair(e1narrow,e2narrow)),sigma2,theta2),gamma2)
			| _ => (Config(Stuck,sigma,theta),gamma))
		end
					
	(* We can localNarrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   localNarrow((v1,v2),'a) => localNarrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   localNarrow((v1,v2),''a) => localNarrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	  
	| (ValuePair(_,_),THole(TypeHole(ArithTypeVar(_)))) => (Config(Stuck,sigma,theta),gamma)
	
	| (ValuePair(_,_),THole(TypeHole(hole))) =>
	
		let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR
										  | ArithTypeVar(_)	   => ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
			val freshType1 = generateFreshTypeVar(freshTypeVar,theta);
			val freshType2 = generateFreshTypeVar(freshTypeVar,theta);
			val newType = Pair(freshType1,freshType2)
			val theta1 = Substitution.union(theta,TypeHole(hole),newType)
			
		in localNarrow(v,newType,sigma,theta1,gamma) end
	
	| (f as Func(x,t,e),Fun(t1,t2)) =>

		(* perform capture avoiding substitution *)
		if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))

		then localNarrow(alphaValue(f,getCounterAndUpdate(),[x]),Fun(t1,t2),sigma,theta,gamma)
		
		else (case unify([t,t1],theta) of 
		
		  NONE => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => 
				
			(* to avoid free variable exception, 
			   add to gamma an arbitrary mapping for variable x, using gen
			   instead of substituting *)
			let val gamma1 = Substitution.union(gamma,x,Value(gen(resolveChainTheta(t,theta1),theta1)))
				
			in (case localNarrowExpr(e,t2,sigma,theta1,gamma1) of
			
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(enarrow),sigma1,theta2),gamma2) =>
			
				(Config(Expression(Value(Func(x,resolveChainTheta(t,theta2),enarrow))),sigma1,theta2),gamma2))
				
			end)
				
	| (Func(_,t,_),THole(TypeHole(TypeVar(a)))) =>
	
		let val freshType = generateFreshTypeVar(TYPE_VAR,theta)
		in (case unify([THole(TypeHole(TypeVar(a))),Fun(t,freshType)],theta) of 
			(* unify to prevent type variable a being an element
			   of the free type variables of t *)
			   
			  NONE => (Config(Stuck,sigma,theta),gamma)
			| SOME theta1 => localNarrow(v,Fun(t,freshType),sigma,theta1,gamma))
		end
	
	| (VHole(SimpleHole(ValueHole(hole))),t) =>
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(hole), sigma) 
		
		then let val v = resolveChainSigma(v,sigma)
			 in (case typeof(substituteValue(v,gamma),theta) of
				
				  (NONE,_) => (Config(Stuck,sigma,theta),gamma)
				| (SOME(vtype),theta1) => (case unify( [THole(TypeHole(hole)), t, vtype], theta1) of
				
					  NONE => (Config(Stuck,sigma,theta),gamma)
					| SOME(theta2) => (Config(Expression(Value(v)),sigma,theta2),gamma)))
			 end
			
		else (case unify( [THole(TypeHole(hole)),t], theta) of
			 (* Hole not already instantiated *)
			 (* Generate value of type t and add to existing instantiations *)
			
				  NONE => (Config(Stuck,sigma,theta),gamma)
				| SOME(theta1) => (case gen(t,theta1) of 
		
					  (* Prevent adding a map from a value hole to itself as a value *)
					  v as VHole(SimpleHole(ValueHole(vtyVar))) =>
						
						if (hole=vtyVar) 
						then (Config(Expression(Value(v)), sigma, theta1), gamma)
						else (Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1),gamma)
				
					| v => (Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1),gamma)))
				
	| (VHole(BinaryOp(EXPR_PAIR,v1,v2)),t) =>
	  localNarrowExpr(ExpressionPair(Value(v1),Value(v2)),t,sigma,theta,gamma)
				
	| (VHole(BinaryOp(ArithOper(oper),v1,v2)),t) =>
	  localNarrowExpr(ArithExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
		
	| (VHole(BinaryOp(BoolOper(oper),v1,v2)),t) =>
	  localNarrowExpr(BoolExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
	
	| (VHole(CaseHole(v1,pat,e)),t) =>
	   localNarrowExpr(Case(Value(v1),pat,e),t,sigma,theta,gamma)
	
	| (VHole(ConditionHole(v1,e1,e2)),t) =>
	   localNarrowExpr(Condition(Value(v1),e1,e2),t,sigma,theta,gamma)
	   
	| (VHole(AppHole(v1,v2)),t) =>
		localNarrowExpr(App(Value(v1),Value(v2)),t,sigma,theta,gamma)
	
	(* for anything that does not match the above clauses, can only return a stuck expression *)
	| _  => (Config(Stuck, sigma, theta), gamma))
	   
	and localNarrowExpr(e,t,sigma,theta,gamma) = (case (e,t) of 

	  (Value(v),t) => localNarrow(v,t,sigma,theta,gamma)
	  
	| (Variable(x),t) => 
		(* Assume x always in gamma 
		   To localNarrow a variable to type t, localNarrow the underlying expression it refers 
		   to to type t and return the variable x as the expression
		   but with new value & type substitutions, and store the localNarrowed expression
		   in gamma to use when building back together the case expression *)
		   
		if Substitution.contains(x,gamma)
		
		then (case localNarrowExpr(Substitution.get(x,gamma),t,sigma,theta,gamma) of
			
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(eNew),sigma1,theta1),gamma1) => (Config(Expression(Variable(x)),sigma1,theta1),
															      Substitution.update(gamma1,x,eNew)))
		
		else raise FreeVariable
		
	(* e1/e2 must be of type Real (if concrete type given) since /:real*real->real *)
	| (ArithExpr(DIVIDE,e1,e2),Real) => (case localNarrowExpr(e1,Real,sigma,theta,gamma) of
	
	      (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,Real,sigma1,theta1,gamma1) of
			
		    (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		  | (Config(Expression(e2narrow),sigma2,theta2),gamma2) => 
				
			  (Config(Expression(ArithExpr(DIVIDE,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
	
	(* can also localNarrow e1/e2 to a type variable *)
	| (ArithExpr(DIVIDE,e1,e2),THole(hole)) => (case unify([resolveChainTheta(t,theta),Real],theta) of 
	
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => localNarrowExpr(e,Real,sigma,theta1,gamma))
	
	(* cannot localNarrow e1/e2 to anything else
	   needed so doesn't match arith expr clause below using wildcard *)
	| (ArithExpr(DIVIDE,_,_),_) => (Config(Stuck,sigma,theta),gamma)
	
	(* For op +,-,*
       t can only be of type Int, Real or an arithmetic type variable 
	   if t an equality type variable, can only be Int
	   if t a general type variable, can be localNarrowed to an arithmetic type variable *)
	| (ArithExpr(oper,e1,e2),t) => (case t of 
	
		  Bool => (Config(Stuck,sigma,theta),gamma)
		| Pair(_,_) => (Config(Stuck,sigma,theta),gamma)
		
		| THole(TypeHole(TypeVar(tyvar))) => 
			if Substitution.contains(TypeHole(TypeVar(tyvar)),theta)
			then localNarrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
			else let val freshArith = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
				 in localNarrowExpr(e,freshArith,sigma,Substitution.union(theta,TypeHole(TypeVar(tyvar)),freshArith),gamma) end
		
		| THole(TypeHole(EqualityTypeVar(tyvar))) =>  
			if Substitution.contains(TypeHole(EqualityTypeVar(tyvar)),theta)
			then localNarrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
			else localNarrowExpr(e,Int,sigma,Substitution.union(theta,TypeHole(EqualityTypeVar(tyvar)),Int),gamma)
		
		| THole(TypeHole(ArithTypeVar(tyvar))) =>
			if Substitution.contains(TypeHole(ArithTypeVar(tyvar)),theta)
			then localNarrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
			else let val narrowType = (case typeofexpr(substitute(e1,gamma),theta) of
		
					  (NONE,_) => t
					| (SOME t1,theta1) => (case typeofexpr(substitute(e2,gamma),theta1) of
					
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
					
				in (case localNarrowExpr(e1,narrowType,sigma,narrowTheta,gamma) of
		
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
				
					  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
					| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
					
						(Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
				end
		
		(* int or real *)
		| _ => (case localNarrowExpr(e1,t,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2),gamma2))))
					
	(* For op =, t can only be of type Bool *)
	| (BoolExpr(EQ,e1,e2),Bool) =>
	
		let val narrowType = (case typeofexpr(substitute(e1,gamma),theta) of
		
			  (NONE,_) => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
			| (SOME t1,theta1) => (case typeofexpr(substitute(e2,gamma),theta1) of
			
				  (NONE,_) => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
				| (SOME t2,theta2) =>
					
					let fun calcType (t1,t2) = (case (t1,t2) of 
					
						  (Int,_)  => Int
						| (_,Int)  => Int
						| (_,Bool) => Bool
						| (Bool,_) => Bool
						| (Pair(a1,a2),Pair(b1,b2)) => Pair(calcType(a1,b1),calcType(a2,b2))
						| (Pair(a1,a2),_) => Pair(calcType(a1,generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)),
												  calcType(a2,generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)))
						| (_,Pair(b1,b2)) => Pair(calcType(b1,generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)),
												  calcType(b2,generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)))
						(* avoid generating fresh equality type variable where possible *)
						| (THole(TypeHole(EqualityTypeVar(_))),_) => t1
						| (_,THole(TypeHole(EqualityTypeVar(_)))) => t2
						| _		   => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta))
				
					in calcType(t1,t2) end))
					
		in (case localNarrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(BoolExpr(EQ,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
		end
	
	(* can also localNarrow e1=e2 to a general type variable *)
	| (BoolExpr(EQ,e1,e2),THole(hole)) => (case unify([resolveChainTheta(t,theta),Bool],theta) of 
	
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => localNarrowExpr(e,Bool,sigma,theta1,gamma))
	
	(* cannot localNarrow e1=e2 to anything else 
	   needed so doesn't match arith expr clause below using wildcard *)
	| (BoolExpr(EQ,_,_),_) => (Config(Stuck,sigma,theta),gamma)
	
	(* For op <,<=,>=,>, t can only be of type Bool *)
	| (BoolExpr(oper,e1,e2),Bool) =>
	
		let val narrowType = (case typeofexpr(substitute(e1,gamma),theta) of
		
			  (NONE,_) => generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			| (SOME t1,theta1) => (case typeofexpr(substitute(e2,gamma),theta1) of
			
				  (NONE,_) => generateFreshTypeVar(ARITH_TYPE_VAR,theta)
				| (SOME t2,theta2) => (case (t1,t2) of
				
					  (Int,_) => Int
					| (_,Int) => Int
					| (_,Real) => Real
					| (Real,_) => Real
					(* avoid generating a fresh arithmetic type variable where possible *)
					| (THole(TypeHole(ArithTypeVar(_))),_) => t1
					| (_,THole(TypeHole(ArithTypeVar(_)))) => t2
					| _		   => generateFreshTypeVar(ARITH_TYPE_VAR,theta))))
	
		in (case localNarrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(BoolExpr(oper,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
		end
		
	(* can also localNarrow e1 op e2 to a general type variable (for op < <= > >=) *)
	| (BoolExpr(oper,e1,e2),THole(hole)) => (case unify([resolveChainTheta(t,theta),Bool],theta) of
	
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => localNarrowExpr(e,Bool,sigma,theta1,gamma))
		
	| (ExpressionPair(e1,e2),Pair(t1,t2)) => (case localNarrowExpr(e1,t1,sigma,theta,gamma) of
	
		  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t2,sigma1,theta1,gamma1) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
			
				(Config(Expression(ExpressionPair(e1narrow,e2narrow)),sigma2,theta2),gamma2)))
	
	(* We can localNarrow a pair to a type hole, if it is a general type variable
	   or equality type variable.
	   localNarrow((v1,v2),'a) equivalent to localNarrow((v1,v2),('a0,'a1)) for fresh 'a0,'a1, and 
	   localNarrow((v1,v2),''a) equivalent to localNarrow((v1,v2),(''a0,''a1)) for fresh ''a0,''a1
	   In both cases add map from type variable to pair of new fresh type variables in theta
	   Arithmetic type variables do not range over lists/pairs containing
	   arithmetic type variable *)
	   
	| (ExpressionPair(_,_),THole(TypeHole(ArithTypeVar(_)))) => (Config(Stuck,sigma,theta),gamma)
	
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
			
		in localNarrowExpr(ExpressionPair(e1,e2),newType,sigma,theta1,gamma) end
	
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
			
			(* we allow for 
			   'case (e1,e2) of ... '
			   'case (v1,v2) of ... '
			   'case x of ... '
			   for variable case, we recursively call the above two forms with substitution
			   but must make sure to put back x in case expression and not the new updated expressions' *)
			
			else let fun narrowSubExpr (c) = (case c of 
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case e1narrow of 
				  
					  ExpressionPair(e11,e12) => (case localNarrowExpr(e2,t,sigma1,theta1,(x,e11)::(y,e12)::gamma1) of
						
						  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
						| (Config(Expression(e2narrow),sigma2,theta2),gamma2) => 
						
							(Config(Expression(Case(ExpressionPair(Substitution.get(x,gamma2),Substitution.get(y,gamma2)),VariablePair(x,y),e2narrow)),
									sigma2,theta2),gamma2))
					  
					  
					| Value(ValuePair(v11,v12)) => (case localNarrowExpr(e2,t,sigma1,theta1,(x,Value(v11))::(y,Value(v12))::gamma1) of
						
						  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
						| (Config(Expression(e2narrow),sigma2,theta2),gamma2) => (case Substitution.get(x,gamma2) of 
						
							  Value(updatedV1) => (case Substitution.get(y,gamma2) of 
																	  
									  Value(updatedV2) => 
										(Config(Expression(Case(Value(ValuePair(updatedV1,updatedV2)),VariablePair(x,y),e2narrow)),sigma2,theta2),gamma2)
										  
									| updatedE2 => 
										(Config(Expression(Case(ExpressionPair(Value(updatedV1),updatedE2),VariablePair(x,y),e2narrow)),sigma2,theta2),gamma2))

							| updatedE1 =>
								(Config(Expression(Case(ExpressionPair(updatedE1,Substitution.get(y,gamma2)),VariablePair(x,y),e2narrow)),sigma2,theta2),gamma2)))
								 
					| Variable(z) => (case narrowSubExpr(Config(Expression(Substitution.get(z,gamma1)),sigma1,theta1),gamma1) of 

						  (Config(Expression(Case(_,p,e)),sigma2,theta2),gamma2) => (Config(Expression(Case(Variable(z),p,e)),sigma2,theta2),gamma2)
						
						| _ => (Config(Stuck,sigma,theta),gamma))
					
					| _ => (Config(Stuck,sigma,theta),gamma)))
			
			in narrowSubExpr(localNarrowExpr(e1,Pair(generateFreshTypeVar(TYPE_VAR,theta),generateFreshTypeVar(TYPE_VAR,theta)),sigma,theta,gamma)) end
			
		end
	
	| (App(e1,e2),t) =>
	
		(* get types we must localNarrow e1 and e2 to in pair, respectively
		   look at types of e1 and e2
		   - if e1 already of some function type ta->tb, and e2 of some type tc
		     use the function type (if can unify t&tb and ta&tc)
		   - otherwise if e1 typeable to some type, use type tc->t to localNarrow e1
		     and type tc to localNarrow e2
		   - otherwise use fresh type 'a->t to localNarrow e1, and the same fresh type 
		     'a to localNarrow e2 *)
		   
		let val (t1,t2,theta1) = 
		
			let val (ta,theta1) = typeofexpr(substitute(e1,gamma),theta);
				val (tb,theta2) = typeofexpr(substitute(e2,gamma),theta1)
			
			in (case (ta,tb) of 
			
				  (SOME (Fun(type1,type2)), SOME type3) => (case unify([t,type2],theta2) of 
				
					  NONE => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
							  in (Fun(fresh,t),fresh,theta) end
					  
					| SOME theta3 => (case unify([type1,type3],theta3) of 
					
						  NONE => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
								  in (Fun(fresh,t),fresh,theta) end
						   
						| SOME theta4 => (Fun(resolveChainTheta(type1,theta4),resolveChainTheta(type2,theta4)),
										  resolveChainTheta(type3,theta4),
										  theta4)))
										  
				| (SOME _, SOME type2) => (Fun(type2,t),type2,theta2)
										  
				| _ => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
				       in (Fun(fresh,t),fresh,theta) end)
			
			end

		in (case localNarrowExpr(e1,t1,sigma,theta1,gamma) of 
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			  
			| (Config(Expression(e1narrow),sigma2,theta2),gamma2) => (case localNarrowExpr(e2,t2,sigma2,theta2,gamma2) of 
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				
				| (Config(Expression(e2narrow),sigma3,theta3),gamma3) => 
				
					(Config(Expression(App(e1narrow,e2narrow)),sigma3,theta3),gamma3)))
		
		end
	
	| _ => (Config(Stuck,sigma,theta),gamma))
	
	in 
		let val (config,_) = localNarrow(v,t,sigma,theta,[]) 
		in config end
	end;
