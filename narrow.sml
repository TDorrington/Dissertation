fun narrow(v,t,sigma,theta,gamma) = (case (v,t) of

	  (Concrete(N(integer)),Int) => Config(Expression(Value(v)), sigma, theta)
	| (Concrete(N(integer)),THole(hole)) => (case unify([t,Int],theta) of 
	      NONE 		  => Config(Stuck,sigma,theta)
		| SOME theta1 => Config(Expression(Value(v)),sigma,theta1))
		
	| (Concrete(B(boolean)),Bool) => Config(Expression(Value(v)), sigma, theta)
	| (Concrete(B(boolean)),THole(hole)) => (case unify([t,Bool],theta) of 
		  NONE  	  => Config(Stuck,sigma,theta)
		| SOME theta1 => Config(Expression(Value(v)),sigma,theta1))
	
	| (Concrete(R(real)),Real) => Config(Expression(Value(v)), sigma, theta)
	| (Concrete(R(real)),THole(hole)) => (case unify([t,Real],theta) of 
		  NONE 		  => Config(Stuck,sigma,theta)
		| SOME theta1 => Config(Expression(Value(v)),sigma,theta1))
		
	| (Concrete(EmptyList),TList(_)) => Config(Expression(Value(v)), sigma, theta)
	| (Concrete(EmptyList),THole(hole)) => (case unify([t,TList(generateFreshTypeVar(TYPE_VAR,theta))],theta) of 
		  NONE		  => Config(Stuck,sigma,theta)
		| SOME theta1 => Config(Expression(Value(v)),sigma,theta1))
	
	| (VRecord(r1),TRecord(r2)) => 
		(* Narrow each value in the record into a record of expressions,
		   and at the end attempt to package back into a value record *)
	
		let fun iterativeNarrow(l1,l2,s,t) = (case (l1,l2) of 
		
			  ([],[]) => Config(Expression(Record([])),s,t)
			| ([],_)  => Config(Stuck,sigma,theta)
			| (_,[])  => Config(Stuck,sigma,theta)
			| ((labv,v1)::rest1,(labt,t1)::rest2) => 
			
				if labv=labt 
				
				then (case narrow(v1,t1,s,t,gamma) of 
			
					  Config(Expression(v1narrow),s1,t1) => (case iterativeNarrow(rest1,rest2,s1,t1) of 
					  
						  Config(Expression(Record(r)),s2,t2) => Config(Expression(Record((labv,v1narrow)::r)),s2,t2)
							
						| _ => Config(Stuck,sigma,theta))
						
					| _ => Config(Stuck,sigma,theta))
					
				else Config(Stuck,sigma,theta))
				
			(* Converts a record of expressions to a record of values, if possible
		       All the entries must be of form Value(v), for some value v
		       If possible, returns record of value, wrapped in SOME
		       Otherwise not all entries of record are values, and returns NONE *)
			fun eToValRecord(r) = (case r of 
			  [] 			       => SOME []
			| (lab1,Value(v1))::r1 => (case eToValRecord(r1) of 
				  NONE   => NONE
				| SOME l => SOME ((lab1,v1)::l))
			| _					   => NONE)
		
		in (case iterativeNarrow(Record.sort(r1),Record.sort(r2),sigma,theta) of
		
			  c as Config(Expression(Record(r)),sigma1,theta1) => (case eToValRecord(r) of 
			  
				  NONE => c
				| SOME l => Config(Expression(Value(VRecord(l))),sigma1,theta1))
			  
			| c => c)
		
		end
					
	(* For narrowing a record to a type hole,
	   narrow({lab1=t1,...,labn=tn},'a)   => narrow({lab1='a1,...,labn='an},{lab1=t1,...,labn=tn})
	   narrow({lab1=t1,...,labn=tn},''a)  => narrow({lab1=t1,...,labn=tn},{lab1=''a1,...,labn=''an})
	   narrow({lab1=t1,...,labn=tn},'''a) => FAIL
	   for fresh 'a1,''a1,...,'an,''an *)
					
	| (VRecord(_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta)
	
	| (VRecord(r),THole(TypeHole(hole))) =>
		
		if Substitution.contains(TypeHole(hole),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											   | TypeVar(_) 		=> TYPE_VAR
											   | ArithTypeVar(_)	=> ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
				val genType = TRecord(genFreshTRecord(Record.getLabels(r),freshTypeVar,theta));
				val theta1 = Substitution.union(theta,TypeHole(hole),genType)
			
			in narrow(v,genType,sigma,theta1,gamma) end
	
	| (VList(l),TList(t)) => 
	
		let fun iterativeNarrow(l,sigma,theta) = (case l of
			
				  []       => Config(Expression(List([])),sigma,theta)
				| v1::rest => (case narrow(v1,t,sigma,theta,gamma) of 
				
					 Config(Expression(v1narrow),sigma1,theta1) => (case iterativeNarrow(rest,sigma1,theta1) of 
						 
						  Config(Expression(List(lnarrow)),sigma2,theta2) => Config(Expression(List(v1narrow::lnarrow)),sigma2,theta2)
						| _ => Config(Stuck,sigma,theta))
						
					| _ => Config(Stuck,sigma,theta)))
					
			(* Converts a list of expressions to a list of values, if possible
		       All the entries must be of form Value(v), for some value v
		       If possible, returns list of values, wrapped in SOME
		       Otherwise not all entries of list are values, and returns NONE *)
			fun eToValList(l) = (case l of 
				  [] 			=> SOME []
				| Value(v1)::l1 => (case eToValList(l1) of 
					  NONE   => NONE
					| SOME l => SOME (v1::l))
				| _				=> NONE)
		
		in (case iterativeNarrow(l,sigma,theta) of
		
			  c as Config(Expression(List(l)),sigma1,theta1) => (case eToValList(l) of 
			  
				  NONE   => c
				| SOME l => Config(Expression(Value(VList(l))),sigma1,theta1))
			  
			| c => c)
		
		end
		
	(* For narrowing a list to a type hole,
	   narrow([v1,...,vn],'a)   => narrow([v1,...,vn],'a0 list)
	   narrow([v1,...,vn],''a)  => narrow([v1,...,vn],''a0 list)
	   narrow([v1,...,vn],'''a) => FAIL
	   for fresh 'a0 and ''a0 *)
					
	| (VList(_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta)
	
	| (VList(_),THole(TypeHole(hole))) =>
		
		if Substitution.contains(TypeHole(hole),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											   | TypeVar(_) 		=> TYPE_VAR
											   | ArithTypeVar(_)	=> ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
				val genType = TList(generateFreshTypeVar(freshTypeVar,theta))
				val theta1 = Substitution.union(theta,TypeHole(hole),genType)
			
			in narrow(v,genType,sigma,theta1,gamma) end
	
	| (Fun(x,t,e),TFun(t1,t2)) =>

		(* perform capture avoiding substitution *)
		if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))

		then narrow(alphaValue(v,getCounterAndUpdate(),[x]),TFun(t1,t2),sigma,theta,gamma)
		
		else (case unify([t,t1],theta) of 
		
		  NONE => Config(Stuck,sigma,theta)
		| SOME theta1 => 
				
			(* to avoid free variable exception, 
			   add to gamma an arbitrary mapping for variable x, using gen
			   instead of substituting *)
			let val gamma1 = Substitution.union(gamma,x,Value(gen(resolveChainTheta(t,theta1),theta1)))
				
			in (case narrowExpr(e,t2,sigma,theta1,gamma1) of
			
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(enarrow),sigma1,theta2) => Config(Expression(Value(Fun(x,resolveChainTheta(t,theta2),enarrow))),sigma1,theta2))
				
			end)
				
	| (Fun(_,t1,_),THole(TypeHole(TypeVar(a)))) =>
	
		if Substitution.contains(TypeHole(TypeVar(a)),theta)
		then narrow(v,resolveChainTheta(THole(TypeHole(TypeVar(a))),theta),sigma,theta,gamma)
		
		else let val freshType = generateFreshTypeVar(TYPE_VAR,theta)
			 in (case unify([t,TFun(t1,freshType)],theta) of 
				(* unify to prevent type variable a being an element
				   of the free type variables of t1 *)
			   
				   NONE => Config(Stuck,sigma,theta)
				 | SOME theta1 => narrow(v,TFun(t1,freshType),sigma,theta1,gamma))
			 end
	
	| (VHole(SimpleHole(ValueHole(hole))),t) => 
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(hole),sigma) 
		
		then let val v = resolveChainSigma(v,sigma)
			 in (case typeofexpr(substitute(Value(v),gamma),theta) of
				
				  (NONE,_) => Config(Stuck,sigma,theta)
				  
				  (* DONT UNIFY [THole(TypeHole(hole)), t, vtype]
				     No need to add the original un-resolved value hole 
					 since we resolve chains anyway
					 could introduce loops *)
				| (SOME(vtype),theta1) => (case unify( [t,vtype], theta1) of
				
					  NONE => Config(Stuck,sigma,theta)
					| SOME(theta2) => Config(Expression(Value(v)),sigma,theta2)))
			 end
			
		else (case unify( [THole(TypeHole(hole)),t], theta) of
			 (* Hole not already instantiated *)
			 (* Generate value of type t and add to existing instantiations *)
			
				  NONE => Config(Stuck,sigma,theta)
				| SOME(theta1) => (case gen(t,theta1) of 
		
					  (* Prevent adding a map from a value hole to itself as a value *)
					  v as VHole(SimpleHole(ValueHole(vtyVar))) =>
						
						if (hole=vtyVar) 
						then Config(Expression(Value(v)), sigma, theta1)
						else Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1)
				
					| v => Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(hole),v), theta1)))
		
	(* For value holes, use narrowExpr implementation,
	   but if we can package returned configuration back into a value hole, do so *)
		
	| (VHole(BinaryOpHole(ArithOper(oper),v1,v2)),t) => (case narrowExpr(ArithExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma) of 
	    (* Either, or both, of the arguments must be value holes *)
		  Config(Expression(ArithExpr(oper,Value(VHole(v1)),Value(v2))),sigma1,theta1) => Config(Expression(Value(VHole(BinaryOpHole(ArithOper(oper),VHole(v1),v2)))),sigma1,theta1)
		| Config(Expression(ArithExpr(oper,Value(v1),Value(VHole(v2)))),sigma1,theta1) => Config(Expression(Value(VHole(BinaryOpHole(ArithOper(oper),v1,VHole(v2))))),sigma1,theta1)
		| c => c)
		
	| (VHole(BinaryOpHole(BoolOper(oper),v1,v2)),t) => (case narrowExpr(BoolExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma) of 
		(* Either, or both, of the arguments must be value holes *)
		  Config(Expression(BoolExpr(oper,Value(VHole(v1)),Value(v2))),sigma1,theta1) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(oper),VHole(v1),v2)))),sigma1,theta1)
		| Config(Expression(BoolExpr(oper,Value(v1),Value(VHole(v2)))),sigma1,theta1) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(oper),v1,VHole(v2))))),sigma1,theta1)
		| c => c)
	
	| (VHole(CaseHole(v1,patExprList)),t) => (case narrowExpr(Case(Value(v1),patExprList),t,sigma,theta,gamma) of
		(* Value we are case-ing on must be a value hole *)
		  Config(Expression(Case(Value(VHole(v1)),patEList)),sigma1,theta1) => Config(Expression(Value(VHole(CaseHole(VHole(v1),patEList)))),sigma1,theta1)
		| c => c)
	
	| (VHole(ConditionHole(v1,e1,e2)),t) => (case narrowExpr(Condition(Value(v1),e1,e2),t,sigma,theta,gamma) of
		(* Value we are condition-ing on must be a value hole *)
		  Config(Expression(Condition(Value(VHole(v1)),e1,e2)),sigma1,theta1) => Config(Expression(Value(VHole(ConditionHole(VHole(v1),e1,e2)))),sigma1,theta1)
		| c => c)
	   
	| (VHole(AppHole(v1,v2)),t) => (case narrowExpr(App(Value(v1),Value(v2)),t,sigma,theta,gamma) of
		(* Either, or both, the values in the application must be a value hole *)
		  Config(Expression(App(Value(v1),Value(VHole(v2)))),sigma1,theta1) => Config(Expression(Value(VHole(AppHole(v1,VHole(v2))))),sigma1,theta1)
		| Config(Expression(App(Value(VHole(v1)),Value(v2))),sigma1,theta1) => Config(Expression(Value(VHole(AppHole(VHole(v1),v2)))),sigma1,theta1)
		| c => c)
	
	| (VHole(RecordHole(r)),t) =>
	
		  (* Converts record of values to a record of expressions,
		     i.e. just wraps v into Value(v) *)
		let fun valToERecord(r) = (case r of 
			  []		    => []
			| (lab1,v1)::r1 => (lab1,Value(v1))::valToERecord(r1))
		
		(* Don't bother trying to wrap into a value hole record again - complicated logic
		   Should do this in the following call to evaluate from calling code in evaluate *)
		in narrowExpr(Record(valToERecord(r)),t,sigma,theta,gamma) end
	
	| (VHole(ListHole(l)),t) =>
	
		(* Converts list of values to a list of expressions,
		   i.e. just wraps v into Value(v) *)
		let fun valToEList(l) = (case l of
			  [] 	 => []
			| v1::l1 => Value(v1)::valToEList(l1))
		
		in narrowExpr(List(valToEList(l)),t,sigma,theta,gamma) end
		
	(* for anything that does not match the above clauses, can only return a stuck expression *)
	| _  => Config(Stuck, sigma, theta))
	   
and narrowExpr(e,t,sigma,theta,gamma) = (case (e,t) of 

	  (Value(v),t) => narrow(v,t,sigma,theta,gamma)
	 	
	(* e1/e2 must be of type Real (if concrete type given) since /:real*real->real *)
	| (ArithExpr(DIVIDE,e1,e2),Real) => (case narrowExpr(e1,Real,sigma,theta,gamma) of
	
	      Config(Stuck,_,_) => Config(Stuck,sigma,theta)
		| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,Real,sigma1,theta1,gamma) of
			
		    Config(Stuck,_,_) => Config(Stuck,sigma,theta)
		  | Config(Expression(e2narrow),sigma2,theta2) => Config(Expression(ArithExpr(DIVIDE,e1narrow,e2narrow)),sigma2,theta2)))
	
	(* can also narrow e1/e2 to a type variable *)
	| (ArithExpr(DIVIDE,e1,e2),THole(hole)) => (case unify([t,Real],theta) of 
	
		  NONE 		  => Config(Stuck,sigma,theta)
		| SOME theta1 => narrowExpr(e,Real,sigma,theta1,gamma))
	
	(* cannot narrow e1/e2 to anything else
	   needed so doesn't match arith expr clause below using wildcard *)
	| (ArithExpr(DIVIDE,_,_),_) => Config(Stuck,sigma,theta)
	
	(* For op +,-,*
       t can only be of type Int, Real or an arithmetic type variable 
	   if t an equality type variable, can only be Int
	   if t a general type variable, can be narrowed to an arithmetic type variable *)
	| (ArithExpr(oper,e1,e2),t) => (case t of 
	
		  Bool 		 => Config(Stuck,sigma,theta)
		| TRecord(_) => Config(Stuck,sigma,theta)
		| TList(_)   => Config(Stuck,sigma,theta)
		| TFun(_,_)  => Config(Stuck,sigma,theta)
		
		| THole(TypeHole(TypeVar(tyvar))) => 
			if Substitution.contains(TypeHole(TypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
			else let val freshArith = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
				 in narrowExpr(e,freshArith,sigma,Substitution.union(theta,TypeHole(TypeVar(tyvar)),freshArith),gamma) end
		
		| THole(TypeHole(EqualityTypeVar(tyvar))) =>  
			if Substitution.contains(TypeHole(EqualityTypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
			else narrowExpr(e,Int,sigma,Substitution.union(theta,TypeHole(EqualityTypeVar(tyvar)),Int),gamma)
		
		| THole(TypeHole(ArithTypeVar(tyvar))) => 
			if Substitution.contains(TypeHole(ArithTypeVar(tyvar)),theta)
			then narrowExpr(e,resolveChainTheta(t,theta),sigma,theta,gamma)
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
					
				in (case narrowExpr(e1,narrowType,sigma,narrowTheta,gamma) of
		
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma) of
				
					  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
					| Config(Expression(e2narrow),sigma2,theta2) => Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2)))
				
				end
		
		(* int or real *)
		| _ => (case narrowExpr(e1,t,sigma,theta,gamma) of
		
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,t,sigma1,theta1,gamma) of
			
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				| Config(Expression(e2narrow),sigma2,theta2) => Config(Expression(ArithExpr(oper,e1narrow,e2narrow)),sigma2,theta2))))
					
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
						
						| (TRecord(r1),TRecord(r2)) => (case iterCalcType(Record.sort(r1),Record.sort(r2)) of 
							  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
							| SOME l => TRecord(l))
							
						| (TRecord(r1),_) =>
							let val r2 = genFreshTRecord(Record.getLabels(r1),EQUALITY_TYPE_VAR,theta2)
							in (case iterCalcType(Record.sort(r1),Record.sort(r2)) of
								  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
								| SOME l => TRecord(l))
							end
							
						| (_,TRecord(r2)) =>
							let val r1 = genFreshTRecord(Record.getLabels(r2),EQUALITY_TYPE_VAR,theta2)
							in (case iterCalcType(Record.sort(r1),Record.sort(r2)) of 
								  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
								| SOME l => TRecord(l))
							end
						
						(* Recursively perform this analysis on type of list elements
						   Call function with repeated type argument, as symmetric *)
						| (TList(t1),_) => TList(calcType(t1,t1))
						| (_,TList(t2)) => TList(calcType(t2,t2))
						
						(* avoid generating fresh equality type variable where possible *)
						| (THole(TypeHole(EqualityTypeVar(_))),_) => t1
						| (_,THole(TypeHole(EqualityTypeVar(_)))) => t2
						
						| _	=> generateFreshTypeVar(EQUALITY_TYPE_VAR,theta))
					
					and iterCalcType(l1,l2) = (case (l1,l2) of 
					
						  ([],[]) => SOME []
						| ([],_)  => NONE
						| (_,[])  => NONE
						| ((labA,a)::restA,(labB,b)::restB) => 
							if labA=labB
							then (case iterCalcType(restA,restB) of 
								  NONE => NONE
								| SOME l => SOME ((labA,calcType(a,b))::l))
							else NONE)
						
					in calcType(t1,t2) end))
					
		in (case narrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma) of
			
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				| Config(Expression(e2narrow),sigma2,theta2) => Config(Expression(BoolExpr(EQ,e1narrow,e2narrow)),sigma2,theta2)))
		end
	
	(* can also narrow e1=e2 to a general type variable *)
	| (BoolExpr(EQ,e1,e2),THole(hole)) => (case unify([t,Bool],theta) of 
	
		  NONE 		  => Config(Stuck,sigma,theta)
		| SOME theta1 => narrowExpr(e,Bool,sigma,theta1,gamma))
	
	(* cannot narrow e1=e2 to anything else 
	   needed so doesn't match arith expr clause below using wildcard *)
	| (BoolExpr(EQ,_,_),_) => Config(Stuck,sigma,theta)
	
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
	
		in (case narrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,narrowType,sigma1,theta1,gamma) of
			
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				| Config(Expression(e2narrow),sigma2,theta2) => Config(Expression(BoolExpr(oper,e1narrow,e2narrow)),sigma2,theta2)))
		end
		
	(* can also narrow e1 op e2 to a general type variable (for op < <= > >=) *)
	| (BoolExpr(oper,e1,e2),THole(hole)) => (case unify([t,Bool],theta) of
	
		  NONE 		  => Config(Stuck,sigma,theta)
		| SOME theta1 => narrowExpr(e,Bool,sigma,theta1,gamma))
					
	(* For narrowing a record to a type hole,
	   narrow({lab1=t1,...,labn=tn},'a)   => narrow({lab1='a1,...,labn='an},{lab1=t1,...,labn=tn})
	   narrow({lab1=t1,...,labn=tn},''a)  => narrow({lab1=t1,...,labn=tn},{lab1=''a1,...,labn=''an})
	   narrow({lab1=t1,...,labn=tn},'''a) => FAIL
	   for fresh 'a1,''a1,...,'an,''an *)
					
	| (Record(r1),TRecord(r2)) => 
	
		let fun iterativeNarrowExpr(l1,l2,s,t) = (case (l1,l2) of 
		
			  ([],[]) => Config(Expression(Record([])),s,t)
			| ([],_)  => Config(Stuck,sigma,theta)
			| (_,[])  => Config(Stuck,sigma,theta)
			| ((labe,e1)::rest1,(labt,type1)::rest2) =>
			
				if labe = labt 
				
				then (case narrowExpr(e1,type1,s,t,gamma) of 
			
					  Config(Expression(e1narrow),s1,t1) => (case iterativeNarrowExpr(rest1,rest2,s1,t1) of 
					  
						  Config(Expression(Record(r)),s2,t2) => Config(Expression(Record((labe,e1narrow)::r)),s2,t2)
							
						| _ => Config(Stuck,sigma,theta))
						
					| _ => Config(Stuck,sigma,theta))
				
				else Config(Stuck,sigma,theta))
		
		in iterativeNarrowExpr(Record.sort(r1),Record.sort(r2),sigma,theta) end
	
	| (Record(_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta)
	
	(* For general type variable or equality type variable *)
	| (Record(r1),THole(TypeHole(hole))) =>
	
		if Substitution.contains(TypeHole(hole),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											| TypeVar(_) 		    => TYPE_VAR
											| ArithTypeVar(_)	    => ARITH_TYPE_VAR;
										    (* arith should never occur as matches above *)
				 val genType = TRecord(genFreshTRecord(Record.getLabels(r1),freshTypeVar,theta));
				 val theta1 = Substitution.union(theta,TypeHole(hole),genType)
			
			 in narrowExpr(e,genType,sigma,theta1,gamma) end
	
	| (List(eList),TList(listType)) =>
	
		let fun iterativeNarrowExpr(l,sigma,theta) = (case l of
		
			  [] => Config(Expression(List([])),sigma,theta)
			| e1::rest => (case narrowExpr(e1,listType,sigma,theta,gamma) of 
			
					  Config(Expression(e1narrow),sigma1,theta1) => (case iterativeNarrowExpr(rest,sigma1,theta1) of 
					  
						  Config(Expression(List(restNarrow)),sigma2,theta2) => Config(Expression(List(e1narrow::restNarrow)),sigma2,theta2)
						| _ => Config(Stuck,sigma,theta))
						
					| _ => Config(Stuck,sigma,theta)))

		in iterativeNarrowExpr(eList,sigma,theta) end
		
	| (List(_),THole(TypeHole(ArithTypeVar(_)))) => Config(Stuck,sigma,theta)
	
	(* For general type variable or equality type variable *)
	| (List(_),THole(TypeHole(hole))) =>
	
		if Substitution.contains(TypeHole(hole),theta)
		then narrowExpr(e,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											| TypeVar(_) 		    => TYPE_VAR
											| ArithTypeVar(_)	    => ARITH_TYPE_VAR;
										    (* arith should never occur as matches above *)
				 val genType = TList(generateFreshTypeVar(freshTypeVar,theta));
				 val theta1 = Substitution.union(theta,TypeHole(hole),genType)
				 
			 in narrowExpr(e,genType,sigma,theta1,gamma) end
	
	| (Condition(e1,e2,e3),t) => (case narrowExpr(e1,Bool,sigma,theta,gamma) of 
	
		  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
		| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,t,sigma1,theta1,gamma) of
		
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(e2narrow),sigma2,theta2) => (case narrowExpr(e3,t,sigma2,theta2,gamma) of
			
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				| Config(Expression(e3narrow),sigma3,theta3) => Config(Expression(Condition(e1narrow,e2narrow,e3narrow)),sigma3,theta3))))
	
	| (Variable(x),t) => 
		(* Assume x always in gamma 
		   To narrow a variable to type t, narrow the underlying expression it refers 
		   to to type t and return the variable x as the expression *)
		   
		if Substitution.contains(x,gamma)
		
		then (case narrowExpr(Substitution.get(x,gamma),t,sigma,theta,gamma) of
			
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			| Config(Expression(_),sigma1,theta1) => Config(Expression(Variable(x)),sigma1,theta1))
		
		else raise FreeVariable
	
	| (Case(e1,patExprList),t) => 
	
		let (* To make overall case expression capture avoiding, we do so 
			   on a pattern-expression pair by pair basis *)
			fun captureAvoiding(l) = (case l of 
			
			  [] => []
			  
			| (pat1,e1)::l1 => 
			
				let val dom = Substitution.domain(gamma);
					val fvRan = fv(Substitution.range(gamma));
					val fvPattern = fvPat(pat1)
					
				(* only change variable names for those clashing in gamma *)
				in (case union(listElement(dom,fvPattern),listElement(fvRan,fvPattern)) of
				
					  [] => (pat1,e1)::captureAvoiding(l1)
					  
					| l  => let val counter = getCounterAndUpdate() 
							(* Generate alpha-variant versions of the pattern & expression pair,
							   then re-call this method
							   Must pass same integer to pattern & expression alpha-variant methods *)
							in captureAvoiding((alphaPat(pat1,counter,l),alphaExpr(e1,counter,l))::l1) end)
				end)
	
			(* To get type we need to narrow e1 to, look at the form of patterns
			   - For a single pattern case, for all cases except records and lists, 
			     we narrow it to a fresh type variable
			     - For records we need to generate a record type (composed
			       recursively of further record & list types, and general type variables
			       in the base cases)
				 - For :: need to generate a list type, of the type generated
				   by recursively calling this function on the pattern before the :: 
				   (in case it is a list of records)
				 - For [] need to generate a (fresh) list type
			   - Now, for a list of patterns, we look through them all until
			   (i)   Record pattern found - return this type, via above analysis,
					 and stop looking through rest
			   (ii)  Cons pattern found - return list type, of type generated
			         by recursively calling this function on first pattern
			   (iii) Null pattern found. Look through the rest of patterns to see if
			         a record/cons pattern found (which will give a more specific type)
					 and if so use that, otherwise generate a (fresh) list type
			   (iv)  No record patterns found - return fresh type variable
                 We are okay to stop looking through the rest of the patterns 
				 as soon as a record pattern, null pattern or cons pattern found,
				 because if the following do not conform, it will fail in matchTypes *)
			fun e1NarrowType(patList) = (case patList of 
			
				(* Return first record type *)
				  PRecord(r)::_ => 
					let fun iterCalcNarrowType(r) = (case r of 
					  [] => []
					| (lab1,pat1)::r1 => (lab1,e1NarrowType([pat1]))::iterCalcNarrowType(r1))
					in TRecord(iterCalcNarrowType(r)) end
				
				| PVal(EmptyList)::l1 => (case e1NarrowType(l1) of 
				
					  t as TRecord(_) => t
					| t as TList(_)   => t
					| _               => TList(generateFreshTypeVar(TYPE_VAR,theta)))
				
				| PCons(pat1,_)::_ => TList(e1NarrowType([pat1]))
				
				(* Non-record/list type at head of list - move on to next element *)
				| _::l1 => e1NarrowType(l1)
					
				(* No items in pattern-expression pair list to check if record patterns exist 
				   Do not restrict to concrete types, e.g. PVal(N(_)) => Int,
				   because if we are narrowing a value hole, we don't want to narrow
				   it to a generated value from gen because, for example,
				   case v['a] of 3 -> ...
				   will get stuck since v['a] will be narrowed to 1
				   gen always returns 1 *)
				| [] => generateFreshTypeVar(TYPE_VAR,theta));
			
			(* Generates a list of patterns from the corresponding list of
			   pattern-expression pairs from the case expression
			   Used in e1NarrowType. I.e. drops expression part of pair *)
			fun getPatList(patExprList) = (case patExprList of 
				
				  [] => []
				| (pat1,_)::l1 => pat1::getPatList(l1))
							
			(* Takes a list of (expression,substitution) pairs, resulting from
			   the call to matchTypesList, and narrows each individual expression
			   to have the type t, passing sigma & theta in a left-to-right manner.
			   Returns a list of the narrowed expressions
			   Also takes the original (pattern,expression) pairs list,
			   so we can build up the new (pattern,narrowed_expression) pairs list by
			   copying the pattern (they are in same order, as nothing done to change it) *)
			fun iterNarrowExpressions(exprSubList,patExprList,sigma,theta) = (case (exprSubList,patExprList) of 
					
				  ([],[]) => SOME ([],sigma,theta)
				 
				(* Ignore old expression *)
				| ((e1,sub1)::l1,(pat1,_)::l2) => (case narrowExpr(e1,t,sigma,theta,sub1) of
				
					  Config(Stuck,_,_) => NONE
					| Config(Expression(e1narrow),sigma1,theta1) => (case iterNarrowExpressions(l1,l2,sigma1,theta1) of 
					
						  NONE => NONE
						| SOME (eList,sigma2,theta2) => SOME ((pat1,e1narrow)::eList,sigma2,theta2)))
						
				(* Shouldn't occur as narrowed-expression list and original pattern-expression list
				   will be of same length and match (in terms of correct pattern -> correct expression) *)
				| _ => NONE)
					
			val patExprList = captureAvoiding(patExprList)
								  
			in (case narrowExpr(e1,e1NarrowType(getPatList(patExprList)),sigma,theta,gamma) of
			
				  Config(Expression(e1narrow),sigma1,theta1) => (case typeofexpr(substitute(e1narrow,gamma),theta1) of 
				  
						  (NONE,theta1) => Config(Stuck,sigma,theta)
						| (SOME tNarrow,theta1) => (case matchTypesList(tNarrow,patExprList,gamma,theta1) of 
						
							  NONE => Config(Stuck,sigma,theta)
							| SOME (exprSubList,theta2) => (case iterNarrowExpressions(exprSubList,patExprList,sigma1,theta2) of 
							
								  NONE => Config(Stuck,sigma,theta)
								| SOME (narrowedEList,sigma3,theta3) => Config(Expression(Case(e1narrow,narrowedEList)),sigma3,theta3))))

				| _ => Config(Stuck,sigma,theta))
				
			end
	
	| (App(e1,e2),t) =>
	
		(* get types we must narrow e1 and e2 to in pair, respectively
		   look at types of e1 and e2
		   - if e1 already of some function type ta->tb, and e2 of some type tc
		     use the function type (if can unify t&tb and ta&tc)
		   - otherwise if e1 typeable to some type, use type tc->t to narrow e1
		     and type tc to narrow e2
		   - otherwise use fresh type 'a->t to narrow e1, and the same fresh type 
		     'a to narrow e2 *)
		   
		let val (narrowE1Type,narrowE2Type,theta1) = 
		
			let val (ta,theta1) = typeofexpr(substitute(e1,gamma),theta);
				val (tb,theta2) = typeofexpr(substitute(e2,gamma),theta1)
			
			in (case (ta,tb) of 
			
				  (SOME (TFun(type1,type2)), SOME type3) => (case unify([t,type2],theta2) of 
				
					  NONE => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
							  in (TFun(fresh,t),fresh,theta) end
					  
					| SOME theta3 => (case unify([type1,type3],theta3) of 
					
						  NONE => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
								  in (TFun(fresh,t),fresh,theta) end
						   
						| SOME theta4 => (TFun(resolveChainTheta(type1,theta4),resolveChainTheta(type2,theta4)),
										  resolveChainTheta(type3,theta4),
										  theta4)))
										  
				| (SOME _, SOME type2) => (TFun(type2,t),type2,theta2)
										  
				| _ => let val fresh = generateFreshTypeVar(TYPE_VAR,theta)
				       in (TFun(fresh,t),fresh,theta) end)
			
			end

		in (case narrowExpr(e1,narrowE1Type,sigma,theta1,gamma) of 
		
			  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
			  
			| Config(Expression(e1narrow),sigma2,theta2) => (case narrowExpr(e2,narrowE2Type,sigma2,theta2,gamma) of 
			
				  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
				
				| Config(Expression(e2narrow),sigma3,theta3) => Config(Expression(App(e1narrow,e2narrow)),sigma3,theta3)))
		
		end
		
	| (Let(x,tX,e1,e2),t) => 
	
		(* Make sure capture avoiding *)
		if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))
		
		(* If not, alpha-variant variable 'x' and expression 'e2', NOT expression 'e1'
		   Must pass same counter to 'x' and 'e2' *)
		then let val counter = getCounterAndUpdate()
			 in narrowExpr(Let(alphaVariable(x,counter,[x]),tX,e1,alphaExpr(e2,counter,[x])),t,sigma,theta,gamma) end
		
		else (case typeofexpr(substitute(e1,gamma),theta) of 
		
			  (NONE,_) => Config(Stuck,sigma,theta)
			| (SOME tE1,theta1) => (case unify([tE1,tX],theta1) of 
			
				  NONE => Config(Stuck,sigma,theta)
				| SOME theta2 => 
				
					(* To avoid free variable exception, 
					   add to gamma an arbitrary mapping for variable x, using gen instead of substituting *)
					let val tX = resolveChainTheta(tX,theta2)
					    val gamma1 = Substitution.union(gamma,x,Value(gen(tX,theta2)))
						
					(* Don't use gamma1 in narrowing of e1 *)
					in (case narrowExpr(e1,tX,sigma,theta2,gamma) of 
					
						  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
						| Config(Expression(e1narrow),sigma1,theta1) => (case narrowExpr(e2,t,sigma1,theta1,gamma1) of 
					
							  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
							| Config(Expression(e2narrow),sigma1,theta1) => Config(Expression(Let(x,tX,e1narrow,e2narrow)),sigma1,theta1)))
							
					end))
				
	(* (i)   unify t1 and t3
	   (ii)  unify type of e2 (after substituting with gamma, and arbitrary mappings for x and y) with t2
	   (iii) narrow e1 to be (latest value) of type t2
	   (iv)  narrow e2 to be type t *)
	| (LetRec(x,TFun(t1,t2),Fun(y,t3,e1),e2),t) =>
	
		(* Make sure x part capture avoiding: x binds in e2 and (fn y:t3=>e1) *)
		if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))
		
		then narrowExpr(alphaExpr(e,getCounterAndUpdate(),[x]),t,sigma,theta,gamma)
		
		else (* Make sure y part capture avoiding: y binds in e1 *)
			  if (element(Substitution.domain(gamma),y) orelse element(fv(Substitution.range(gamma)),y))
			  
			  (* Only alpha-variant the function value, not variable 'x' or expression 'e2': only x binds in e2, not y *)
			  then narrowExpr(LetRec(x,TFun(t1,t2),alphaValue(Fun(y,t3,e1),getCounterAndUpdate(),[y]),e2),t,sigma,theta,gamma)
			  
			  else (case unify([t1,t3],theta) of 
			 
					  NONE => Config(Stuck,sigma,theta)
					| SOME theta1 => 
					
						let (* Get latest values for t1 and t2 *)
							val t1 = resolveChainTheta(t1,theta1);
							val t2 = resolveChainTheta(t2,theta1);
							
							(* Add arbitrary mapping for x and y to gamma for e1 *)
							val gamma1 = Substitution.union(Substitution.union(gamma,x,Value(gen(TFun(t1,t2),theta1))),
															y,Value(gen(t1,theta1)))
															
						in (case typeofexpr(substitute(e1,gamma1),theta1) of
						
							  (NONE,_) => Config(Stuck,sigma,theta)
							| (SOME tE1,theta2) => (case unify([tE1,t2],theta2) of 
							
								  NONE => Config(Stuck,sigma,theta)
								| SOME theta3 => 
								
									let (* Get latest values for t1 and t2 *)
										val t1 = resolveChainTheta(t1,theta3);
										val t2 = resolveChainTheta(t2,theta3);
						
										(* Add arbitrary mapping for x and y to gamma for e1 *)
										val gamma1 = Substitution.union(Substitution.union(gamma,x,Value(gen(TFun(t1,t2),theta3))),
																		y,Value(gen(t1,theta3)));
																		
										(* Add arbitrary mapping for x to gamma for e2 *)
										val gamma2 = Substitution.union(gamma,x,Value(gen(TFun(t1,t2),theta3)))
										
									in (case narrowExpr(e1,t2,sigma,theta3,gamma1) of 
									
										  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
										| Config(Expression(e1narrow),sigma1,theta1) =>  (case narrowExpr(e2,t,sigma1,theta1,gamma2) of 
										
											  Config(Stuck,_,_) => Config(Stuck,sigma,theta)
											| Config(Expression(e2narrow),sigma2,theta2) => 
												
												Config(Expression(LetRec(x,TFun(t1,t2),Fun(y,t1,e1narrow),e2narrow)),sigma2,theta2)))
									
									end))
									
							end)
		
	| _ => Config(Stuck,sigma,theta))
	
