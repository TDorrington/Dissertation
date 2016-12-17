(* narrow dynamically performs type-checking 
    takes a value v, type t and current substitutions and
    refines v to have type t, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta) = 

let fun localNarrow(v,t,sigma,theta,gamma) = (case (v,t) of

	  (Concrete(N(integer)),Int) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (Concrete(N(integer)),THole(hole)) => (case unify([t,Int],theta) of 
	      NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
		
	| (Concrete(B(boolean)),Bool) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (Concrete(B(boolean)),THole(hole)) => (case unify([t,Bool],theta) of 
		  NONE  	  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
	
	| (Concrete(R(real)),Real) => (Config(Expression(Value(v)), sigma, theta), gamma)
	| (Concrete(R(real)),THole(hole)) => (case unify([t,Real],theta) of 
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => (Config(Expression(Value(v)),sigma,theta1),gamma))
	
	| (VRecord(r1),TRecord(r2)) => 
		(* Don't try and bother putting it into value records since it requires 
		   ALL individual narrowed values to be returned as values, not expressions *)
	
		let fun iterativeNarrow(l1,l2,s,t,g) = (case (l1,l2) of 
		
			  ([],[]) => (Config(Expression(Record([])),s,t),g)
			| ([],_)  => (Config(Stuck,sigma,theta),gamma)
			| (_,[])  => (Config(Stuck,sigma,theta),gamma)
			| ((labv,v1)::rest1,(labt,t1)::rest2) => 
			
				if labv=labt 
				
				then (case localNarrow(v1,t1,s,t,g) of 
			
					  (Config(Expression(v1narrow),s1,t1),g1) => (case iterativeNarrow(rest1,rest2,s1,t1,g1) of 
					  
						  (Config(Expression(Record(r)),s2,t2),g2) => 
							(Config(Expression(Record((labv,v1narrow)::r)),s2,t2),g2)
							
						| _ => (Config(Stuck,sigma,theta),gamma))
						
					| _ => (Config(Stuck,sigma,theta),gamma))
					
				else (Config(Stuck,sigma,theta),gamma))
		
		in iterativeNarrow(Record.sort(r1),Record.sort(r2),sigma,theta,gamma) end
					
	(* For narrowing a record to a type hole,
	   narrow({lab1=t1,...,labn=tn},'a)   => narrow({lab1='a1,...,labn='an},{lab1=t1,...,labn=tn})
	   narrow({lab1=t1,...,labn=tn},''a)  => narrow({lab1=t1,...,labn=tn},{lab1=''a1,...,labn=''an})
	   narrow({lab1=t1,...,labn=tn},'''a) => FAIL
	   for fresh 'a1,''a1,...,'an,''an *)
					
	| (VRecord(_),THole(TypeHole(ArithTypeVar(_)))) => (Config(Stuck,sigma,theta),gamma)
	
	| (VRecord(r),THole(TypeHole(hole))) =>
		
		if Substitution.contains(TypeHole(hole),theta)
		then localNarrow(v,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										  | TypeVar(_) 		   => TYPE_VAR
										  | ArithTypeVar(_)	   => ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
				val genType = genFreshTRecord(Record.getLabels(r),freshTypeVar,theta);
				val theta1 = Substitution.union(theta,TypeHole(hole),genType)
			
			in localNarrow(v,genType,sigma,theta1,gamma) end
	
	| (Fun(x,t,e),TFun(t1,t2)) =>

		(* perform capture avoiding substitution *)
		if (element(Substitution.domain(gamma),x) orelse element(fv(Substitution.range(gamma)),x))

		then localNarrowExpr(alphaVariant(Value(v),getCounterAndUpdate(),[x]),TFun(t1,t2),sigma,theta,gamma)
		
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
			
				(* Remove the generated value from x
				   This allows us to pass gammas in a left-to-right manner to keep the updated substitutions *)				  
				(Config(Expression(Value(Fun(x,resolveChainTheta(t,theta2),enarrow))),sigma1,theta2),Substitution.remove(gamma2,x)))
				
			end)
				
	| (Fun(_,t1,_),THole(TypeHole(TypeVar(a)))) =>
	
		if Substitution.contains(TypeHole(TypeVar(a)),theta)
		then localNarrow(v,resolveChainTheta(THole(TypeHole(TypeVar(a))),theta),sigma,theta,gamma)
		
		else let val freshType = generateFreshTypeVar(TYPE_VAR,theta)
			 in (case unify([t,TFun(t1,freshType)],theta) of 
				(* unify to prevent type variable a being an element
				   of the free type variables of t1 *)
			   
				   NONE => (Config(Stuck,sigma,theta),gamma)
				 | SOME theta1 => localNarrow(v,TFun(t1,freshType),sigma,theta1,gamma))
			 end
	
	| (VHole(SimpleHole(ValueHole(hole))),t) =>
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(hole), sigma) 
		
		then let val v = resolveChainSigma(v,sigma)
			 in (case typeofexpr(substitute(Value(v),gamma),theta) of
				
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
				
	| (VHole(BinaryOpHole(ArithOper(oper),v1,v2)),t) =>
	  localNarrowExpr(ArithExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
		
	| (VHole(BinaryOpHole(BoolOper(oper),v1,v2)),t) =>
	  localNarrowExpr(BoolExpr(oper,Value(v1),Value(v2)),t,sigma,theta,gamma)
	
	| (VHole(CaseHole(v1,pat,e)),t) =>
	   localNarrowExpr(Case(Value(v1),pat,e),t,sigma,theta,gamma)
	
	| (VHole(ConditionHole(v1,e1,e2)),t) =>
	   localNarrowExpr(Condition(Value(v1),e1,e2),t,sigma,theta,gamma)
	   
	| (VHole(AppHole(v1,v2)),t) =>
		localNarrowExpr(App(Value(v1),Value(v2)),t,sigma,theta,gamma)
	
	| (VHole(RecordHole(r)),t) =>
		let fun valToERecord(r) = (case r of 
		  []		    => []
		| (lab1,v1)::r1 => (lab1,Value(v1))::valToERecord(r1))
		in localNarrowExpr(Record(valToERecord(r)),t,sigma,theta,gamma) end
	
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
			| (Config(Expression(eNew),sigma1,theta1),gamma1) => 
				(Config(Expression(Variable(x)),sigma1,theta1),Substitution.update(gamma1,x,eNew)))
		
		else raise FreeVariable
		
	(* e1/e2 must be of type Real (if concrete type given) since /:real*real->real *)
	| (ArithExpr(DIVIDE,e1,e2),Real) => (case localNarrowExpr(e1,Real,sigma,theta,gamma) of
	
	      (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,Real,sigma1,theta1,gamma1) of
			
		    (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		  | (Config(Expression(e2narrow),sigma2,theta2),gamma2) => 
				
			  (Config(Expression(ArithExpr(DIVIDE,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
	
	(* can also localNarrow e1/e2 to a type variable *)
	| (ArithExpr(DIVIDE,e1,e2),THole(hole)) => (case unify([t,Real],theta) of 
	
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => localNarrowExpr(e,Real,sigma,theta1,gamma))
	
	(* cannot localNarrow e1/e2 to anything else
	   needed so doesn't match arith expr clause below using wildcard *)
	| (ArithExpr(DIVIDE,_,_),_) => (Config(Stuck,sigma,theta),gamma)
	
	(* For op +,-,*
       t can only be of type Int, Real or an arithmetic type variable 
	   if t an equality type variable, can only be Int
	   if t a general type variable, can be narrowed to an arithmetic type variable *)
	| (ArithExpr(oper,e1,e2),t) => (case t of 
	
		  Bool => (Config(Stuck,sigma,theta),gamma)
		| TRecord(_) => (Config(Stuck,sigma,theta),gamma)
		| TFun(_,_) => (Config(Stuck,sigma,theta),gamma)
		
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
						
						| (TRecord(r1),TRecord(r2)) => (case iterCalcType(Record.sort(r1),Record.sort(r2)) of 
							  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
							| SOME l => TRecord(l))
							
						| (TRecord(r1),_) =>
							let val TRecord(r2) = genFreshTRecord(Record.getLabels(r1),EQUALITY_TYPE_VAR,theta2)
							in (case iterCalcType(Record.sort(r1),Record.sort(r2)) of
								  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
								| SOME l => TRecord(l))
							end
							
						| (_,TRecord(r2)) =>
							let val TRecord(r1) = genFreshTRecord(Record.getLabels(r2),EQUALITY_TYPE_VAR,theta2)
							in (case iterCalcType(Record.sort(r1),Record.sort(r2)) of 
								  NONE => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
								| SOME l => TRecord(l))
							end
							
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
					
		in (case localNarrowExpr(e1,narrowType,sigma,theta,gamma) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,narrowType,sigma1,theta1,gamma1) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e2narrow),sigma2,theta2),gamma2) =>
				
					(Config(Expression(BoolExpr(EQ,e1narrow,e2narrow)),sigma2,theta2),gamma2)))
		end
	
	(* can also localNarrow e1=e2 to a general type variable *)
	| (BoolExpr(EQ,e1,e2),THole(hole)) => (case unify([t,Bool],theta) of 
	
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
	| (BoolExpr(oper,e1,e2),THole(hole)) => (case unify([t,Bool],theta) of
	
		  NONE 		  => (Config(Stuck,sigma,theta),gamma)
		| SOME theta1 => localNarrowExpr(e,Bool,sigma,theta1,gamma))
					
	(* For narrowing a record to a type hole,
	   narrow({lab1=t1,...,labn=tn},'a)   => narrow({lab1='a1,...,labn='an},{lab1=t1,...,labn=tn})
	   narrow({lab1=t1,...,labn=tn},''a)  => narrow({lab1=t1,...,labn=tn},{lab1=''a1,...,labn=''an})
	   narrow({lab1=t1,...,labn=tn},'''a) => FAIL
	   for fresh 'a1,''a1,...,'an,''an *)
					
	| (Record(r1),TRecord(r2)) => 
	
		let fun iterativeNarrowExpr(l1,l2,s,t,g) = (case (l1,l2) of 
		
			  ([],[]) => (Config(Expression(Record([])),s,t),g)
			| ([],_)  => (Config(Stuck,sigma,theta),gamma)
			| (_,[])  => (Config(Stuck,sigma,theta),gamma)
			| ((labe,e1)::rest1,(labt,t1)::rest2) =>
			
				if labe = labt 
				
				then (case localNarrowExpr(e1,t1,s,t,g) of 
			
					  (Config(Expression(e1narrow),s1,t1),g1) => (case iterativeNarrowExpr(rest1,rest2,s1,t1,g1) of 
					  
						  (Config(Expression(Record(r)),s2,t2),g2) => 
							(Config(Expression(Record((labe,e1narrow)::r)),s2,t2),g2)
							
						| _ => (Config(Stuck,sigma,theta),gamma))
						
					| _ => (Config(Stuck,sigma,theta),gamma))
				
				else (Config(Stuck,sigma,theta),gamma))
		
		in iterativeNarrowExpr(Record.sort(r1),Record.sort(r2),sigma,theta,gamma) end
	
	| (Record(_),THole(TypeHole(ArithTypeVar(_)))) => (Config(Stuck,sigma,theta),gamma)
	
	(* For general type variable or equality type variable *)
	| (Record(r1),THole(TypeHole(hole))) =>
	
		if Substitution.contains(TypeHole(hole),theta)
		then localNarrowExpr(e,resolveChainTheta(THole(TypeHole(hole)),theta),sigma,theta,gamma)
		
		else let val freshTypeVar = case hole of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											| TypeVar(_) 		    => TYPE_VAR
											| ArithTypeVar(_)	    => ARITH_TYPE_VAR;
										    (* arith should never occur as matches above *)
				val genType = genFreshTRecord(Record.getLabels(r1),freshTypeVar,theta);
				val theta1 = Substitution.union(theta,TypeHole(hole),genType)
			
		in localNarrowExpr(e,genType,sigma,theta1,gamma) end
	
	| (Condition(e1,e2,e3),t) => (case localNarrowExpr(e1,Bool,sigma,theta,gamma) of 
	
		  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
		| (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case localNarrowExpr(e2,t,sigma1,theta1,gamma1) of
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			| (Config(Expression(e2narrow),sigma2,theta2),gamma2) => (case localNarrowExpr(e3,t,sigma2,theta2,gamma2) of
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				| (Config(Expression(e3narrow),sigma3,theta3),gamma3) => 
				
					(Config(Expression(Condition(e1narrow,e2narrow,e3narrow)),sigma3,theta3),gamma3))))
	
	| (Case(e1,pat,e2),t) => 
	
		(* perform capture avoiding substitution *)
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma));
			val fvPattern = fvPat(pat)
		(* only change variable names for those clashing in gamma *)
		in (case union(listElement(dom,fvPattern), listElement(fvRan,fvPattern)) of
		
			(* non-empty list of clashing variables *)
			  a::l => localNarrowExpr(alphaVariant(e,getCounterAndUpdate(),a::l),t,sigma,theta,gamma)
			  
			(* empty list of clashing variables: capture avoiding *)
			| [] => 
			
			(* To get type we need to narrow e1 to, look at form of pattern *)
			let fun e1NarrowType(pat) = (case pat of 
			
				  PRecord(r)   => 
					let fun iterCalcNarrowType(r) = (case r of 
					  [] => []
					| (lab1,pat1)::r1 => (lab1,e1NarrowType(pat1))::iterCalcNarrowType(r1))
					in TRecord(iterCalcNarrowType(r)) end
					
				(* Do not restrict to concrete types, e.g. PVal(N(_)) => Int,
				   because if we are narrowing a value hole, we don't want to narrow
				   it to a generated value from gen because, for example,
				   case v['a] of 3 -> ...
				   will get stuck since v['a] will be narrowed to 1
				   gen always returns 1 *)
				| _ => generateFreshTypeVar(TYPE_VAR,theta));
				
			(* Updates e1, after e2 has been narrowed
			   Only need to update e1 if it matched a variable, in which case we get 
			   the (update) variable substitution from gamma
			   Some of the cases cannot occur (e.g. label set not being identical in records)
			   since they will have had to of matched to get to this point of calling this function,
			   but we include for exhaustive matching anyway *)
				fun updateE1(e,pat,gamma) = (case pat of 
				
				  PWildcard   => SOME e
				| PVal(_)     => SOME e

				| PVar(x)     => (case e of 
					  Variable(_) => SOME e
					| _ 		  => SOME (Substitution.get(x,gamma)))
	
				| PRecord(r2) => (case e of 
					(* For record patterns, we allow e to be 
					   - an expression record,
					   - a value record, 
					   - a variable, in which case we do not update the variable *)
				
					  Record(r1) => (case (Record.sort(r1),Record.sort(r2)) of 
					  
						  ([],[]) => SOME (Record([]))
						| ([],_)  => NONE
						| (_,[])  => NONE
						| ((lab1,e1)::l1,(lab2,pat2)::l2) => 
						
							if lab1=lab2 
							then (case updateE1(e1,pat2,gamma) of 
						
								  NONE => NONE
								| SOME newE1 => (case updateE1(Record(l1),PRecord(l2),gamma) of 
								
									  SOME (Record(newL1)) => SOME (Record((lab1,newE1)::newL1))
									| _ => NONE))
							
							else NONE)
						
					| Value(VRecord(r1)) => (case (Record.sort(r1),Record.sort(r2)) of 
					  (* don't try and wrap back into a value record, because that requires
					     all the individually updated fields to be returned as values
						 return as an expression record *)
					  
						  ([],[]) => SOME (Record([]))
						| ([],_)  => NONE
						| (_,[])  => NONE
						| ((lab1,v1)::l1,(lab2,pat2)::l2) => 
						
							if lab1=lab2 
							then (case updateE1(Value(v1),pat2,gamma) of 
						
								  NONE => NONE
								| SOME newV1 => (case updateE1(Value(VRecord(l1)),PRecord(l2),gamma) of 
								
									  SOME (Record(newL1)) => SOME (Record((lab1,newV1)::newL1))
									| _ => NONE))
							
							else NONE)
					
					| Variable(_) => SOME e
					
					| _ => NONE))
					  			
			in (case localNarrowExpr(e1,e1NarrowType(pat),sigma,theta,gamma) of

				  (Config(Expression(e1narrow),sigma1,theta1),gamma1) => (case typeofexpr(substitute(e1narrow,gamma),theta) of 
				  
						  (NONE,theta1) => (Config(Stuck,sigma,theta),gamma)
						| (SOME tNarrow,theta1) => (case matchTypes(tNarrow,pat,gamma1,theta1) of 
						
							  NONE => (Config(Stuck,sigma,theta),gamma)
							| SOME (gamma2,theta2) => (case localNarrowExpr(e2,t,sigma1,theta2,gamma2) of 
							
								  (Config(Expression(e2narrow),sigma3,theta3),gamma3) => (case updateE1(e1narrow,pat,gamma3) of 
								  
									  SOME e => (Config(Expression(Case(e,pat,e2narrow)),sigma3,theta3),gamma3)
									| NONE => (Config(Stuck,sigma,theta),gamma))
									
								| _ => (Config(Stuck,sigma,theta),gamma))))
					
				| _ => (Config(Stuck,sigma,theta),gamma))
				
			end)
				
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

		in (case localNarrowExpr(e1,narrowE1Type,sigma,theta1,gamma) of 
		
			  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
			  
			| (Config(Expression(e1narrow),sigma2,theta2),gamma2) => (case localNarrowExpr(e2,narrowE2Type,sigma2,theta2,gamma2) of 
			
				  (Config(Stuck,_,_),_) => (Config(Stuck,sigma,theta),gamma)
				
				| (Config(Expression(e2narrow),sigma3,theta3),gamma3) => 
				
					(Config(Expression(App(e1narrow,e2narrow)),sigma3,theta3),gamma3)))
		
		end
		
	| _ => (Config(Stuck,sigma,theta),gamma))
	
in 
	let val (config,_) = localNarrow(v,t,sigma,theta,[]) 
	in config end
end;
