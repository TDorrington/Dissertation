(* Matches a value's type against a pattern
   Returns variable substitution (in an option data type) and type hole substitution *)
fun matchTypes(t,pat,gamma,theta) = (case pat of 

	  PWildcard => SOME (gamma,theta)
	
	| PVar(x) => if Substitution.contains(x,gamma)
				 then NONE (* x already bound in match *)
				 else SOME (Substitution.union(gamma,x,Value(gen(t,theta))),theta)
	
	 (* real constants cannot occur in patterns *)
	| PVal(R(_)) => NONE
		  
	| PVal(N(_)) => (case unify([t,Int],theta) of 
		
		  NONE        => NONE
		| SOME theta1 => SOME (gamma,theta1))
		
	| PVal(B(_)) => (case unify([t,Bool],theta) of 
		
		  NONE        => NONE
		| SOME theta1 => SOME (gamma,theta1))
	
	| PRecord(r) => (case t of 
		(* Same idea as typeof code *)
		
		  TRecord(r1) =>
			let fun matchLists(l,gamma,theta) = (case l of 
			
					  [] => SOME (gamma,theta)
					| (t1,pat1)::l1 => (case matchTypes(t1,pat1,gamma,theta) of 
									  
						  NONE => NONE
						| SOME (gamma1,theta1) => (case matchLists(l1,gamma1,theta1) of 
									 
								NONE => NONE
							  | SOME (gamma2,theta2) => SOME (gamma2,theta2))))
			
			in (case Record.merge(r1,r) of 
				  NONE   => NONE
				| SOME l => matchLists(l,gamma,theta))
			end
		  
		| THole(TypeHole(ArithTypeVar(_))) => NONE
		
		| THole(TypeHole(tyvar)) =>
			let val typevar_type = case tyvar of 
					  TypeVar(_)    	 => TYPE_VAR
					| EqualityTypeVar(_) => EQUALITY_TYPE_VAR
					| ArithTypeVar(_)  	 => ARITH_TYPE_VAR;
					(* arith cannot occur - matched above, but for non-exhaustive warnings *)
				val genType = genFreshTRecord(Record.getLabels(r),typevar_type,theta);
				val theta1 = Substitution.union(theta,TypeHole(tyvar),genType);
			in matchTypes(genType,pat,gamma,theta1) end
		
		| _ => NONE));
		
(* ----------------------------------------------------------------------------- *)   
(* Datatype returned from match function 
	- Fail means we could not match, e.g. match(3,4)
	- Hole means we cannot match only with the current information, so this is an indicator 
	  to the returning code that the case expression containing the match should
	  be left as a case hole 
	- Success means we can match the expression and pattern, and returns the 
	  relevant substitutions *)
datatype matchResult = Fail
					 | Hole of valhole
					 | Success of valSub * typeSub * variableSub;
					 
(* Matches an expression and a pattern
   Returns matchResult datatype instance *)
fun match(e,pat,sigma,theta,gamma) = (case pat of 

	  PWildcard => Success (sigma,theta,gamma)
	
	| PVar(x) => if Substitution.contains(x,gamma)
				 then Fail (* x already bound in match *)
				 else Success (sigma,theta,Substitution.union(gamma,x,e))
	
	 (* real constants cannot occur in patterns *)
	| PVal(R(_)) => Fail
		  
	| PVal(N(n1)) => (case e of
	
		  Value(Concrete(N(n2))) => if n1=n2 then Success (sigma,theta,gamma) else Fail
		
		| Value(VHole(SimpleHole(ValueHole(tyvar)))) => (case unify([THole(TypeHole(tyvar)),Int],theta) of 
		
			  NONE => Fail
			| SOME theta1 => Success (Substitution.union(sigma,ValueHole(tyvar),Concrete(N(n1))),theta1,gamma))
		
		(* Any other compound value hole *)
		| Value(VHole(h)) => Hole h
	
		(* Expressions cannot occur - will all be values
		   For example, match(3+2,5) cannot be a case
		   Also, variables will be substituted for *)
		| _ => Fail)
		
	| PVal(B(b1)) => (case e of
	
		  Value(Concrete(B(b2))) => if b1=b2 then Success (sigma,theta,gamma) else Fail
		
		| Value(VHole(SimpleHole(ValueHole(tyvar)))) => (case unify([THole(TypeHole(tyvar)),Bool],theta) of 
		
			  NONE => Fail
			| SOME theta1 => Success (Substitution.union(sigma,ValueHole(tyvar),Concrete(B(b1))),theta1,gamma))
		
		(* Any other compound value hole *)
		| Value(VHole(h)) => Hole h
		  
		(* Expressions cannot occur - will all be values
		   For example, match(3=2,true) cannot be a case
		   Also, all variables will be substituted for *)
		| _ => Fail)
	
	| PRecord(r) => (case e of 
		
		(* Takes an argument of the (v,pat) pairs matched up to this point in the recursive
		   calls of this function in (lab,v) form 
		    Used in case we need to generate the value hole of the record to return
			Processed used in case we need to generate value hole version of input record *)
		  Value(VRecord(r1)) =>
			let fun matchLists(l1,l2,sigma,theta,gamma,processed) = (case (l1,l2) of 
			
					  ([],[]) => Success (sigma,theta,gamma)
					| ([],_)  => Fail
					| (_,[])  => Fail
					| ((labv,v1)::rest1,(labp,pat1)::rest2) => 
					
						if labv=labp 
						then (case match(Value(v1),pat1,sigma,theta,gamma) of 
									  
						  Fail   => Fail

						(* Even though we return a hole, keep looking through rest of list
						   to check none of them fail *)
						| Hole h => (case matchLists(rest1,rest2,sigma,theta,gamma,processed) of 
							
							  Fail => Fail
							| _    => Hole (RecordHole((labv,VHole(h))::rest1)))
							
						| Success (sigma1,theta1,gamma1) => (case matchLists(rest1,rest2,sigma1,theta1,gamma1,processed) of 
						
							  Hole (RecordHole(r)) => Hole (RecordHole(append((labv,v1)::processed,r)))
							| s as Success(sigma2,theta2,gamma2) => s
							| _ => Fail))
							
						else Fail)
			
			in matchLists(Record.sort(r1),Record.sort(r),sigma,theta,gamma,[]) end
		  
		| Record(r1) => 
		
			let fun matchLists(l1,l2,sigma,theta,gamma,processed) = (case (l1,l2) of 
			
					  ([],[]) => Success (sigma,theta,gamma)
					| ([],_)  => Fail
					| (_,[])  => Fail
					| ((labe,e1)::rest1,(labp,pat1)::rest2) => 
					
						if labe=labp 
						then (case match(e1,pat1,sigma,theta,gamma) of 
									  
						  Fail   => Fail
						  
						| Hole _ => Fail
							
						| Success (sigma1,theta1,gamma1) => (case matchLists(rest1,rest2,sigma1,theta1,gamma1,processed) of 
						
							  s as Success(sigma2,theta2,gamma2) => s
							| _ => Fail))
							
						else Fail)
						
			in matchLists(Record.sort(r1),Record.sort(r),sigma,theta,gamma,[]) end
		  
		  
		| Value(VHole(SimpleHole(ValueHole(ArithTypeVar(_))))) =>  Fail
		
		| Value(VHole(SimpleHole(ValueHole(tyvar)))) =>
			let val tyvar_type = case tyvar of 
					  TypeVar(_)    	 => TYPE_VAR
					| EqualityTypeVar(_) => EQUALITY_TYPE_VAR
					| ArithTypeVar(_)  	 => ARITH_TYPE_VAR;
					(* arith cannot occur - matched above, but here for non-exhaustive warnings *)
				val genType = genFreshTRecord(Record.getLabels(r),tyvar_type,theta);
				val genVal = gen(genType,theta);
				val theta1 = Substitution.union(theta,TypeHole(tyvar),genType);
				val sigma1 = Substitution.union(sigma,ValueHole(tyvar),genVal)
			in match(Value(genVal),pat,sigma1,theta1,gamma) end
		
		| Value(VHole(h)) => Hole h
		
		| _ => Fail));
		