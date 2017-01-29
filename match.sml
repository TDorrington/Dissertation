(* Matches a value's type against a pattern
   Returns variable substitution (in an option data type) and type hole substitution
   as well as the expression that matched which we need to calculate the resulting type of *)
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
	
	(* t must be a list type *)
	| PVal(EmptyList) => (case unify([t,TList(generateFreshTypeVar(TYPE_VAR,theta))],theta) of 
	
		  NONE        => NONE
		| SOME theta1 => SOME (gamma,theta1))
	
	(* t must be a list type, say TList(t)
	   Then recursively match pat1 to type t, and pat2 to type TList(t) *)
	| PCons(pat1,pat2) => (case unify([t,TList(generateFreshTypeVar(TYPE_VAR,theta))],theta) of 
	
		  NONE 		  => NONE
		| SOME theta1 => (case resolveChainTheta(t,theta1) of 
		
			  TList(list_type) => (case matchTypes(list_type,pat1,gamma,theta1) of 
			  
				  NONE                 => NONE
				| SOME (gamma1,theta1) => matchTypes(TList(list_type),pat2,gamma1,theta1))
			  
			| _ => NONE))
	
	| PRecord(r) => (case t of 
		
		  TRecord(r1) =>
			let fun matchLists(l,gamma,theta) = (case l of 
			
					  []            => SOME (gamma,theta)
					| (t1,pat1)::l1 => (case matchTypes(t1,pat1,gamma,theta) of 
									  
						  NONE                 => NONE
						| SOME (gamma1,theta1) => matchLists(l1,gamma1,theta1)))
			
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
				val genType = TRecord(genFreshTRecord(Record.getLabels(r),typevar_type,theta));
				val theta1 = Substitution.union(theta,TypeHole(tyvar),genType);
			in matchTypes(genType,pat,gamma,theta1) end
		
		| _ => NONE));
	
(* Matches a single type against a list of patterns,
   i.e. matches that type against each pattern individually
   Used in typeof to calculate type of case expression
   For 'case e of pat1=>e1 | pat2=>e1 | ... | patn=>en'
   Calculate type of e, say t
   Then we call matchTypesList(t,[(pat1,e1),...,(patn,en)])
   and iteratively called matchTypes(t,pati)
   We return (if successful) a list of (e_i,sub_i) pairs, where 
   expression e_i =ei, and substitution sub_i = result of matchTypes(t,pati) *)
fun matchTypesList(t,patExprList,gamma,theta) = (case patExprList of 

	  []  			=> SOME ([],theta)
	| (pat1,e1)::l1 => (case matchTypes(t,pat1,gamma,theta) of 
	
		  NONE 				 => NONE
		| SOME (sub1,theta1) => (case matchTypesList(t,l1,gamma,theta1) of 
		
			  NONE 						=> NONE
			| SOME (exprSubList,theta2) => SOME ((e1,sub1)::exprSubList,theta2))));
	
(* ----------------------------------------------------------------------------- *)   
(* Datatype returned from match function 
	- Fail means we could not match, e.g. match(3,4)
	- Hole means we cannot match only with the current information, so this is an indicator 
	  to the returning code that the case expression containing the match should
	  be left as a case hole 
	- Success means we can match the value and pattern, and returns the 
	  relevant substitutions, as well as the expression associated with the
	  matching pattern *)
datatype matchResult = Fail
					 | Hole of valhole
					 | Success of e * valSub * typeSub * variableSub;
					 
(* Matches a value and a list of (pattern-expressions), returning the first
   such match in the list of patterns 
   Returns matchResult datatype instance
   Match will always return the first such match, and will sometimes incorrectly
   match types, e.g. case 3 of 3 -> 0 | {} -> 1, will match, but in reality
   it should not since {} is not of type int
   i.e. when invalid pattern form comes *after* the pattern which matches the expression,
   it will incorrectly match
   This is 'okay', however, since any calls to match from evaluate
   and always preceded by calls to matchTypesList above to check pattern & expression
   we are case-ing on are first of the correct type
   We only match against a value because expressions cannot occur due to contexts &
   evaluation order in the evaluate function *)
   
fun match(v,patExprList,sigma,theta,gamma) = (case patExprList of 

	  (PWildcard,expr)::_ => Success (expr,sigma,theta,gamma)
	
	| (PVar(x),expr)::_ => if Substitution.contains(x,gamma)
						   then Fail (* x already bound in match *)
						   else Success (expr,sigma,theta,Substitution.union(gamma,x,Value(v)))
	
	 (* real constants cannot occur in patterns, regardless what's left *)
	| (PVal(R(_)),_)::_ => Fail
		  
	| (PVal(N(n1)),expr)::rest => (case v of
	
		  Concrete(N(n2)) => if n1=n2 
							 then Success (expr,sigma,theta,gamma) 
							 else match(v,rest,sigma,theta,gamma)
		
		| VHole(SimpleHole(ValueHole(tyvar))) => (case unify([THole(TypeHole(tyvar)),Int],theta) of 
		
			  NONE        => Fail
			| SOME theta1 => Success (expr,Substitution.union(sigma,ValueHole(tyvar),Concrete(N(n1))),theta1,gamma))
		
		| VHole(h) => Hole h
		
		| _ => Fail)
		
	| (PVal(B(b1)),expr)::rest => (case v of
	
		  Concrete(B(b2)) => if b1=b2 
							 then Success (expr,sigma,theta,gamma) 
							 else match(v,rest,sigma,theta,gamma)
		
		| VHole(SimpleHole(ValueHole(tyvar))) => (case unify([THole(TypeHole(tyvar)),Bool],theta) of 
		
			  NONE        => Fail
			| SOME theta1 => Success (expr,Substitution.union(sigma,ValueHole(tyvar),Concrete(B(b1))),theta1,gamma))
		
		| VHole(h) => Hole h
		  
		| _ => Fail)
	
	| (PVal(EmptyList),expr)::rest => (case v of 
	
		  Concrete(EmptyList) => Success(expr,sigma,theta,gamma)
		  
		| VList(_) => match(v,rest,sigma,theta,gamma)
		
		| VHole(SimpleHole(ValueHole(tyvar))) => (case unify([THole(TypeHole(tyvar)),TList(generateFreshTypeVar(TYPE_VAR,theta))],theta) of 
		
			  NONE        => Fail
			| SOME theta1 => Success(expr,Substitution.union(sigma,ValueHole(tyvar),Concrete(EmptyList)),theta1,gamma))
			
		| VHole(h) => Hole h
		
		| _ => Fail)
	
	| (PCons(pat1,pat2),expr)::rest => (case v of 
	
		  Concrete(EmptyList) => match(v,rest,sigma,theta,gamma)
		
		(* Associate arbitrary expressions with recursive calls to pat *)
		| VList(v1::rest1) => (case match(v1,[(pat1,expr)],sigma,theta,gamma) of 
		
			  Fail => match(v,rest,sigma,theta,gamma)
			  
			(* Even though we return a hole, match rest1 and pat2 to check it doesn't fail
			   First check rest1 not empty to avoiding introducing VList([]) *)
			| Hole h => (case rest1 of 
			
				  [] => (case match(Concrete(EmptyList),[(pat2,expr)],sigma,theta,gamma) of 
								
					  Fail => Fail
					| _    => Hole (ListHole(VHole(h)::rest1)))
				  
				| _ => (case match(VList(rest1),[(pat2,expr)],sigma,theta,gamma) of 
								
					  Fail => Fail
					| _    => Hole (ListHole(VHole(h)::rest1))))
				
			| Success(_,sigma1,theta1,gamma1) => (case rest1 of 
			
				  (* Avoid introducing empty VList([]) *)
				  [] => (case match(Concrete(EmptyList),[(pat2,expr)],sigma1,theta1,gamma1) of 
			
					  Hole (ListHole(rest)) => Hole (ListHole(v1::rest))
					| s as Success(_,_,_,_) => s
					| _                     => match(v,rest,sigma,theta,gamma))
					
				| _ => (case match(VList(rest1),[(pat2,expr)],sigma1,theta1,gamma1) of 
			
					  Hole (ListHole(rest)) => Hole (ListHole(v1::rest))
					| s as Success(_,_,_,_) => s
					| _                     => match(v,rest,sigma,theta,gamma))))
			
		| VHole(SimpleHole(ValueHole(ArithTypeVar(_)))) =>  Fail
		
		| VHole(SimpleHole(ValueHole(tyvar))) =>
		
			let val tyvar_type = case tyvar of TypeVar(_)    => TYPE_VAR
										| EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										| ArithTypeVar(_)  	 => ARITH_TYPE_VAR;
										(* arith cannot occur - matched above *)
				val genType = TList(generateFreshTypeVar(tyvar_type,theta));
				val genVal = gen(genType,theta);
				val theta1 = Substitution.union(theta,TypeHole(tyvar),genType);
				val sigma1 = Substitution.union(sigma,ValueHole(tyvar),genVal)
			in match(genVal,patExprList,sigma1,theta1,gamma) end
		
		| VHole(h) => Hole h
		
		| _ => Fail)
	
	| (PRecord(r),expr)::rest => (case v of 
	
		  VRecord(r1) =>
		  
			let fun matchLists(l1,l2,s,t,g,processed) = (case (l1,l2) of 
			
					  ([],[]) => Success (expr,s,t,g)
					  
					  (* If records of different lengths, fail, regardless of other patterns *)
					| ([],_)  => Fail
					| (_,[])  => Fail
					
					| ((labv,v1)::rest1,(labp,pat1)::rest2) => 
					
						if labv=labp 
						
						(* Associate arbitrary 'expr' with pat1 to make it into a (pattern-expression) list *)
						then (case match(v1,[(pat1,expr)],s,t,g) of 
									
						  (* Stop processing this pattern, and look at the other patterns
						     using the original sigma,theta,gamma *)
						  Fail => match(v,rest,sigma,theta,gamma)

						(* Even though we return a hole, keep looking through rest of list
						   to check none of them fail *)
						| Hole h => (case matchLists(rest1,rest2,s,t,g,processed) of 
							
							  Fail => Fail
							| _    => Hole (RecordHole((labv,VHole(h))::rest1)))
							
						| Success (_,s1,t1,g1) => (case matchLists(rest1,rest2,s1,t1,g1,processed) of 
						
							  Hole (RecordHole(r)) => Hole (RecordHole(append((labv,v1)::processed,r)))
							
							(* For success, pass on the expression in case the sub-call
							   to matchLists re-called match on the next pattern in the list *)
							| s as Success(e2,s2,t2,g2) => s
							
							| _ => Fail))
							
						(* If labels are not equal, fail, regardless of other patterns *)
						else Fail)
			
			in matchLists(Record.sort(r1),Record.sort(r),sigma,theta,gamma,[]) end
		  
		| VHole(SimpleHole(ValueHole(ArithTypeVar(_)))) =>  Fail
		
		| VHole(SimpleHole(ValueHole(tyvar))) =>
		
			let val tyvar_type = case tyvar of TypeVar(_)    	 => TYPE_VAR
											| EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											| ArithTypeVar(_)  	 => ARITH_TYPE_VAR;
											(* arith cannot occur - matched above *)
				val genType = TRecord(genFreshTRecord(Record.getLabels(r),tyvar_type,theta));
				val genVal = gen(genType,theta);
				val theta1 = Substitution.union(theta,TypeHole(tyvar),genType);
				val sigma1 = Substitution.union(sigma,ValueHole(tyvar),genVal)
			in match(genVal,patExprList,sigma1,theta1,gamma) end
		
		| VHole(h) => Hole h
		
		| _ => Fail)
		
	(* No more pattern-expressions to check *)
	| _ => Fail);
		