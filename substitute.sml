(* Takes an expression e, a map of variables to expressions, gamma,
   and substitutes all variables occurring in expression e, for which there is a map
   to some value in gamma, with that value. Denoted 'gamma e'
   Simply implemented by pattern matching on the structure of the expression
   and recursively calling substitute on its sub-expressions 
   Only non-trivial case is case expression. Need to perform a capture-avoiding sbustitution. 
*)
			
fun substitute(e,gamma:variableSub) = case e of 

	  Value(_) => e
	| Plus(e1,e2) => Plus(substitute(e1,gamma),substitute(e2,gamma))
	| Times(e1,e2) => Times(substitute(e1,gamma),substitute(e2,gamma))
	| Subtract(e1,e2) => Subtract(substitute(e1,gamma),substitute(e2,gamma))
	| Divide(e1,e2) => Divide(substitute(e1,gamma),substitute(e2,gamma))
	| LessThan(e1,e2) => LessThan(substitute(e1,gamma),substitute(e2,gamma))
	| MoreThan(e1,e2) => MoreThan(substitute(e1,gamma),substitute(e2,gamma))
	| LessThanEqual(e1,e2) => LessThanEqual(substitute(e1,gamma),substitute(e2,gamma))
	| MoreThanEqual(e1,e2) => MoreThanEqual(substitute(e1,gamma),substitute(e2,gamma))
	| Equal(e1,e2) => Equal(substitute(e1,gamma),substitute(e2,gamma))
	| Condition(e1,e2,e3) => Condition(substitute(e1,gamma),substitute(e2,gamma),substitute(e3,gamma))
	| ExpressionPair(e1,e2) => ExpressionPair(substitute(e1,gamma),substitute(e2,gamma))
	
	| Variable(a) => if Substitution.contains(a,gamma) 
					 then Substitution.get(a,gamma)
					 else Variable(a)
					
	| c as Case (Value(v),ExpressionPair(Variable(x),Variable(y)),e2) => 
		(* Need to perform a capture-avoiding substitution for 'case e1 of (x,y) -> e2'
		   To check if we need to perform alpha conversion, 
		   check that x and y are not in the domain of gamma
		   and x and y are not in the set of free variables of the range of gamma
		   Note that first expression is a value here; the substitute method will only
		   be called on case expressions when the first argument is a value pair.
		   Same reasoning why case expression clause in alphaInvariant method of this form *)
		   
		let val dom = Substitution.domain(gamma);
			val fvRan = fv(Substitution.range(gamma))
		in
			if (((element(dom,x)=false) andalso (element(dom,y)=false)) andalso 
				((element(fvRan,x)=false) andalso (element(fvRan,y)=false)))
			then
				(* Substitution is capture-avoiding
				   Recursively substitute in body of case expression *)
				Case(Value(v),ExpressionPair(Variable(x),Variable(y)),substitute(e2,gamma))
			else
				(* Need to generate an alpha invariant version of the case expression
				   and then call substitute again
				   We pass in the unique integer we want to append to all variable names *)
				substitute(alphaVariant(c,getCounterAndUpdate()),gamma)
		end
		
			
		
	
