(* Takes an expression e, a gamma of variables to values, gamma,
   and substitutes all occurrences of the variable in the expression
   with the corresponding value, denoted e[v/x]
   Simply implemented by pattern matching on the structure of the expression
   and recursively calling substitute on its sub-expressions *)
			
fun substitute(e,gamma) = case e of 

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
					
	| caseExpr Case (Value(_),ExpressionPair(Variable(x),Variable(y)),e) => 
	
		let val dom = domain(gamma);
			val fvRan = fv(range(gamma))
		in
		if (((element(dom,x)=false) andalso (element(dom,y)=false)) andalso 
		    ((element(fvRan,x)=false) andalso (element(fvRan,y)=false)))
		then
		
		else
			substitute(
		end
		
			
		
	
