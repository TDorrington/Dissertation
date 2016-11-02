(* Takes an expression, a value and a variable
   and substitutes all occurrences of the variable in the expression
   with the corresponding value
   Simply implemented by pattern matching on expression of structure
   and recursively calling substitute on its sub-expressions
   
   The only non trivial case is the Case expression. 
   Consider
   case (3,2) of (x1,x2) ->
		case (4,5) of (x1,x2) -> true
   If we did substitute (x1=3,x2=2) throughout the entire e3
   we would be left with 
		case (4,5) of (3,2) -> true which would get stuck
		
   Further consider the case 
   case (3,2) of (x1,x2) ->
		case (4,5) of (x1,x2) -> x1+x2
   This should give 9, since variables should bind to their closest matching case pattern
   Hence, we cannot recursively call substitute on e2 and e3 for case(e1,e2,e3)
   
   To solve this, we first evaluate the case expression with its value pair argument 
   substituted for, and then substitute the resulting configuration returned from evaluation *)
	
fun substitute(e,v,x,s,t,evaluate) = 

	let val rec subs = fn (e,v,x) => case e of 
		  Value(_) => e
		| Plus(e1,e2) => Plus(subs(e1,v,x),subs(e2,v,x))
		| Times(e1,e2) => Times(subs(e1,v,x),subs(e2,v,x))
		| Subtract(e1,e2) => Subtract(subs(e1,v,x),subs(e2,v,x))
		| Divide(e1,e2) => Divide(subs(e1,v,x),subs(e2,v,x))
		| LessThan(e1,e2) => LessThan(subs(e1,v,x),subs(e2,v,x))
		| MoreThan(e1,e2) => MoreThan(subs(e1,v,x),subs(e2,v,x))
		| LessThanEqual(e1,e2) => LessThanEqual(subs(e1,v,x),subs(e2,v,x))
		| MoreThanEqual(e1,e2) => MoreThanEqual(subs(e1,v,x),subs(e2,v,x))
		| Equal(e1,e2) => Equal(subs(e1,v,x),subs(e2,v,x))
		| Condition(e1,e2,e3) => Condition(subs(e1,v,x),subs(e2,v,x),subs(e3,v,x))
		| ExpressionPair(e1,e2) => ExpressionPair(subs(e1,v,x),subs(e2,v,x))
		| Variable(a) => if a = x then (Value(v),s,t) else (Variable(a),s,t)
		| Case(e1,e2,e3) => 
			let val (newExpr,sigma,theta) = evaluate(Config(Expression(Case(subs(e1,v,x),e2,e3)),s,t))
				val (a,_,_) = subs(newExpr,v,x)
			in (a,sigma,theta) end
	
	in subs(e,v,x) end;
	
