(* Takes an expression e, a value v and a variable x
   and substitutes all occurrences of the variable in the expression
   with the corresponding value
   Simply implemented by pattern matching on the structure of the expression
   and recursively calling substitute on its sub-expressions
   We return the expression with its substitutions made, i.e. e[v/x], as well as new 
   value and type substitutions. The substitutions are all unchanged except possibly
   in the case expression: evaluating the sub expressions may change substitutions 
   sinces we call evaluate
   
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
   but must first instead evaluate the sub-case expression.
   
   This is because we need to work up to alpha-conversion. We solve this by instead
   substituting variable x for value v in the expression e1
   and first recursively calling evaluate on Case(e1[v/x],e2,e3), before substituting afterwards

	This is why substitute also takes the arguments s, t and evaluate
	i.e. the value and type substitutions and the function evaluate *)
			
fun substitute(e,v,x,s,t,evaluate) = case e of 

	  (* handle non-trivial case first *)
	  Case(e1,e2,e3) => 
			let val (subexpression,sigma,theta) = substitute(e1,v,x,s,t,evaluate)
				val Config(Expression(newExpr),sigma1,theta1) = evaluate(Config(Expression(Case(subexpression,e2,e3)),sigma,theta))
			in substitute(newExpr,v,x,sigma1,theta1,evaluate) end

	| _ => let val rec subs = fn(e) => case e of 
	
			  Value(_) => e
			| Plus(e1,e2) => Plus(subs(e1),subs(e2))
			| Times(e1,e2) => Times(subs(e1),subs(e2))
			| Subtract(e1,e2) => Subtract(subs(e1),subs(e2))
			| Divide(e1,e2) => Divide(subs(e1),subs(e2))
			| LessThan(e1,e2) => LessThan(subs(e1),subs(e2))
			| MoreThan(e1,e2) => MoreThan(subs(e1),subs(e2))
			| LessThanEqual(e1,e2) => LessThanEqual(subs(e1),subs(e2))
			| MoreThanEqual(e1,e2) => MoreThanEqual(subs(e1),subs(e2))
			| Equal(e1,e2) => Equal(subs(e1),subs(e2))
			| Condition(e1,e2,e3) => Condition(subs(e1),subs(e2),subs(e3))
			| ExpressionPair(e1,e2) => ExpressionPair(subs(e1),subs(e2))
			| Variable(a) => if a = x then Value(v) else Variable(a)
			
			in (subs(e),s,t) end;
		
	
