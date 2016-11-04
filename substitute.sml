(* Takes an expression e, a value v and a variable x
   and substitutes all occurrences of the variable in the expression
   with the corresponding value, denoted e[v/x]
   Simply implemented by pattern matching on the structure of the expression
   and recursively calling substitute on its sub-expressions
   
   Note there is no case for the expression of case-pair; they are always evaluated
   first and we never get to an expression which needs substituting and contains one *)
			
fun substitute(e,v,x) = case e of 

	  Value(_) => e
	| Plus(e1,e2) => Plus(substitute(e1,v,x),substitute(e2,v,x))
	| Times(e1,e2) => Times(substitute(e1,v,x),substitute(e2,v,x))
	| Subtract(e1,e2) => Subtract(substitute(e1,v,x),substitute(e2,v,x))
	| Divide(e1,e2) => Divide(substitute(e1,v,x),substitute(e2,v,x))
	| LessThan(e1,e2) => LessThan(substitute(e1,v,x),substitute(e2,v,x))
	| MoreThan(e1,e2) => MoreThan(substitute(e1,v,x),substitute(e2,v,x))
	| LessThanEqual(e1,e2) => LessThanEqual(substitute(e1,v,x),substitute(e2,v,x))
	| MoreThanEqual(e1,e2) => MoreThanEqual(substitute(e1,v,x),substitute(e2,v,x))
	| Equal(e1,e2) => Equal(substitute(e1,v,x),substitute(e2,v,x))
	| Condition(e1,e2,e3) => Condition(substitute(e1,v,x),substitute(e2,v,x),substitute(e3,v,x))
	| ExpressionPair(e1,e2) => ExpressionPair(substitute(e1,v,x),substitute(e2,v,x))
	| Variable(a) => if a = x then Value(v) else Variable(a)
			
		
	
