(* Evaluates expression, assuming closed *)

fun elab(expr) = case expr of

	  Value(v) => (v,[],[])
	  
(*  | Variable(Var(x)) => ... *)

	| Plus(e1,e2)
	
	| LessThan(e1,e2)
	
	| MoreThan(e1,e2)
	
	| LessThanEqual(e1,e2)
	
	| MoreThanEqual(e1,e2)
	
	| Equal
	  