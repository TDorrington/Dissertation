(* Takes an expression and returns an alpha-variant version
   Also takes a list of variables, vars, which we only need change; we cannot change all variables
   as we only want to change those variables bound in the closest fun/case binding.
   This is used in the substitute function when the side condition fails.
   Only non trivial case is a variable: to make a different
   variable identifier, and (hopefully) no longer capture avoiding,
   we append a unique integer to it, which is passed from outside the function.
   This preserves de Brujin Indices order: all occurrences of a variable x in expression e 
   are now ALL referred to as variable xn, after the call to alphaVariant(e,n) *)

fun alphaVariant(e,n,vars) = 

	(* local function to recursively substitute based on structure of expression - 
	   avoids need to pass n and vars in each call *)
	let val rec localSub = fn e => case e of 
	
		  Value(v) => Value(v)
		| Plus(e1,e2) => Plus(localSub(e1),localSub(e2))
		| Times(e1,e2) => Times(localSub(e1),localSub(e2))
		| Subtract(e1,e2) => Subtract(localSub(e1),localSub(e2))
		| Divide(e1,e2) => Divide(localSub(e1),localSub(e2))
		| LessThan(e1,e2) => LessThan(localSub(e1),localSub(e2))
		| MoreThan(e1,e2) => MoreThan(localSub(e1),localSub(e2))
		| LessThanEqual(e1,e2) => LessThanEqual(localSub(e1),localSub(e2))
		| MoreThanEqual(e1,e2) => MoreThanEqual(localSub(e1),localSub(e2))
		| Equal(e1,e2) => Equal(localSub(e1),localSub(e2))
		| Condition(e1,e2,e3) => Condition(localSub(e1),localSub(e2),localSub(e3))
		| ExpressionPair(e1,e2) => ExpressionPair(localSub(e1),localSub(e2))
		| Case (Value(v),ExpressionPair(varX as Variable(Var(x)), varY as Variable(Var(y))),e) => 
			Case(Value(v),ExpressionPair(localSub(varX),localSub(varY)),localSub(e))
		| Variable(Var(s)) => 
			if element(vars,Var(s)) then Variable(Var(s ^ Int.toString(n)))
									else e
									
	in localSub(e) end;