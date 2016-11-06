(* Takes an expression and returns an alpha-invariant version
   This is used in the substitute function when the side condition fails.
   Only non trivial case is a variable: to make a different
   variable identifier, and (hopefully) no longer capture avoiding,
   we append a unique integer to it, which is passed from outside the function.
   This preserves de Brujin Indices order: all occurrences of a variable x in expression e 
   are now ALL referred to as variable xn, after the call to alphaVariant(e,n) *)

fun alphaVariant(e,n) = case e of 

	  Value(v) => Value(v)
	| Plus(e1,e2) => Plus(alphaVariant(e1,n),alphaVariant(e2,n))
	| Times(e1,e2) => Times(alphaVariant(e1,n),alphaVariant(e2,n))
	| Subtract(e1,e2) => Subtract(alphaVariant(e1,n),alphaVariant(e2,n))
	| Divide(e1,e2) => Divide(alphaVariant(e1,n),alphaVariant(e2,n))
	| LessThan(e1,e2) => LessThan(alphaVariant(e1,n),alphaVariant(e2,n))
	| MoreThan(e1,e2) => MoreThan(alphaVariant(e1,n),alphaVariant(e2,n))
	| LessThanEqual(e1,e2) => LessThanEqual(alphaVariant(e1,n),alphaVariant(e2,n))
	| MoreThanEqual(e1,e2) => MoreThanEqual(alphaVariant(e1,n),alphaVariant(e2,n))
	| Equal(e1,e2) => Equal(alphaVariant(e1,n),alphaVariant(e2,n))
	| Condition(e1,e2,e3) => Condition(alphaVariant(e1,n),alphaVariant(e2,n),alphaVariant(e3,n))
	| ExpressionPair(e1,e2) => ExpressionPair(alphaVariant(e1,n),alphaVariant(e2,n))
	| Case (Value(v),ExpressionPair(Variable(Var(x)), Variable(Var(y))),e) => 
		Case(Value(v),
			 ExpressionPair(Variable(Var(x ^ Int.toString(n))), Variable(Var(y ^ Int.toString(n)))),
			 alphaVariant(e,n))
	| Variable(Var(s)) => Variable(Var(s ^ Int.toString(n)));