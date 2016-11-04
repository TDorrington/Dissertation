(* Takes an expression and returns an alpha-invariant version
   This is used in the substitute function when the side condition fails, i.e. in the case
   that either x is in the domain of the substitution
   or x is in the free variables of the range of the domain *)

fun alphaInvariant(e) = case e of 

	  Value(v) => Value(v)
	| Plus(e1,e2) => Plus(alphaInvariant(e1),alphaInvariant(e2))
	| Times(e1,e2) => Times(alphaInvariant(e1),alphaInvariant(e2))
	| Subtract(e1,e2) => Subtract(alphaInvariant(e1),alphaInvariant(e2))
	| Divide(e1,e2) => Divide(alphaInvariant(e1),alphaInvariant(e2))
	| LessThan(e1,e2) => LessThan(alphaInvariant(e1),alphaInvariant(e2))
	| MoreThan(e1,e2) => MoreThan(alphaInvariant(e1),alphaInvariant(e2))
	| LessThanEqual(e1,e2) => LessThanEqual(alphaInvariant(e1),alphaInvariant(e2))
	| MoreThanEqual(e1,e2) => MoreThanEqual(alphaInvariant(e1),alphaInvariant(e2)))
	| Equal(e1,e2) => Equal(alphaInvariant(e1),alphaInvariant(e2))
	| Condition(e1,e2,e3) => Condition(alphaInvariant(e1),alphaInvariant(e2),alphaInvariant(e3))
	| ExpressionPair(e1,e2) => ExpressionPair(alphaInvariant(e1),alphaInvariant(e2))
	| Case (Value(v),ExpressionPair(varX as Variable(x), varY as Variable(y)),e) => 
		Case(Value(v),ExpressionPair(alphaInvariant(varX),alphaInvariant(varY)), alphaInvariant(e))
	| Variable(Var(s)) => Variable(Var(s^"_"));