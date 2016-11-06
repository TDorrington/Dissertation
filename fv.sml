(* Takes an expression and returns a set containing the free variables in that expression

   Only non-trivial case is the case expression
   For case v of (x1,x2) -> e2
   We find the free variables of e2, and remove from that set the variables x1 and x2 *)

fun fvExpr(e) = case e of 

	  Value(_) => []
	| Plus(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Times(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Subtract(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Divide(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| LessThan(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| MoreThan(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| LessThanEqual(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| MoreThanEqual(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Equal(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Condition(e1,e2,e3) => append(append(fvExpr(e1),fvExpr(e2)),fvExpr(e3))
	| ExpressionPair(e1,e2) => append(fvExpr(e1),fvExpr(e2))
	| Case (Value(_),ExpressionPair(Variable(x),Variable(y)),e) => remove(remove(fvExpr(e),x),y)
	| Variable(x) => [x];
	
fun fv ([]) = []
|   fv (e::eList) = append(fvExpr(e),fv(eList));