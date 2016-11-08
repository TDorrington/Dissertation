fun equalValues(v1,v2) = case(v1,v2) of

	  (R(r1),R(r2)) => r1<=r2 andalso r2<=r1
	| (ValuePair(v11,v12),ValuePair(v21,v22)) => equalValues(v11,v21) andalso equalValues(v12,v22)
	| (N(n1),N(n2)) => n1=n2
	| (B(b1),B(b2)) => b1=b2
	| (VHole(s1),VHole(s2)) => s1=s2
	| _ => false;

fun equalExpressions(e1,e2) = case (e1,e2) of

	  (Value(v1),Value(v2)) => equalValues(v1,v2)
	| (Plus(e11,e12),Plus(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Times(e11,e12),Times(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Subtract(e11,e12),Subtract(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Divide(e11,e12),Divide(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (LessThan(e11,e12),LessThan(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (MoreThan(e11,e12),MoreThan(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (LessThanEqual(e11,e12),LessThanEqual(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (MoreThanEqual(e11,e12),MoreThanEqual(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Equal(e11,e12),Equal(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Condition(e11,e12,e13),Condition(e21,e22,e23)) => 
		equalExpressions(e11,e21) andalso equalExpressions(e12,e22) andalso equalExpressions(e13,e23)
	| (ExpressionPair(e11,e12),ExpressionPair(e21,e22)) => equalExpressions(e11,e21) andalso equalExpressions(e12,e22)
	| (Case(e11,e12,e13),Case(e21,e22,e23)) => 
		equalExpressions(e11,e21) andalso equalExpressions(e12,e22) andalso equalExpressions(e13,e23)
	| (Variable(x),Variable(y)) => x=y
	| _ => false;