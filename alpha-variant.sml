(* Takes an expression and returns an alpha-variant version
   Also takes a list of variables, vars, which we only need change; we cannot change all variables
   as we only want to change those variables bound in the closest fun/case binding.
   This is used in the substitute function when the side condition fails.
   Only non trivial case is a variable: to make a different
   variable identifier, and (hopefully) no longer capture avoiding,
   we append a unique integer to it, which is passed from outside the function.
   This is implemented in the function substituteVar
   This preserves de Brujin Indices order: all occurrences of a variable x in expression e 
   are now ALL referred to as variable xn, after the call to alphaVariant(e,n)
*)

fun substituteVar(Var(s),n,vars) = 

	if element(vars,Var(s)) 
	then Var(s^Int.toString(n))
	else Var(s);

fun alphaVariant(e,n,vars) = 

	(* local function to recursively substitute based on structure of expression - 
	   avoids need to pass n and vars in each call *)
	let fun localSub e = case e of 
	
		(* Compound value holes can contain expressions *)
		  Value(ValuePair(v1,v2)) => 
			let val Value(alpha1) = localSub(Value(v1));
				val Value(alpha2) = localSub(Value(v2))
			in Value(ValuePair(alpha1,alpha2)) end
			
		| Value(VHole(hole)) => (case hole of 
		
			  SimpleHole(_) => e
			  
			| BinaryOp(oper,hole1,hole2) =>
				let val Value(VHole(alpha1)) = localSub(Value(VHole(hole1)));
					val Value(VHole(alpha2)) = localSub(Value(VHole(hole2)))
				in Value(VHole(BinaryOp(oper,alpha1,alpha2))) end
				
			| ConditionHole(hole1,e1,e2) =>
				let val Value(VHole(alpha1)) = localSub(Value(VHole(hole1)))
				in Value(VHole(ConditionHole(alpha1,localSub(e1),localSub(e2)))) end
				
			| CaseHole(hole1,VariablePair(x,y),e) =>
				Value(VHole(CaseHole(hole1,VariablePair(substituteVar(x,n,vars),substituteVar(y,n,vars)),localSub(e)))))
				
		| Value(_) => e (* int, real or bool *)
		| ArithExpr(arithOper,e1,e2) => ArithExpr(arithOper,localSub(e1),localSub(e2))
		| BoolExpr (boolOper, e1,e2) => BoolExpr (boolOper, localSub(e1),localSub(e2))
		| ExpressionPair(e1,e2) => ExpressionPair(localSub(e1),localSub(e2))
		| Condition(e1,e2,e3) => Condition(localSub(e1),localSub(e2),localSub(e3))
		| Case(e1,VariablePair(x,y),e2) =>
			Case(e1,VariablePair(substituteVar(x,n,vars),substituteVar(y,n,vars)),localSub(e2))
		| Variable(x) => Variable(substituteVar(x,n,vars))
									
	in localSub(e) end;