(* Function to determine if we can take an expression and convert it to a value hole
   (i.e. a value)
   Returns outcome in an option datatype. If not successful, returns NONE *)
   
fun exprToValHole(e) = case e of

	  Value(VHole(h)) => SOME h
	
	| Value(ValuePair(v1,v2)) => (case (exprToValHole(Value(v1)),exprToValHole(Value(v2))) of
	
		 (SOME hole1, SOME hole2) => SOME (BinaryOp(EXPR_PAIR,hole1,hole2))
		| _ => NONE)
	
	| Value(_) => NONE
	
	| Variable(_) => NONE
	
	| ArithExpr(arithoper,e1,e2) => (case (exprToValHole(e1),exprToValHole(e2)) of
	
		  (SOME hole1, SOME hole2) => SOME (BinaryOp(ArithOper(arithoper),hole1,hole2))
		| _ => NONE)
		
	| BoolExpr(booloper,e1,e2) => (case (exprToValHole(e1),exprToValHole(e2)) of
	
		  (SOME hole1, SOME hole2) => SOME (BinaryOp(BoolOper(booloper),hole1,hole2))
		| _ => NONE)
		  
	| ExpressionPair(e1,e2) => (case (exprToValHole(e1),exprToValHole(e2)) of
	
		  (SOME hole1, SOME hole2) => SOME (BinaryOp(EXPR_PAIR,hole1,hole2))
		| _ => NONE)
		
	| Case(e1,pat,e2) => (case exprToValHole(e1) of
	
		  (SOME hole1) => SOME (CaseHole(hole1,pat,e2))
		| _ => NONE)
		
	| Condition(e1,e2,e3) => (case exprToValHole(e1) of
	
		  (SOME hole1) => SOME (ConditionHole(hole1,e2,e3))
		| _ => NONE);

	