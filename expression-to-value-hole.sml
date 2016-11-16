
(* Function to determine if we can take an expression and convert it to a value hole
   (i.e. a value)
   Returns outcome in an option datatype. If not successful, returns NONE
   Done recursively on strucutre of expression:
	- values of type int, real or bool cannot be a value hole, and neither can variables
	
	- values which are value holes trivially a value hole (i.e. drop Value datatype wrapper)
	
	- values which are pairs of values can only be a value hole if both components
	  can be a value hole
	  
	- arithmetic expressions can only be a value hole if both sub-expressions are value holes
	
	- boolean expressions can only be value holes if both sub-expressions are value holes
	
	- condition expressions can only be values holes if its gaurd is a value hole
	  remember value holes for conditions are of type (valhole * e * e)
	  we place no constraints on the two branch expression)
	  
	- case expressions can only be value holese if its left-most expression is a value hole
	  similarly to condition, place no constraints on result expression
	  value holes for cases are of type (valhole * pat * e)
	  
*)
   
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

	