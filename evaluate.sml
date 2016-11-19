(* No evaluation necessary for a value 
  (context-value) *)
fun evaluate (c as  Config(Expression(Value(v)),sigma,theta)) = c

(* Can't evaluate a stuck expression any more *)
(* (context-stuck) *)
|   evaluate (c as Config(Stuck,sigma,theta)) = c

(* Cannot evaluate a free variable on its own any further
   (context-var) *)
|  	evaluate (c as Config(Expression(Variable(x)),sigma,theta)) = c

(* Arithmetic operations +,-,*,/ with both arguments as values   ------------------- *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op an arithmetic operator *)
| 	evaluate (Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,ArithOper(oper))
		
(* Boolean operations <,<=,>,>=,= with both arguments as values *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op a boolean operator *)
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,BoolOper(oper))
 
(* Arithmetic & boolean operators: right argument a generic expression -----------------  *)
(* (context-op-2) *)
	  
|	evaluate (Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		  c as Config(Stuck,sigma1,theta1) => c
			  
		| Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
				
		| Config(Expression(e2eval),sigma1,theta1) => (case exprToValHole(e2eval) of
				
			  NONE => Config(Expression(ArithExpr(oper,Value(v1),e2eval)),sigma1,theta1)
			| SOME hole => evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(VHole(hole)))),sigma1,theta1))))
	
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		c as Config(Stuck,sigma1,theta1) => c
			  
		| Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
				
		| Config(Expression(e2eval),sigma1,theta1) => (case exprToValHole(e2eval) of
				
			  NONE => Config(Expression(BoolExpr(oper,Value(v1),e2eval)),sigma1,theta1)
			| SOME hole => evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(VHole(hole)))),sigma1,theta1))))
  
(* Arithmetic & boolean operators: both arguments a generic expression -----------------  *)
(* (context-op-1*)

|	evaluate (Config(Expression(ArithExpr(oper,e1,e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  c as Config(Stuck,sigma1,theta1) => c
			  
		| Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma1,theta1))
				
		| Config(Expression(e1eval),sigma1,theta1) => (case exprToValHole(e1eval) of
			
			  NONE => Config(Expression(ArithExpr(oper,e1eval,e2)),sigma1,theta1)
			| SOME hole => evaluate(Config(Expression(ArithExpr(oper,Value(VHole(hole)),e2)),sigma1,theta1))))
	
|	evaluate (Config(Expression(BoolExpr(oper,e1,e2)),sigma,theta)) =
		(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
			  c as Config(Stuck,sigma1,theta1) => c
			  
			| Config(Expression(Value(v1)),sigma1,theta1) =>
				evaluate(Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma1,theta1))
				
			| Config(Expression(e1eval),sigma1,theta1) => (case exprToValHole(e1eval) of
				
					  NONE => Config(Expression(BoolExpr(oper,e1eval,e2)),sigma1,theta1)
					| SOME hole => evaluate(Config(Expression(BoolExpr(oper,Value(VHole(hole)),e2)),sigma1,theta1))))

(* Resolves the slight loophole that <v,v> can be both an expression and a value *)
(* (context-value-pair) *)
|	evaluate (Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma,theta)) =
		evaluate(Config(Expression(Value(ValuePair(v1,v2))),sigma,theta))
	
(* Handles (context-pair-2), where left hand expression in pair a value *)
|  	evaluate (Config(Expression(ExpressionPair(Value(v1),e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		  c as Config(Stuck,sigma1,theta1) => c
		  
		| Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma1,theta1))
			
		| Config(Expression(e2eval),sigma1,theta1) => (case exprToValHole(e2eval) of
			
				  NONE => Config(Expression(ExpressionPair(Value(v1),e2eval)),sigma1,theta1)
				| SOME hole => evaluate(Config(Expression(ExpressionPair(Value(v1),Value(VHole(hole)))),sigma1,theta1))))

(* Handles (context-pair-1), where both expressions in a pair not a value *)
|  	evaluate (Config(Expression(ExpressionPair(e1,e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  c as Config(Stuck,sigma1,theta1) => c
		  
		| Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),e2)),sigma1,theta1))
			
		| Config(Expression(e1eval),sigma1,theta1) => (case exprToValHole(e1eval) of
			
				  NONE => Config(Expression(ExpressionPair(e1eval,e2)),sigma1,theta1)
				| SOME hole => evaluate(Config(Expression(ExpressionPair(Value(VHole(hole)),e2)),sigma1,theta1))))
	
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2) and (E-IF-BAD) *)
|  evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) =

	(case narrow(v,Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b))),sigma2,theta2) =>
		
			if b (* rule E-IF-GOOD1 *)
				 then evaluate(Config(Expression(e1),sigma2,theta2))
				
				 (* rule E-IF-GOOD2 *)
				 else evaluate(Config(Expression(e2),sigma2,theta2)))
				 
(* (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta)) =
   	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  c as Config(Stuck,sigma1,theta1) => c
		  
		| Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(Condition(Value(v1),e2,e3)),sigma1,theta1))
			
		| Config(Expression(e1eval),sigma1,theta1) => (case exprToValHole(e1eval) of
			
				  NONE => Config(Expression(Condition(e1eval,e2,e3)),sigma1,theta1)
				| SOME hole => evaluate(Config(Expression(Condition(Value(VHole(hole)),e2,e3)),sigma1,theta1))))
 
(* Implements evaluation & type inference rules for case-pair expression with first operand a value *)
(* i.e. implement rules (E-CASE-PAIR-GOOD) and (E-CASE-PAIR-BAD) *) 
|  evaluate (Config(Expression(Case(Value(v),VariablePair(x1,x2),e)),sigma,theta)) =

	(* generate fresh type variables, alpha1 and alpha2, using unique counter *)
	let val alpha1 = generateFreshTypeVar(TYPE_VAR,theta)
		val alpha2 = generateFreshTypeVar(TYPE_VAR,theta)
		
	in (case narrow(v,Pair(alpha1,alpha2),sigma,theta) of
			
	  (* rule E-CASE-PAIR-BAD *)
	  c as Config(Stuck,sigma1,theta1) => c
			  
	  (* rule E-CASE-PAIR-GOOD *)
	| Config(Expression(Value(ValuePair(v1,v2))),sigma1,theta1) =>
				
		(* declare mapping of variables to values that will be used in 
		   substitution function *)
		let val gamma = [ (x1,Value(v1)), (x2,Value(v2)) ]
		in evaluate(Config(Expression(substitute(e,gamma)),sigma1,theta1)) end)
				
   end
   
(* (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,pat,e2)),sigma,theta)) =
	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		c as Config(Stuck,sigma1,theta1) => c
			  
		| Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(Case(Value(v1),pat,e2)),sigma1,theta1))
				
		| Config(Expression(e1eval),sigma1,theta1) => (case exprToValHole(e1eval) of
				
			  NONE => Config(Expression(Case(e1eval,pat,e2)),sigma1,theta1)
			| SOME hole => evaluate(Config(Expression(Case(Value(VHole(hole)),pat,e2)),sigma1,theta1))))
	
(* Expression matches none of the above patterns in the clauses
   Must be a stuck expression *)
| evaluate (Config(Expression(_),sigma,theta)) = Config(Stuck,sigma,theta);