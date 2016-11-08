(* No evaluation necessary for a value 
   But we can still perform substitutions on value holes
   or value pairs which contain value holes (c.f. recursive local function) *)
fun evaluate (c as  Config(Expression(Value(v)),sigma,theta)) =

	let val rec sub = fn value => (case value of
	
		  N(_) => value
		| B(_) => value
		| R(_) => value 
		| VHole(valueHole) => 
				if Substitution.contains(valueHole,sigma)
				then Substitution.get(valueHole,sigma)
				else value
		| ValuePair(value1,value2) => ValuePair(sub(value1),sub(value2)))

	in Config(Expression(Value(sub(v))),sigma,theta) end
	
(* Can't evaluate a stuck expression any more *)
(* (context-stuck) *)
|   evaluate (c as Config(Stuck,sigma,theta)) = c

(* Cannot evaluate a free variable on its own any further *)
|  	evaluate (c as Config(Expression(Variable(x)),sigma,theta)) = c

(* Resolves the slight loophole that <v,v> can be an expression and a value *)
(* (context-value-pair) *)
|	evaluate (Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma,theta)) =
		Config(Expression(Value(ValuePair(v1,v2))),sigma,theta)

(* Handles pair of general expressions *)
|  	evaluate (Config(Expression(ExpressionPair(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,EXPRPAIR,evaluate)

(* Arithmetic operations +,-,*,/ with both arguments as values   ------------------- *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op an arithmetic operator *)
|   evaluate (Config(Expression(Plus(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,PLUS)
|   evaluate (Config(Expression(Times(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,TIMES)
|   evaluate (Config(Expression(Subtract(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,SUBTRACT)
|   evaluate (Config(Expression(Divide(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,DIVIDE)
		
(* Boolean operations <,<=,>,>=,= with both arguments as values *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op a boolean operator *)
|  evaluate (Config(Expression(LessThan(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,LESS)
|  evaluate (Config(Expression(MoreThan(Value(v1),Value(v2))),sigma,theta)) =	
		elabPhraseOperation(v1,v2,sigma,theta,MORE)
|  evaluate (Config(Expression(LessThanEqual(Value(v1),Value(v2))),sigma,theta)) =	
		elabPhraseOperation(v1,v2,sigma,theta,LESSEQ)
|  evaluate (Config(Expression(MoreThanEqual(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,MOREEQ)
|  evaluate (Config(Expression(Equal(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,EQ)
 
(* Arithmetic & boolean operators: at least one argument a generic expression -----------------  *)
  
|   evaluate (Config(Expression(Plus(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,PLUS,evaluate)
	
|   evaluate (Config(Expression(Times(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,TIMES,evaluate)
	
|	evaluate (Config(Expression(Subtract(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,SUBTRACT,evaluate)

|	evaluate (Config(Expression(Divide(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,DIVIDE,evaluate)

|	evaluate (Config(Expression(LessThan(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,LESS,evaluate)

| 	evaluate (Config(Expression(MoreThan(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,MORE,evaluate)

| 	evaluate (Config(Expression(LessThanEqual(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,LESSEQ,evaluate)

| 	evaluate (Config(Expression(MoreThanEqual(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,MOREEQ,evaluate)

| 	evaluate (Config(Expression(Equal(e1,e2)),sigma,theta)) =
	contextRuleOp2(e1,e2,sigma,theta,EQ,evaluate)
	
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
				 
(* Handles context rule (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta)) =
   contextRuleOp3(e1,e2,e3,sigma,theta,COND,evaluate)
 
(* Implements evaluation & type inference rules for case-pair expression with first operand a value *)
(* i.e. implement rules (E-CASE-PAIR-GOOD) and (E-CASE-PAIR-BAD) *) 
|  evaluate (Config(Expression(Case(Value(v),ExpressionPair(Variable(x1),Variable(x2)),e)),sigma,theta)) =

	(* generate fresh type variables, alpha1 and alpha2, using unique counter *)
	let val alpha1 = TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())));
		val alpha2 = TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())))
	in
		(* double check fresh type variables not already in type substitution
		   this should not be the case if we begin evaluation with empty substitution since all
		   the type variables generated by the program are unique,
		   but may be exceptional circumstance where type substitution already contains 
		   mapping for alpha1 or alpha2
		   If already in map, keep re-calling function until they are different from any already there *)
		if (Substitution.contains(alpha1,theta) orelse Substitution.contains(alpha2,theta))
		then evaluate(Config(Expression(Case(Value(v),ExpressionPair(Variable(x1),Variable(x2)),e)),sigma,theta))
		else
	   
			case narrow(v,Pair(THole(alpha1),THole(alpha2)),sigma,theta) of
			
			  (* rule E-CASE-PAIR-BAD *)
			  c as Config(Stuck,sigma1,theta1) => c
			  
			  (* rule E-CASE-PAIR-GOOD *)
			| Config(Expression(Value(ValuePair(v1,v2))),sigma1,theta1) =>
				
				(* declare mapping of variables to values that will be used in 
				   substitution function *)
				let val gamma = [ (x1,Value(v1)), (x2,Value(v2)) ]
				in evaluate(Config(Expression(substitute(e,gamma)),sigma1,theta1)) end
				
   end
   
(* Handles context rule (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,e2 as ExpressionPair(Variable(x1),Variable(x2)),e3)),sigma,theta)) =
	contextRuleOp3(e1,e2,e3,sigma,theta,CASE,evaluate)	
	
(* Expression matches none of the above patterns in the clauses
   Must be a stuck expression *)
| evaluate (Config(Expression(_),sigma,theta)) = Config(Stuck,sigma,theta);