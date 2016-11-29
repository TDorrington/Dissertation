(* Curried function which takes an operator oper, and
   returns a function from a pair of two values to a possibly-stuck expression *)
val rec operationWrap = fn oper =>

   (fn (N(n1),N(n2)) => (case oper of 
		  ArithOper(PLUS) 	  => Expression(Value(N(n1+n2)))
		| ArithOper(SUBTRACT) => Expression(Value(N(n1-n2)))
		| ArithOper(TIMES)    => Expression(Value(N(n1*n2)))
		| ArithOper(DIVIDE)   => Stuck
		| BoolOper(LESS)      => Expression(Value(B(n1<n2)))
		| BoolOper(LESS_EQ)   => Expression(Value(B(n1<=n2)))
		| BoolOper(MORE)      => Expression(Value(B(n1>n2)))
		| BoolOper(MORE_EQ)   => Expression(Value(B(n1>=n2)))
		| BoolOper(EQ)        => Expression(Value(B(n1=n2)))
		| EXPR_PAIR			  => Stuck)
		
    | (R(r1),R(r2)) => (case oper of 
		  ArithOper(PLUS)     => Expression(Value(R(r1+r2)))
		| ArithOper(SUBTRACT) => Expression(Value(R(r1-r2)))
		| ArithOper(TIMES)    => Expression(Value(R(r1*r2)))
		| ArithOper(DIVIDE)   => Expression(Value(R(r1/r2)))
		| BoolOper(LESS)      => Expression(Value(B(r1<r2)))
		| BoolOper(LESS_EQ)   => Expression(Value(B(r1<=r2)))
		| BoolOper(MORE)      => Expression(Value(B(r1>r2)))
		| BoolOper(MORE_EQ)   => Expression(Value(B(r1>=r2)))
		| BoolOper(EQ)		  => Stuck
		| EXPR_PAIR		      => Stuck)
		 
	| (B(b1), B(b2)) => (case oper of 
		  BoolOper(EQ) => Expression(Value(B(b1=b2)))
		| _ 		   => Stuck)
	 
	| (va as ValuePair(va1,va2), vb as ValuePair(vb1,vb2)) => (case oper of
		  BoolOper(EQ) => (case (operationWrap(oper) (va1,vb1), operationWrap(oper) (va2,vb2)) of
							
			  (Expression(Value(B(b1))),Expression(Value(B(b2)))) => Expression(Value(B(b1 andalso b2)))
			| (Expression(_),Expression(_)) => Expression(Value(VHole(BinaryOp(oper,va,vb))))
			| _	=> Stuck)
			
		| _ 		   => Stuck)
	
	| (VHole(hole), v2) => Expression(Value(VHole(BinaryOp(oper,VHole(hole),v2))))
	 
	| (v1, VHole(hole)) => Expression(Value(VHole(BinaryOp(oper,v1,VHole(hole)))))
	
	| _ => Stuck);
		
(* Rules for arithmetic and boolean operations *)
fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, gamma, oper, t) : config = 
	
	(case evaluate(narrow(v1,t,sigma,theta,gamma)) of
			
	  (* rule E-OP-BAD1 *)
	  c as Config(Stuck,_,_,_) => c 
	  
	| Config(Expression(Value(n1)),sigma1,theta1,gamma1) => (case evaluate(narrow(v2,t,sigma1,theta1,gamma1)) of
					
		  (* rule E-OP-BAD2 *)
		  c as Config(Stuck,_,_,_) => c
		  
		| Config(Expression(Value(n2)),sigma2,theta2,gamma2) => 
		
			(* rule E-OP-GOOD *)
			Config(oper(n1,n2),sigma2,theta2,gamma2)))
						
and elabPhraseOperation(v1,v2,sigma,theta,gamma,oper) =
	
	let val wrapper = operationWrap(oper) 
	in (case (v1,v2) of 
	
	  (* op : int * int -> int/bool *)
	  (N(_),N(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Int)
	| (N(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Int)
	| (VHole(_),N(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Int)
		
	  (* op : real * real -> real/bool *)
	| (R(_),R(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Real)
	| (R(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Real)
	| (VHole(_),R(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Real)
		
	(* = : bool * bool -> bool *)
	| (B(_),B(_))     => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Bool)
	| (B(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Bool)
	| (VHole(_),B(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,gamma,wrapper,Bool)
		
	(* For value pair arguments, can only be operator equal
	   Recursively call this function on each of the pair arguments *)
	| (ValuePair(va1,va2),ValuePair(vb1,vb2)) => 
		if oper = BoolOper(EQ) 
		then let val Config(e1,sigma1,theta1,gamma1) = elabPhraseOperation(va1,vb1,sigma,theta,gamma,oper);
			     val Config(e2,sigma2,theta2,gamma2) = elabPhraseOperation(va2,vb2,sigma1,theta1,gamma1,oper)
				 in (case (e1,e2) of
					
					  (Expression(Value(B(b1))),Expression(Value(B(b2)))) => 
						Config(Expression(Value(B(b1 andalso b2))),sigma2,theta2,gamma2)
						
					| (Expression(_),Expression(_)) => 
						Config(Expression(Value(VHole(BinaryOp(BoolOper(EQ),resolveChainSigma(v1,sigma2),resolveChainSigma(v2,sigma2))))),sigma2,theta2,gamma2)
					
					| _ => Config(Stuck,sigma2,theta2,gamma2))
					
				end
				
		else Config(Stuck,sigma,theta,gamma)
		
	| (ValuePair(_,_),VHole(_)) => (case typeof(v1,theta,gamma) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta,gamma)
		| (SOME(pairType),theta1) => (case evaluate(narrow(v2,pairType,sigma,theta1,gamma)) of
		
			  c as Config(Stuck,sigma2,theta2,gamma2) => c
			| Config(Expression(Value(v2narrow)),sigma2,theta2,gamma2) => 
				
				elabPhraseOperation(v1,v2narrow,sigma2,theta2,gamma2,oper)))
		
	| (VHole(_),ValuePair(_,_)) => (case typeof(v2,theta,gamma) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta,gamma)
		| (SOME(pairType),theta1) => (case evaluate(narrow(v1,pairType,sigma,theta1,gamma)) of
		
			  c as Config(Stuck,sigma2,theta2,gamma2) => c
			| Config(Expression(Value(v1narrow)),sigma2,theta2,gamma2) => 
				
				elabPhraseOperation(v1narrow,v2,sigma2,theta2,gamma2,oper)))	
		
	| (VHole(_),VHole(_)) =>
		let val (t,theta1) = (case oper of 
		
			  ArithOper(DIVIDE) => (Real,theta)
			  
			 (* When using typeof in the two clauses below, return the original
			    theta if they are not equal equality/arithmetic type variables *)
			  
			| BoolOper(EQ) => 
				let val (t1,theta1) = typeof(v1,theta,gamma);
					val (t2,theta2) = typeof(v2,theta1,gamma)
				in  (case (t1,t2) of 
				
				  (SOME (THole(TypeHole(EqualityTypeVar(a)))),SOME (THole(TypeHole(EqualityTypeVar(b))))) =>
					if a=b then (THole(TypeHole(EqualityTypeVar(a))),theta2)
					       else (generateFreshTypeVar(EQUALITY_TYPE_VAR,theta),theta)
						   
				| _ => (generateFreshTypeVar(EQUALITY_TYPE_VAR,theta),theta))
				
				end
				
			| _  => 
				let val (t1,theta1) = typeof(v1,theta,gamma);
					val (t2,theta2) = typeof(v2,theta1,gamma)
				in  (case (t1,t2) of 
				
				  (SOME (THole(TypeHole(ArithTypeVar(a)))), SOME (THole(TypeHole(ArithTypeVar(b))))) =>
					if a=b then (THole(TypeHole(ArithTypeVar(a))),theta2)
					       else (generateFreshTypeVar(ARITH_TYPE_VAR,theta),theta)
						   
				| _ => (generateFreshTypeVar(ARITH_TYPE_VAR,theta),theta))
				
				end)
				
		in elabPhraseOperationEvaluate(v1,v2,sigma,theta1,gamma,wrapper,t) end
		
	| _	=> Config(Stuck,sigma,theta,gamma))
		
	end
		
(* No evaluation necessary for a value 
  (context-value) *)
and evaluate (Config(Expression(Value(v)),s,t,g)) =
	Config(Expression(Value(resolveChainSigma(v,s))),s,t,g)

(* Can't evaluate a stuck expression any more *)
(* (context-stuck) *)
|   evaluate (c as Config(Stuck,_,_,_)) = c

(* Cannot evaluate a free variable on its own any further
   (context-var) *)
|  	evaluate (Config(Expression(Variable(x)),s,t,g)) = 
	Config(Expression(Substitution.get(x,g)),s,t,g)

(* Arithmetic operations +,-,*,/ with both arguments as values 
   i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op an arithmetic operator *)
| 	evaluate (Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma,theta,gamma)) =

		elabPhraseOperation(v1,v2,sigma,theta,gamma,ArithOper(oper))
		
(* Boolean operations <,<=,>,>=,= with both arguments as values 
   i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op a boolean operator *)
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma,theta,gamma)) =

		elabPhraseOperation(v1,v2,sigma,theta,gamma,BoolOper(oper))
 
(* (context-op-2) for arithmetic expressions *)	  
|	evaluate (Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e2),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
			  
		| Config(Expression(Value(v2)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma1,theta1,gamma1)))
	
(* (context-op-2) for boolean expressions *)	 
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e2),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
			  
		| Config(Expression(Value(v2)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma1,theta1,gamma1)))
  
(* (context-op-1) for arithmetic expressions *)	 
|	evaluate (Config(Expression(ArithExpr(oper,e1,e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e1),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
			  
		| Config(Expression(Value(v1)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma1,theta1,gamma1)))
	
(* (context-op-1) for boolean expressions *)	
|	evaluate (Config(Expression(BoolExpr(oper,e1,e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e1),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
			  
		| Config(Expression(Value(v1)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma1,theta1,gamma1)))

(* Resolves the slight loophole that <v,v> can be both an expression and a value *)
(* (context-value-pair) *)
|	evaluate (Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma,theta,gamma)) =
		evaluate(Config(Expression(Value(ValuePair(v1,v2))),sigma,theta,gamma))
	
(* Handles (context-pair-2), where left hand expression in pair a value *)
|  	evaluate (Config(Expression(ExpressionPair(Value(v1),e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e2),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
		  
		| Config(Expression(Value(v2)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma1,theta1,gamma1)))

(* Handles (context-pair-1), where both expressions in a pair not a value *)
|  	evaluate (Config(Expression(ExpressionPair(e1,e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e1),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
		  
		| Config(Expression(Value(v1)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),e2)),sigma1,theta1,gamma1)))
	
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2), (E-IF-BAD) *)
|  evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta,gamma)) =

	(case evaluate(narrow(v,Bool,sigma,theta,gamma)) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,_,_,_) => c
		 
		| Config(Expression(Value(B(b))),sigma1,theta1,gamma1) =>
		
			if b 
				 (* rule E-IF-GOOD1 *)
				 then evaluate(Config(Expression(e1),sigma1,theta1,gamma1))
				
				 (* rule E-IF-GOOD2 *)
				 else evaluate(Config(Expression(e2),sigma1,theta1,gamma1))
		
		| Config(Expression(Value(newV)),sigma1,theta1,gamma1) =>
		
			(* rule E-IF-HOLE *)
			Config(Expression(Value(VHole(ConditionHole(newV,e1,e2)))),sigma1,theta1,gamma1))
			
				 
(* (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta,gamma)) =

   	(case evaluate(Config(Expression(e1),sigma,theta,gamma)) of 
		
		  c as Config(Stuck,_,_,_) => c
		  
		| Config(Expression(Value(v1)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(Condition(Value(v1),e2,e3)),sigma1,theta1,gamma1)))
 
(* Implements evaluation & type inference rules for case-pair expression with first operand a value *)
(* i.e. implement rules (E-CASE-PAIR-GOOD) and (E-CASE-PAIR-BAD) *) 
|  evaluate (Config(Expression(Case(Value(v),VariablePair(x1,x2),e)),sigma,theta,gamma)) =

	let val alpha1 = generateFreshTypeVar(TYPE_VAR,theta)
		val alpha2 = generateFreshTypeVar(TYPE_VAR,theta)
		
	in (case evaluate(narrow(v,Pair(alpha1,alpha2),sigma,theta,gamma)) of
			
	  (* rule E-CASE-PAIR-BAD *)
	  c as Config(Stuck,_,_,_) => c
			  
	  (* rule E-CASE-PAIR-GOOD *)
	| Config(Expression(Value(ValuePair(v1,v2))),sigma1,theta1,gamma1) =>
				
		let val gamma2 = append([ (x1,Value(v1)), (x2,Value(v2)) ],gamma1)
		in evaluate(Config(Expression(substitute(e,gamma2)),sigma1,theta1,gamma2)) end)
				
   end
   
(* (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,pat,e2)),sigma,theta,gamma)) =

	(case evaluate(Config(Expression(e1),sigma,theta,gamma)) of 
		
		c as Config(Stuck,_,_,_) => c
			  
		| Config(Expression(Value(v1)),sigma1,theta1,gamma1) =>
			evaluate(Config(Expression(Case(Value(v1),pat,e2)),sigma1,theta1,gamma1)));