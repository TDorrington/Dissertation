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
fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, oper, t) : config = 
	
	(case evaluate(narrow(v1,t,sigma,theta)) of 
	  
		  Config(Expression(Value(n1)),sigma1,theta1) => (case evaluate(narrow(v2,t,sigma1,theta1)) of 

				  (* rule E-OP-GOOD *)
				  Config(Expression(Value(n2)),sigma2,theta2) => Config(oper(n1,n2),sigma2,theta2)
				
				 (* rule E-OP-BAD2 *)
				| Config(_,sigma2,theta2) => Config(Stuck,sigma2,theta2))
		
		 (* rule E-OP-BAD1 *)
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
						
and elabPhraseOperation(v1,v2,sigma,theta,oper) =
	
	let val wrapper = operationWrap(oper) 
	in (case (v1,v2) of 
	
	  (* op : int * int -> int/bool *)
	  (N(_),N(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
	| (N(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
	| (VHole(_),N(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
		
	  (* op : real * real -> real/bool *)
	| (R(_),R(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
	| (R(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
	| (VHole(_),R(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
		
	(* = : bool * bool -> bool *)
	| (B(_),B(_))     => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
	| (B(_),VHole(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
	| (VHole(_),B(_)) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
		
	(* For value pair arguments, can only be operator equal
	   Recursively call this function on each of the pair arguments *)
	| (ValuePair(va1,va2),ValuePair(vb1,vb2)) => 
		if oper = BoolOper(EQ) 
		then let val Config(e1,sigma1,theta1) = elabPhraseOperation(va1,vb1,sigma,theta,oper);
			     val Config(e2,sigma2,theta2) = elabPhraseOperation(va2,vb2,sigma1,theta1,oper)
				 in (case (e1,e2) of
					
					  (Expression(Value(B(b1))),Expression(Value(B(b2)))) => 
						Config(Expression(Value(B(b1 andalso b2))),sigma2,theta2)
						
					| (Expression(_),Expression(_)) => 
						Config(Expression(Value(VHole(BinaryOp(BoolOper(EQ),resolveChainSigma(v1,sigma2),resolveChainSigma(v2,sigma2))))),sigma2,theta2)
					
					| _ => Config(Stuck,sigma2,theta2))
					
				end
				
		else Config(Stuck,sigma,theta)
		
	| (ValuePair(_,_),VHole(_)) => (case typeof(v1,theta) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta)
		| (SOME(pairType),theta1) => 
		
			(case evaluate(narrow(v2,pairType,sigma,theta1)) of 
		
				  Config(Expression(Value(v2narrow)),sigma2,theta2) => 
					elabPhraseOperation(v1,v2narrow,sigma2,theta2,oper)
					
				| Config(_,sigma2,theta2) => Config(Stuck,sigma,theta)))
		
	| (VHole(_),ValuePair(_,_)) => (case typeof(v2,theta) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta)
		| (SOME(pairType),theta1) => 
		
			(case evaluate(narrow(v1,pairType,sigma,theta1)) of 
			
			  Config(Expression(Value(v1narrow)),sigma2,theta2) => 
				elabPhraseOperation(v1narrow,v2,sigma2,theta2,oper)
				
			| Config(_,sigma2,theta2) => Config(Stuck,sigma,theta)))
			
	| (VHole(_),VHole(_)) =>
		let val (t,theta1) = (case oper of 
		
			  ArithOper(DIVIDE) => (Real,theta)
			  
			 (* When using typeof in the two clauses below, return the original
			    theta if they are not equal equality/arithmetic type variables *)
			  
			| BoolOper(EQ) => 
				let val (t1,theta1) = typeof(v1,theta);
					val (t2,theta2) = typeof(v2,theta1)
				in  (case (t1,t2) of 
				
				  (SOME (THole(TypeHole(EqualityTypeVar(a)))),SOME (THole(TypeHole(EqualityTypeVar(b))))) =>
					if a=b then (THole(TypeHole(EqualityTypeVar(a))),theta2)
					       else (generateFreshTypeVar(EQUALITY_TYPE_VAR,theta),theta)
						   
				| _ => (generateFreshTypeVar(EQUALITY_TYPE_VAR,theta),theta))
				
				end
				
			| _  => 
				let val (t1,theta1) = typeof(v1,theta);
					val (t2,theta2) = typeof(v2,theta1)
				in  (case (t1,t2) of 
				
				  (SOME (THole(TypeHole(ArithTypeVar(a)))), SOME (THole(TypeHole(ArithTypeVar(b))))) =>
					if a=b then (THole(TypeHole(ArithTypeVar(a))),theta2)
					       else (generateFreshTypeVar(ARITH_TYPE_VAR,theta),theta)
						   
				| _ => (generateFreshTypeVar(ARITH_TYPE_VAR,theta),theta))
				
				end)
				
		in elabPhraseOperationEvaluate(v1,v2,sigma,theta1,wrapper,t) end
		
	| _	=> Config(Stuck,sigma,theta))
		
	end
		
(* No evaluation necessary for a value 
  (context-value) *)
and evaluate (Config(Expression(Value(v)),s,t)) =
	Config(Expression(Value(resolveChainSigma(v,s))),s,t)

(* Can't evaluate a stuck expression any more *)
(* (context-stuck) *)
|   evaluate (c as Config(Stuck,_,_)) = c

(* Cannot evaluate a free variable on its own any further *)
|  	evaluate (Config(Expression(Variable(x)),s,t)) = raise FreeVariable

(* Arithmetic operations +,-,*,/ with both arguments as values 
   i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op an arithmetic operator *)
| 	evaluate (Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma,theta)) =

		elabPhraseOperation(v1,v2,sigma,theta,ArithOper(oper))
		
(* Boolean operations <,<=,>,>=,= with both arguments as values 
   i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op a boolean operator *)
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma,theta)) =

		elabPhraseOperation(v1,v2,sigma,theta,BoolOper(oper))
 
(* (context-op-2) for arithmetic expressions *)	  
|	evaluate (Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		  Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* (context-op-2) for boolean expressions *)	 
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		  Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
  
(* (context-op-1) for arithmetic expressions *)	 
|	evaluate (Config(Expression(ArithExpr(oper,e1,e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e1),sigma,theta)) of 
			  
		 Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* (context-op-1) for boolean expressions *)	
|	evaluate (Config(Expression(BoolExpr(oper,e1,e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

(* Resolves the slight loophole that <v,v> can be both an expression and a value *)
(* (context-value-pair) *)
|	evaluate (Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma,theta)) =

		evaluate(Config(Expression(Value(ValuePair(v1,v2))),sigma,theta))
	
(* Handles (context-pair-2), where left hand expression in pair a value *)
|  	evaluate (Config(Expression(ExpressionPair(Value(v1),e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		  Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

(* Handles (context-pair-1), where both expressions in a pair not a value *)
|  	evaluate (Config(Expression(ExpressionPair(e1,e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(ExpressionPair(Value(v1),e2)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2), (E-IF-BAD) *)
|  evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) =

	(case evaluate(narrow(v,Bool,sigma,theta)) of 
	
		  Config(Expression(Value(B(b))),sigma1,theta1) =>
		
			if b 
				 (* rule E-IF-GOOD1 *)
				 then evaluate(Config(Expression(e1),sigma1,theta1))
				
				 (* rule E-IF-GOOD2 *)
				 else evaluate(Config(Expression(e2),sigma1,theta1))
		
		(* rule E-IF-HOLE *)
		| Config(Expression(Value(newV)),sigma1,theta1) =>
			Config(Expression(Value(VHole(ConditionHole(newV,e1,e2)))),sigma1,theta1)

		(* rule E-IF-BAD *)
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
			
(* (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta)) =

   	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(Condition(Value(v1),e2,e3)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
 
(* Implements evaluation & type inference rules for case-pair expression with first operand a value *)
(* i.e. implement rules (E-CASE-PAIR-GOOD) and (E-CASE-PAIR-BAD) *) 
|  evaluate (Config(Expression(Case(Value(v),VariablePair(x1,x2),e)),sigma,theta)) =

	let val alpha1 = generateFreshTypeVar(TYPE_VAR,theta);
		val alpha2 = generateFreshTypeVar(TYPE_VAR,theta);
	in (case evaluate(narrow(v,Pair(alpha1,alpha2),sigma,theta)) of 

		  (* rule E-CASE-PAIR-GOOD *)
		  Config(Expression(Value(ValuePair(v1,v2))),sigma1,theta1) =>
					
			let val gamma = [ (x1,Value(v1)), (x2,Value(v2)) ]
			in evaluate(Config(Expression(substitute(e,gamma)),sigma1,theta1)) end
			
		(* rule E-CASE-HOLE *)
		| Config(Expression(Value(v1)),sigma1,theta1) =>
		
			Config(Expression(Case(Value(v1),VariablePair(x1,x2),e)),sigma1,theta1)
			
		(* rule E-CASE-PAIR-BAD *)
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
		
   end
   
(* (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,pat,e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e1),sigma,theta)) of 
	
		  Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(Case(Value(v1),pat,e2)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

(* Implements evaluation & type inference rules for application expression with both operands a value *)
(* i.e. implement rules (E-APP-GOOD) and (E-APP-BAD) *) 			
| 	evaluate (Config(Expression(App(Value(v1),Value(v2))),sigma,theta)) =

	(case typeof(v2,theta) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta)
		| (SOME t1,theta1) => 
				
			let val freshType = generateFreshTypeVar(TYPE_VAR,theta1);
				val narrowType = Fun(t1,freshType);
				
			in (case evaluate(narrow(v1,narrowType,sigma,theta1)) of
				
				  (* Rule E-APP-GOOD *)
				  Config(Expression(Value(Func(x,t,e))),sigma2,theta2) =>
					evaluate(Config(Expression(substitute(e,[(x,Value(v2))])),sigma2,theta2))
					
				(* Rule E-APP-HOLE *)
				| Config(Expression(Value(v1narrow)),sigma2,theta2) => 
					Config(Expression(Value(VHole(AppHole(v1narrow,v2)))),sigma2,theta2)
					
				(* rule E-APP-BAD *)
				| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
				
			end)

(* context-app-1 *)
| 	evaluate (Config(Expression(App(Value(v1),e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e2),sigma,theta)) of 
		
		 Config(Expression(Value(v2)),sigma1,theta1) =>
			evaluate(Config(Expression(App(Value(v1),Value(v2))),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
			
(* context-app-2 *)
| 	evaluate (Config(Expression(App(e1,e2)),sigma,theta)) =

	(case evaluate(Config(Expression(e1),sigma,theta)) of

		 Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(App(Value(v1),e2)),sigma1,theta1))
	
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1));