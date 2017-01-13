(* Curried function which takes an operator oper, and
   returns a function from a pair of two values to a possibly-stuck expression *)
val rec operationWrap = fn oper =>

   (fn (Concrete(N(n1)),Concrete(N(n2))) => (case oper of 
		  ArithOper(PLUS) 	  => Expression(Value(Concrete(N(n1+n2))))
		| ArithOper(SUBTRACT) => Expression(Value(Concrete(N(n1-n2))))
		| ArithOper(TIMES)    => Expression(Value(Concrete(N(n1*n2))))
		| ArithOper(DIVIDE)   => Stuck
		| BoolOper(LESS)      => Expression(Value(Concrete(B(n1<n2))))
		| BoolOper(LESS_EQ)   => Expression(Value(Concrete(B(n1<=n2))))
		| BoolOper(MORE)      => Expression(Value(Concrete(B(n1>n2))))
		| BoolOper(MORE_EQ)   => Expression(Value(Concrete(B(n1>=n2))))
		| BoolOper(EQ)        => Expression(Value(Concrete(B(n1=n2)))))
		
    | (Concrete(R(r1)),Concrete(R(r2))) => (case oper of 
		  ArithOper(PLUS)     => Expression(Value(Concrete(R(r1+r2))))
		| ArithOper(SUBTRACT) => Expression(Value(Concrete(R(r1-r2))))
		| ArithOper(TIMES)    => Expression(Value(Concrete(R(r1*r2))))
		| ArithOper(DIVIDE)   => Expression(Value(Concrete(R(r1/r2))))
		| BoolOper(LESS)      => Expression(Value(Concrete(B(r1<r2))))
		| BoolOper(LESS_EQ)   => Expression(Value(Concrete(B(r1<=r2))))
		| BoolOper(MORE)      => Expression(Value(Concrete(B(r1>r2))))
		| BoolOper(MORE_EQ)   => Expression(Value(Concrete(B(r1>=r2))))
		| BoolOper(EQ)		  => Stuck)
		 
	| (Concrete(B(b1)), Concrete(B(b2))) => (case oper of 
		  BoolOper(EQ) => Expression(Value(Concrete(B(b1=b2))))
		| _ 		   => Stuck)
	 
	| (VRecord(r1),VRecord(r2)) => (case oper of
	
		  BoolOper(EQ) => 
		    (* Perform operationWrap recursively on each pair of values for corresponding labels
			   If all return boolean, then return conjunction of all booleans
			   Otherwise return as binary op value hole, v[record1 = record2] *)
			let fun iterOperationWrap(l1,l2) = (case (l1,l2) of 
				
				  ([],[]) => Expression(Value(Concrete(B(true))))
				| ([],_)  => Stuck
				| (_,[])  => Stuck
				| ((lab1,v1)::rest1,(lab2,v2)::rest2) =>
					if lab1=lab2
					then (case (operationWrap oper (v1,v2)) of
						
						  Expression(Value(Concrete(B(b1)))) => (case iterOperationWrap(rest1,rest2) of 
								  Expression(Value(Concrete(B(b2)))) => Expression(Value(Concrete(B(b1 andalso b2))))
 								| Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VRecord(r1),VRecord(r2)))))
								| Stuck => Stuck)
						  
						(* If doesn't evaluate to boolean, carrying on evaluating the rest of the
						   equal labels pairs, just in case one of the pairs gets stuck *)
						| Expression(_) => (case iterOperationWrap(rest1,rest2) of 
							  Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VRecord(r1),VRecord(r2)))))
							| Stuck => Stuck)
						
						| Stuck => Stuck)
						
					else Stuck)
		
			in iterOperationWrap(Record.sort(r1),Record.sort(r2)) end
			
		| _ => Stuck)
	
	| (VHole(hole), v2) => Expression(Value(VHole(BinaryOpHole(oper,VHole(hole),v2))))
	 
	| (v1, VHole(hole)) => Expression(Value(VHole(BinaryOpHole(oper,v1,VHole(hole)))))
	
	| _ => Stuck);	
	
(* Rules for arithmetic and boolean operations *)
fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, oper, t) = (case evaluate(narrow(v1,t,sigma,theta,[])) of 
	  
	  Config(Expression(Value(n1)),sigma1,theta1) => (case evaluate(narrow(v2,t,sigma1,theta1,[])) of 

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
	  (Concrete(N(_)),Concrete(N(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
	| (Concrete(N(_)),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
	| (VHole(_),Concrete(N(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int)
		
	  (* op : real * real -> real/bool *)
	| (Concrete(R(_)),Concrete(R(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
	| (Concrete(R(_)),VHole(_))		  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
	| (VHole(_),Concrete(R(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real)
		
	(* = : bool * bool -> bool *)
	| (Concrete(B(_)),Concrete(B(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
	| (Concrete(B(_)),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
	| (VHole(_),Concrete(B(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool)
		
	(* For record arguments, can only be operator equal
	   Recursively call this function on each of the pairs of values for corresponding labels *)
	| (VRecord(r1),VRecord(r2)) =>	
	
		if oper = BoolOper(EQ)
		
		then let fun iterElabEvaluate(l1,l2,sigma,theta) = (case (l1,l2) of 
		
			  ([],[]) => Config(Expression(Value(Concrete(B(true)))),sigma,theta)
			| ([],_)  => Config(Stuck,sigma,theta)
			| (_,[])  => Config(Stuck,sigma,theta)
			| ((lab1,v1)::rest1,(lab2,v2)::rest2) => 
				if lab1=lab2
				then (case elabPhraseOperation(v1,v2,sigma,theta,oper) of
							
					  Config(Expression(Value(Concrete(B(b1)))),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
					  
							  Config(Expression(Value(Concrete(B(b2)))),sigma2,theta2) =>
								Config(Expression(Value(Concrete(B(b1 andalso b2)))),sigma2,theta2)
							
							| Config(Expression(_),sigma2,theta2) =>
								Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
									resolveChainSigma(VRecord(r1),sigma2),
									resolveChainSigma(VRecord(r2),sigma2))))),sigma2,theta2)
									
							| Config(Stuck,sigma2,theta2) => Config(Stuck,sigma2,theta2))
					
					(* If doesn't evaluate to boolean, still go on to evaluate the rest of the equal labels pairs
					   just to have a sigma and gamma to resolveChain the original records
					   before putting into a compound value hole,
					   or to see if another equal labels pair gets stuck *)
					| Config(Expression(_),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
					
						  Config(Expression(_),sigma2,theta2) =>
							Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
								resolveChainSigma(VRecord(r1),sigma2),
								resolveChainSigma(VRecord(r2),sigma2))))),sigma2,theta2)
						  
						| Config(Stuck,sigma2,theta2) => Config(Stuck,sigma2,theta2))
							
					| Config(Stuck,sigma1,theta1) => Config(Stuck,sigma1,theta1))
						
				else Config(Stuck,sigma,theta))
				
		in iterElabEvaluate(Record.sort(r1),Record.sort(r2),sigma,theta) end
		
		else Config(Stuck,sigma,theta)
			
	| (VRecord(_),VHole(_)) => (case typeof(v1,theta) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta)
		| (SOME(recordType),theta1) => (case evaluate(narrow(v2,recordType,sigma,theta1,[])) of 
		
			  Config(Expression(Value(v2narrow)),sigma2,theta2) => 
				elabPhraseOperation(v1,v2narrow,sigma2,theta2,oper)
					
			| Config(_,sigma2,theta2) => Config(Stuck,sigma,theta)))
		
	| (VHole(_),VRecord(_)) => (case typeof(v2,theta) of 
	
		  (NONE,_) => Config(Stuck,sigma,theta)
		| (SOME(recordType),theta1) => (case evaluate(narrow(v1,recordType,sigma,theta1,[])) of 
			
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
|	evaluate (Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma,theta)) = (case evaluate(Config(Expression(e2),sigma,theta)) of 
		
	  Config(Expression(Value(v2)),sigma1,theta1) =>
		evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
			
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* (context-op-2) for boolean expressions *)	 
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma,theta)) = (case evaluate(Config(Expression(e2),sigma,theta)) of 
		
	  Config(Expression(Value(v2)),sigma1,theta1) =>
		evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma1,theta1))
			
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
  
(* (context-op-1) for arithmetic expressions *)	 
|	evaluate (Config(Expression(ArithExpr(oper,e1,e2)),sigma,theta)) = (case evaluate(Config(Expression(e1),sigma,theta)) of 
		  
	 Config(Expression(Value(v1)),sigma1,theta1) =>
		evaluate(Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma1,theta1))
			
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* (context-op-1) for boolean expressions *)	
|	evaluate (Config(Expression(BoolExpr(oper,e1,e2)),sigma,theta)) = (case evaluate(Config(Expression(e1),sigma,theta)) of 
		
	  Config(Expression(Value(v1)),sigma1,theta1) =>
		evaluate(Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma1,theta1))
			
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

(* Takes a record of expressions and turns into a value record of values
   Label-expression pairs evaluated in a left-to-right manner
   returning list of label-value pairs *)
(* (context-record) *)
|	evaluate (Config(Expression(Record(r)),sigma,theta)) = 

	let fun iterEvaluate(r,sigma,theta) = (case r of 
	
		  [] => (SOME [],sigma,theta)
		  
		| (lab,Value(v))::rest => (case iterEvaluate(rest,sigma,theta) of 
		
			  (SOME l,sigma1,theta1) => (SOME ((lab,v)::l),sigma1,theta1)
			| (NONE,sigma1,theta1)   => (NONE,sigma1,theta1))
		
		| (lab,e)::rest => (case evaluate(Config(Expression(e),sigma,theta)) of 
		
			  Config(Expression(Value(v)),sigma1,theta1) => (case iterEvaluate(rest,sigma1,theta1) of 
			  
				  (SOME l,sigma2,theta2) => (SOME ((lab,v)::l),sigma2,theta2)
				| (NONE,sigma2,theta2)   => (NONE,sigma2,theta2))
			 
			| Config(_,sigma1,theta1) => (NONE,sigma1,theta1)))
				
	in (case iterEvaluate(r,sigma,theta) of 
	
		  (SOME l,sigma1,theta1) => evaluate(Config(Expression(Value(VRecord(l))),sigma1,theta1))
		| (NONE,sigma1,theta1)   => Config(Stuck,sigma1,theta1))
		
	end
		  
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2), (E-IF-BAD) *)
|  evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) =

	(case evaluate(narrow(v,Bool,sigma,theta,[])) of 
	
		  Config(Expression(Value(Concrete(B(b)))),sigma1,theta1) =>
		
			if b 
				 (* rule E-IF-GOOD1 *)
				 then evaluate(Config(Expression(e1),sigma1,theta1))
				
				 (* rule E-IF-GOOD2 *)
				 else evaluate(Config(Expression(e2),sigma1,theta1))
		
		(* rule E-IF-HOLE *)
		| Config(Expression(Value(newV)),sigma1,theta1) =>
			evaluate(Config(Expression(Value(VHole(ConditionHole(newV,e1,e2)))),sigma1,theta1))

		(* rule E-IF-BAD *)
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
			
(* (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta)) =

   	(case evaluate(Config(Expression(e1),sigma,theta)) of 
		
		  Config(Expression(Value(v1)),sigma1,theta1) =>
			evaluate(Config(Expression(Condition(Value(v1),e2,e3)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
 
(* Implements evaluation & type inference rules for case expression with first operand a value *)
(* i.e. implement rules (E-CASE-GOOD) and (E-CASE-BAD) *) 
|  evaluate (Config(Expression(c as Case(Value(v),patExprList)),sigma,theta)) =
	
	(* Narrow the whole expression to some type variable. This 
	   a) narrows the value we are case-ing on
	   b) checks type of value and type of all patterns agree (i.e. matchTypesLists)
	   c) type of all expression branches agree *)
	   
	(* Don't follow this call to narrowExpr by evaluate like other calls to narrow
	   We want the case expression back (hence why matching non-exhaustive below *)
	(case narrowExpr(c,generateFreshTypeVar(TYPE_VAR,theta),sigma,theta,[]) of 

		  Config(Expression(Case(v1narrow,patExprList)),sigma1,theta1) => (case evaluate(Config(Expression(v1narrow),sigma1,theta1)) of 
		  
			  Config(Expression(v1narrow),sigma1,theta1) => (case match(v1narrow,patExprList,sigma1,theta1,[]) of 
		  
				(* E-CASE-BAD1 *)
				  Fail => Config(Stuck,sigma1,theta1)
				  
				(* E-CASE-HOLE *)
				| Hole h => evaluate(Config(Expression(Value(VHole(CaseHole(VHole(h),patExprList)))),sigma1,theta1))
				
				(* E-CASE-GOOD *)
				| Success (expr,sigma2,theta2,gamma) => evaluate(Config(Expression(substitute(expr,gamma)),sigma2,theta2)))
				
			| _ => Config(Stuck,sigma1,theta1))
					
		(* E-CASE-BAD2 *)
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
   
(* (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,patExprList)),sigma,theta)) = (case evaluate(Config(Expression(e1),sigma,theta)) of 
	
		  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Case(Value(v1),patExprList)),sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

(* Implements evaluation & type inference rules for application expression with both operands a value *)
(* i.e. implement rules (E-APP-GOOD) and (E-APP-BAD) *) 			
| 	evaluate (Config(Expression(App(Value(v1),Value(v2))),sigma,theta)) = (case typeof(v2,theta) of 
	
	  (NONE,_) => Config(Stuck,sigma,theta)
	| (SOME t1,theta1) => 
				
		let val freshType = generateFreshTypeVar(TYPE_VAR,theta1);
			val narrowType = TFun(t1,freshType);
				
		in (case evaluate(narrow(v1,narrowType,sigma,theta1,[])) of
				
			  (* Rule E-APP-GOOD *)
			  Config(Expression(Value(Fun(x,t,e))),sigma2,theta2) =>
				evaluate(Config(Expression(substitute(e,[(x,Value(v2))])),sigma2,theta2))
					
			(* Rule E-APP-HOLE *)
			| Config(Expression(Value(v1narrow)),sigma2,theta2) => 
				evaluate(Config(Expression(Value(VHole(AppHole(v1narrow,v2)))),sigma2,theta2))
					
			(* rule E-APP-BAD *)
			| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
				
		end)

(* context-app-1 *)
| 	evaluate (Config(Expression(App(Value(v1),e2)),sigma,theta)) = (case evaluate(Config(Expression(e2),sigma,theta)) of 
		
	 Config(Expression(Value(v2)),sigma1,theta1) => evaluate(Config(Expression(App(Value(v1),Value(v2))),sigma1,theta1))
			
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
			
(* context-app-2 *)
| 	evaluate (Config(Expression(App(e1,e2)),sigma,theta)) = (case evaluate(Config(Expression(e1),sigma,theta)) of

	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(App(Value(v1),e2)),sigma1,theta1))

	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* Implements evaluation & type inference rules for let expressions with e1 a value *)
(* i.e. implements rules (E-LET-GOOD) and (E-LET-BAD) *)
|	evaluate (Config(Expression(Let(x,t,Value(v1),e2)),sigma,theta)) = (case evaluate(narrow(v1,t,sigma,theta,[])) of 

	  Config(Expression(Value(v1narrow)),sigma1,theta1) => evaluate(Config(Expression(substitute(e2,[(x,Value(v1narrow))])),sigma1,theta1))
	  
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* context-let *)
|	evaluate (Config(Expression(Let(x,t,e1,e2)),sigma,theta)) = (case evaluate(Config(Expression(e1),sigma,theta)) of 

	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Let(x,t,Value(v1),e2)),sigma1,theta1))
	 
	| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))
	
(* Implements evaluation & type inference rules for let-rec expressions *)
(* i.e. implements rules (E-LET-REC-GOOD) and (E-LET-REC-BAD) *)
|	evaluate (Config(Expression(l as LetRec(x,TFun(t1,t2),Fun(y,t3,e1),e2)),sigma,theta)) = 

	(* Narrow whole expression to some general type variable, similarly to case expression
	   This will, for example, check types t1 and t3 unify, or that e1 is of type t2, etc. *)
	   
	(* Don't follow this call to narrowExpr by evaluate like other calls to narrow
	   We want the case expression back (hence why matching non-exhaustive below *)
	   
	(case narrowExpr(l,generateFreshTypeVar(TYPE_VAR,theta),sigma,theta,[]) of 
	
		  Config(Expression(l as LetRec(x,tfun,Fun(y,t3,e1),e2)),sigma1,theta1) => 
		  
			evaluate(Config(Expression(substitute(e2,[(x,Value(Fun(y,t3,LetRec(x,tfun,Fun(y,t3,e1),e1))))])),
						    sigma1,theta1))
			
		| Config(_,sigma1,theta1) => Config(Stuck,sigma1,theta1))

| 	evaluate (Config(Expression(LetRec(_,_,_,_)),sigma,theta)) = Config(Stuck,sigma,theta);		