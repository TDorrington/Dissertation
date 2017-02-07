(* Curried function which takes an operator oper, and
   returns a function from a pair of two values to a possibly-stuck expression *)
val rec operationWrap = fn (oper,cntr) =>

   (fn (Concrete(N(n1)),Concrete(N(n2))) => (case oper of 
		  ArithOper(PLUS) 	  => Expression(Value(Concrete(N(n1+n2))))
		| ArithOper(SUBTRACT) => Expression(Value(Concrete(N(n1-n2))))
		| ArithOper(TIMES)    => Expression(Value(Concrete(N(n1*n2))))
		| ArithOper(DIVIDE)   => Stuck(cntr)
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
		| BoolOper(EQ)		  => Stuck(cntr))
		 
	| (Concrete(B(b1)), Concrete(B(b2))) => (case oper of 
		  BoolOper(EQ) => Expression(Value(Concrete(B(b1=b2))))
		| _ 		   => Stuck(cntr))
	 
	| (VRecord(r1),VRecord(r2)) => (case oper of
	
		  BoolOper(EQ) => 
		  
		    (* Perform operationWrap recursively on each pair of values for corresponding labels
			   If all return boolean, then return conjunction of all booleans
			   Otherwise return as binary op value hole, v[record1 = record2] (or stuck) *)
			let fun iterOperationWrap(l1,l2) = (case (l1,l2) of 
				
				  ([],[]) => Expression(Value(Concrete(B(true))))
				| ([],_)  => Stuck(cntr)
				| (_,[])  => Stuck(cntr)
				| ((lab1,v1)::rest1,(lab2,v2)::rest2) =>
				
					if lab1=lab2
					
					then (case (operationWrap (oper,cntr) (v1,v2)) of
						
						  Expression(Value(Concrete(B(b1)))) => (case iterOperationWrap(rest1,rest2) of 
						  
							  Expression(Value(Concrete(B(b2)))) => Expression(Value(Concrete(B(b1 andalso b2))))
 							| Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VRecord(r1),VRecord(r2)))))
							| Stuck(i)      => Stuck(i))
						  
						(* If doesn't evaluate to boolean, carrying on evaluating the rest of the
						   equal labels pairs, just in case one of the pairs gets stuck *)
						| Expression(_) => (case iterOperationWrap(rest1,rest2) of 
						
							  Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VRecord(r1),VRecord(r2)))))
							| Stuck(i)      => Stuck(i))
						
						| Stuck(i) => Stuck(i))
						
					else Stuck(cntr))
		
			in iterOperationWrap(Record.sort(r1),Record.sort(r2)) end
			
		| _ => Stuck(cntr))
	
	| (Concrete(EmptyList),Concrete(EmptyList)) => (case oper of 
	
		  BoolOper(EQ) => Expression(Value(Concrete(B(true))))
		| _			   => Stuck(cntr))
	
	| (Concrete(EmptyList),VList(_)) => (case oper of 
	
		  BoolOper(EQ) => Expression(Value(Concrete(B(false))))
		| _			   => Stuck(cntr))
		
	| (VList(_),Concrete(EmptyList)) => (case oper of 
	
		  BoolOper(EQ) => Expression(Value(Concrete(B(false))))
		| _			   => Stuck(cntr))
	
	| (VList(l1),VList(l2)) => (case oper of 
	
		  BoolOper(EQ) =>
		  
		    (* Perform operationWrap recursively on each pair of corresponding values in list
			   If all return boolean, then return conjunction of all booleans
			   Otherwise return as binary op value hole, v[list1 = list2] (or stuck) *)
			let fun iterOperationWrap(l1,l2) = (case (l1,l2) of 
				
				  ([],[]) => Expression(Value(Concrete(B(true))))
				  
				(* Unlike records, lists do not have to be the same length, e.g. [1,2] = [1] => false *)
				| ([],_)  => Expression(Value(Concrete(B(false))))
				| (_,[])  => Expression(Value(Concrete(B(false))))
				
				|(v1::rest1,v2::rest2) => (case (operationWrap (oper,cntr) (v1,v2)) of
					
					  Expression(Value(Concrete(B(b1)))) => (case iterOperationWrap(rest1,rest2) of
					  
						  Expression(Value(Concrete(B(b2)))) => Expression(Value(Concrete(B(b1 andalso b2))))
						| Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VList(l1),VList(l2)))))
						| Stuck(i)      => Stuck(i))
						
					(* If doesn't evaluate to boolean, carrying on evaluating the rest of the
					   equal labels pairs, just in case one of the pairs gets stuck *)
					| Expression(_) => (case iterOperationWrap(rest1,rest2) of
					
						  Expression(_) => Expression(Value(VHole(BinaryOpHole(oper,VList(l1),VList(l2)))))
						| Stuck(i)      => Stuck(i))
						
					| Stuck(i) => Stuck(i)))
					
			in iterOperationWrap(l1,l2) end
		  
		| _ => Stuck(cntr))
	
	| (VHole(hole), v2) => Expression(Value(VHole(BinaryOpHole(oper,VHole(hole),v2))))
	 
	| (v1, VHole(hole)) => Expression(Value(VHole(BinaryOpHole(oper,v1,VHole(hole)))))
	
	| _ => Stuck(cntr));	
	
(* Rules for arithmetic and boolean operations *)
fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, oper, t, cntr) = (case evaluate(narrow(v1,t,sigma,theta,[],cntr),cntr) of 
	  
	  Config(Expression(Value(n1)),sigma1,theta1) => (case evaluate(narrow(v2,t,sigma1,theta1,[],cntr),cntr) of 

		  (* rule E-OP-GOOD *)
		  Config(Expression(Value(n2)),sigma2,theta2) => Config(oper(n1,n2),sigma2,theta2)
				
		 (* rule E-OP-BAD2 *)
		| Config(c,sigma2,theta2) => Config(c,sigma2,theta2))
		
	 (* rule E-OP-BAD1 *)
	| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
						
and elabPhraseOperation(v1,v2,sigma,theta,oper,cntr) =
	
	let val wrapper = operationWrap(oper,cntr)
	in (case (v1,v2) of 
	
	  (* op : int * int -> int/bool *)
	  (Concrete(N(_)),Concrete(N(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int,cntr)
	| (Concrete(N(_)),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int,cntr)
	| (VHole(_),Concrete(N(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Int,cntr)
		
	  (* op : real * real -> real/bool *)
	| (Concrete(R(_)),Concrete(R(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real,cntr)
	| (Concrete(R(_)),VHole(_))		  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real,cntr)
	| (VHole(_),Concrete(R(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Real,cntr)
		
	(* = : bool * bool -> bool *)
	| (Concrete(B(_)),Concrete(B(_))) => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool,cntr)
	| (Concrete(B(_)),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool,cntr)
	| (VHole(_),Concrete(B(_))) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta,wrapper,Bool,cntr)
		
	(* For record arguments, can only be operator equal
	   Recursively call this function on each of the pairs of values for corresponding labels *)
	| (VRecord(r1),VRecord(r2)) =>	
	
		if oper = BoolOper(EQ)
		
		then let fun iterElabEvaluate(l1,l2,sigma,theta) = (case (l1,l2) of 
		
			  ([],[]) => Config(Expression(Value(Concrete(B(true)))),sigma,theta)
			| ([],_)  => Config(Stuck(cntr),sigma,theta)
			| (_,[])  => Config(Stuck(cntr),sigma,theta)
			| ((lab1,v1)::rest1,(lab2,v2)::rest2) => 
				if lab1=lab2
				then (case elabPhraseOperation(v1,v2,sigma,theta,oper,cntr) of
							
					  Config(Expression(Value(Concrete(B(b1)))),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
					  
							  Config(Expression(Value(Concrete(B(b2)))),sigma2,theta2) => Config(Expression(Value(Concrete(B(b1 andalso b2)))),sigma2,theta2)
							
							| Config(Expression(_),sigma2,theta2) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
																			resolveChainSigma(VRecord(r1),sigma2),
																			resolveChainSigma(VRecord(r2),sigma2))))),sigma2,theta2)
									
							| Config(Stuck(i),sigma2,theta2) => Config(Stuck(i),sigma2,theta2))
					
					(* If doesn't evaluate to boolean, still go on to evaluate the rest of the equal labels pairs
					   just to have a sigma and gamma to resolveChain the original records
					   before putting into a compound value hole,
					   or to see if another equal labels pair gets stuck *)
					| Config(Expression(_),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
					
						  Config(Expression(_),sigma2,theta2) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
																		resolveChainSigma(VRecord(r1),sigma2),
																		resolveChainSigma(VRecord(r2),sigma2))))),sigma2,theta2)
						  
						| Config(Stuck(i),sigma2,theta2) => Config(Stuck(i),sigma2,theta2))
							
					| Config(Stuck(i),sigma1,theta1) => Config(Stuck(i),sigma1,theta1))
						
				else Config(Stuck(cntr),sigma,theta))
				
		in iterElabEvaluate(Record.sort(r1),Record.sort(r2),sigma,theta) end
		
		else Config(Stuck(cntr),sigma,theta)
			
	| (VRecord(_),VHole(_)) => (case typeof(v1,theta) of 
	
		  (NONE,_) 					=> Config(Stuck(cntr),sigma,theta)
		| (SOME(recordType),theta1) => (case evaluate(narrow(v2,recordType,sigma,theta1,[],cntr),cntr) of 
		
			  Config(Expression(Value(v2narrow)),sigma2,theta2) => elabPhraseOperation(v1,v2narrow,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2) 							=> Config(c,sigma2,theta2)))
		
	| (VHole(_),VRecord(_)) => (case typeof(v2,theta) of 
	
		  (NONE,_) 					=> Config(Stuck(cntr),sigma,theta)
		| (SOME(recordType),theta1) => (case evaluate(narrow(v1,recordType,sigma,theta1,[],cntr),cntr) of 
			
			  Config(Expression(Value(v1narrow)),sigma2,theta2) => elabPhraseOperation(v1narrow,v2,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2) 							=> Config(c,sigma2,theta2)))
	
	| (Concrete(EmptyList),Concrete(EmptyList)) =>
	
		if oper = BoolOper(EQ)
		then Config(Expression(Value(Concrete(B(true)))),sigma,theta)
		else Config(Stuck(cntr),sigma,theta)
		
	| (Concrete(EmptyList),VList(_)) =>
	
		if oper = BoolOper(EQ)
		then Config(Expression(Value(Concrete(B(false)))),sigma,theta)
		else Config(Stuck(cntr),sigma,theta)
	
	| (VList(_),Concrete(EmptyList)) =>
		
		if oper = BoolOper(EQ)
		then Config(Expression(Value(Concrete(B(false)))),sigma,theta)
		else Config(Stuck(cntr),sigma,theta)
		
	| (Concrete(EmptyList),VHole(_)) => (case typeof(v1,theta) of 
	
		  (NONE,_) 				  => Config(Stuck(cntr),sigma,theta)
		| (SOME(listType),theta1) => (case evaluate(narrow(v2,listType,sigma,theta1,[],cntr),cntr) of
			
			  Config(Expression(Value(v2narrow)),sigma2,theta2) => elabPhraseOperation(v1,v2narrow,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2)				    		=> Config(c,sigma2,theta2)))

	| (VHole(_),Concrete(EmptyList)) => (case typeof(v2,theta) of 
	
		  (NONE,_) 				  => Config(Stuck(cntr),sigma,theta)
		| (SOME(listType),theta1) => (case evaluate(narrow(v1,listType,sigma,theta1,[],cntr),cntr) of
		
			  Config(Expression(Value(v1narrow)),sigma2,theta2) => elabPhraseOperation(v1narrow,v2,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2) 							=> Config(c,sigma2,theta2)))
	
	(* For list arguments, can only be operator equal
	   Recursively call this function on each of the corresponding pairs of values *)
	| (VList(l1),VList(l2)) => 
	
		if oper = BoolOper(EQ)
		
		then let fun iterElabEvaluate(l1,l2,sigma,theta) = (case (l1,l2) of 
		
			  ([],[]) => Config(Expression(Value(Concrete(B(true)))),sigma,theta)
			
			(* Unlike records, lists can be different lengths *)
			| ([],_)  => Config(Expression(Value(Concrete(B(false)))),sigma,theta)
			| (_,[])  => Config(Expression(Value(Concrete(B(false)))),sigma,theta)
	
			| (v1::rest1,v2::rest2) => (case elabPhraseOperation(v1,v2,sigma,theta,oper,cntr) of 
			
				  Config(Expression(Value(Concrete(B(b1)))),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
				  
					  Config(Expression(Value(Concrete(B(b2)))),sigma2,theta2) => Config(Expression(Value(Concrete(B(b1 andalso b2)))),sigma2,theta2)
					  
					| Config(Expression(_),sigma2,theta2) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
																	resolveChainSigma(VList(l1),sigma2),
																	resolveChainSigma(VList(l2),sigma2))))),sigma2,theta2)
																										
					| Config(Stuck(i),sigma2,theta2) => Config(Stuck(i),sigma2,theta2))
					
				| Config(Expression(_),sigma1,theta1) => (case iterElabEvaluate(rest1,rest2,sigma1,theta1) of 
				
					  Config(Expression(_),sigma2,theta2) => Config(Expression(Value(VHole(BinaryOpHole(BoolOper(EQ),
																	resolveChainSigma(VList(l1),sigma2),
																	resolveChainSigma(VList(l2),sigma2))))),sigma2,theta2)
					
					| Config(Stuck(i),sigma2,theta2) => Config(Stuck(i),sigma2,theta2))
					
				| Config(Stuck(i),sigma1,theta1) => Config(Stuck(i),sigma1,theta1)))
				
			in iterElabEvaluate(l1,l2,sigma,theta) end
				
		else Config(Stuck(cntr),sigma,theta)
	
	| (VList(_),VHole(_)) => (case typeof(v1,theta) of 
	
		  (NONE,_) 				  => Config(Stuck(cntr),sigma,theta)
		| (SOME(listType),theta1) => (case evaluate(narrow(v2,listType,sigma,theta1,[],cntr),cntr) of
			
			  Config(Expression(Value(v2narrow)),sigma2,theta2) => elabPhraseOperation(v1,v2narrow,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2) 							=> Config(c,sigma2,theta2)))
			
	| (VHole(_),VList(_)) => (case typeof(v2,theta) of 
	
		  (NONE,_) 				  => Config(Stuck(cntr),sigma,theta)
		| (SOME(listType),theta1) => (case evaluate(narrow(v1,listType,sigma,theta1,[],cntr),cntr) of
		
			  Config(Expression(Value(v1narrow)),sigma2,theta2) => elabPhraseOperation(v1narrow,v2,sigma2,theta2,oper,cntr)
			| Config(c,sigma2,theta2) 							=> Config(c,sigma2,theta2)))
	
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
				
		in elabPhraseOperationEvaluate(v1,v2,sigma,theta1,wrapper,t,cntr) end
		
	| _	=> Config(Stuck(cntr),sigma,theta))
		
	end
		
(* No evaluation necessary for a value: (context-value) *)
and evaluate (Config(Expression(Value(v)),s,t),_) = Config(Expression(Value(resolveChainSigma(v,s))),s,t)

(* Can't evaluate a stuck expression any more:(context-stuck) *)
|   evaluate (c as Config(Stuck(_),_,_),_) = c

| 	evaluate (Config(Expression(CounterExpr(e,i)),s,t),_) = evaluate (Config(Expression(e),s,t),i) 

(* Cannot evaluate a free variable on its own any further *)
|  	evaluate (Config(Expression(Variable(x)),s,t),_) = raise FreeVariable

(* Arithmetic operations +,-,*,/ with both arguments as values, i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for arithmetic operator *)
| 	evaluate (Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma,theta),cntr) = elabPhraseOperation(v1,v2,sigma,theta,ArithOper(oper),cntr)
		
(* Boolean operations <,<=,>,>=,= with both arguments as values, i.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for boolean operator *)
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma,theta),cntr) = elabPhraseOperation(v1,v2,sigma,theta,BoolOper(oper),cntr)
 
(* (context-op-2) for arithmetic expressions *)	  
|	evaluate (Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e2),sigma,theta),cntr) of 
		
	  Config(Expression(Value(v2)),sigma1,theta1) => evaluate(Config(Expression(ArithExpr(oper,Value(v1),Value(v2))),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
	
(* (context-op-2) for boolean expressions *)	 
|	evaluate (Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e2),sigma,theta),cntr) of 
		
	  Config(Expression(Value(v2)),sigma1,theta1) => evaluate(Config(Expression(BoolExpr(oper,Value(v1),Value(v2))),sigma1,theta1),cntr)	
	| Config(c,sigma1,theta1) 			 		  => Config(c,sigma1,theta1))
  
(* (context-op-1) for arithmetic expressions *)	 
|	evaluate (Config(Expression(ArithExpr(oper,e1,e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 
		  
	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(ArithExpr(oper,Value(v1),e2)),sigma1,theta1),cntr)	
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
	
(* (context-op-1) for boolean expressions *)	
|	evaluate (Config(Expression(BoolExpr(oper,e1,e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 
		
	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(BoolExpr(oper,Value(v1),e2)),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))

(* Takes a record of expressions and turns into a value record of values
   Label-expression pairs evaluated in a left-to-right manner
   returning list of label-value pairs: (context-record) *)
|	evaluate (Config(Expression(Record(r)),sigma,theta),cntr) = 

	let datatype result = Fail of int | Success of (lab * v) list;
	
		fun iterEvaluate(r,sigma,theta) = (case r of 
	
		  [] => (Success ([]),sigma,theta)
		  
		| (lab,Value(v))::rest => (case iterEvaluate(rest,sigma,theta) of 
		
			  (Success l,sigma1,theta1) => (Success ((lab,v)::l),sigma1,theta1)
			| (Fail(i),sigma1,theta1)   => (Fail(i),sigma1,theta1))
		
		| (lab,e)::rest => (case evaluate(Config(Expression(e),sigma,theta),cntr) of 
		
			  Config(Expression(Value(v)),sigma1,theta1) => (case iterEvaluate(rest,sigma1,theta1) of 
			  
				  (Success l,sigma2,theta2) => (Success ((lab,v)::l),sigma2,theta2)
				| (Fail(i),sigma2,theta2)   => (Fail(i),sigma2,theta2))
			 			 
			| Config(Expression(_),sigma1,theta1) => (Fail(cntr),sigma1,theta1)
			| Config(Stuck(i),sigma1,theta1)      => (Fail(i),sigma1,theta1)))
				
	in (case iterEvaluate(r,sigma,theta) of 
	
		  (Success l,sigma1,theta1) => evaluate(Config(Expression(Value(VRecord(l))),sigma1,theta1),cntr)
		| (Fail(i),sigma1,theta1)   => Config(Stuck(i),sigma1,theta1))
		
	end

(* Takes a list of expressions and turns into a value list of values
   Expressions evaluated in a left-to-right manner, returning list of values *)
(* (context-list) *)
|	evaluate (Config(Expression(List(l)),sigma,theta),cntr) =

	let datatype result = Fail of int | Success of v list;
	
		fun iterEvaluate(l,sigma,theta) = (case l of 
	
		  [] => (Success ([]),sigma,theta)
		
		| Value(v)::rest => (case iterEvaluate(rest,sigma,theta) of 
		
			  (Success l,sigma1,theta1) => (Success (v::l),sigma1,theta1)
			| (Fail(i),sigma1,theta1)   => (Fail(i),sigma1,theta1))
			
		| e::rest => (case evaluate(Config(Expression(e),sigma,theta),cntr) of 
		
			  Config(Expression(Value(v)),sigma1,theta1) => (case iterEvaluate(rest,sigma1,theta1) of 
			  
				  (Success l,sigma2,theta2) => (Success (v::l),sigma2,theta2)
				| (Fail(i),sigma2,theta2)   => (Fail(i),sigma2,theta2))
				
			| Config(Expression(_),sigma1,theta1) => (Fail(cntr),sigma1,theta1)
			| Config(Stuck(i),sigma1,theta1) 	  => (Fail(i),sigma1,theta1)))
			
	in (case iterEvaluate(l,sigma,theta) of 
	
		  (Success l,sigma1,theta1) => evaluate(Config(Expression(Value(VList(l))),sigma1,theta1),cntr)
		| (Fail(i),sigma1,theta1)   => Config(Stuck(i),sigma1,theta1))
		
	end
	
(* Implements evaluation * type inference rules for cons of two values *)
| 	evaluate (Config(Expression(Cons(Value(v1),Value(v2))),sigma,theta),cntr) =

	let val freshVar = generateFreshTypeVar(TYPE_VAR,theta)
	in (case evaluate(narrow(v2,TList(freshVar),sigma,theta,[],cntr),cntr) of 
	
		  Config(Expression(Value(v2narrow)),sigma1,theta1) => (case evaluate(narrow(v1,resolveChainTheta(freshVar,theta1),sigma1,theta1,[],cntr),cntr) of 
		  
			  (* (E-CONS-GOOD) *)
			  Config(Expression(Value(v1narrow)),sigma2,theta2) => (case v2narrow of 
			  
				  Concrete(EmptyList) => evaluate(Config(Expression(Value(VList([v1narrow]))),sigma2,theta2),cntr)
				| VHole(h)			  => evaluate(Config(Expression(Value(VHole(ConsHole(v1narrow,v2narrow)))),sigma2,theta2),cntr)
				| VList(l)			  => evaluate(Config(Expression(Value(VList(v1narrow::l))),sigma2,theta2),cntr)
				| _					  => Config(Stuck(cntr),sigma2,theta2))
			  
			(* (E-CONS-BAD2) *)
			| Config(c,sigma2,theta2) => Config(c,sigma2,theta2))
		  
		(* (E-CONS-BAD1) *)
		| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
		
	end

(* (context-cons-2) *)
| 	evaluate (Config(Expression(Cons(Value(v1),e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e2),sigma,theta),cntr) of 

	  Config(Expression(Value(v2)),sigma1,theta1) => evaluate(Config(Expression(Cons(Value(v1),Value(v2))),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))

(* (context-cons-1) *)
|	evaluate (Config(Expression(Cons(e1,e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 
	
	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Cons(Value(v1),e2)),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 			  		  => Config(c,sigma1,theta1))
	
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2), (E-IF-BAD) *)
|  evaluate (Config(Expression(e as Condition(Value(v),e1,e2)),sigma,theta),cntr) = 

	(* Narrow whole expression to some fresh general type variable
	   Not only does this nrrow the value we are conditioning on to a boolean,
	   But it also ensures the two branches can be narrowed to the same type 
	   Dont evaluate the resulting narrowed expression, only evalaute the value part we are case-ing on to get back a Boolean
	   (Otherwise infinite loops) *)
	   
	(case narrowExpr(e,generateFreshTypeVar(TYPE_VAR,theta),sigma,theta,[],cntr) of 
	
		  Config(Expression(Condition(v1narrow,e1,e2)),sigma1,theta1) => (case evaluate(Config(Expression(v1narrow),sigma1,theta1),cntr) of 
	
			  Config(Expression(Value(Concrete(B(b)))),sigma1,theta1) =>
			
				if b 
					 (* rule E-IF-GOOD1 *)
					 then evaluate(Config(Expression(e1),sigma1,theta1),cntr)
					
					 (* rule E-IF-GOOD2 *)
					 else evaluate(Config(Expression(e2),sigma1,theta1),cntr)
			
			(* rule E-IF-HOLE *)
			| Config(Expression(Value(newV)),sigma1,theta1) => 
			
				evaluate(Config(Expression(Value(VHole(ConditionHole(newV,e1,e2)))),sigma1,theta1),cntr)

			(* rule E-IF-BAD1 *)
			| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
			
		(* rule E-IF-BAD2 *)
		| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
			
(* (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e1,e2,e3)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 
		
	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Condition(Value(v1),e2,e3)),sigma1,theta1),cntr)	
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
 
(* Implements evaluation & type inference rules for case expression with first operand a value *)
(* i.e. implement rules (E-CASE-GOOD) and (E-CASE-BAD) *) 
|  evaluate (Config(Expression(c as Case(Value(v),patExprList)),sigma,theta),cntr) =
	
	(* Narrow the whole expression to some type variable. This 
	   a) narrows the value we are case-ing on
	   b) checks type of value and type of all patterns agree (i.e. matchTypesLists)
	   c) type of all expression branches agree *)
	   
	(* Don't follow this call to narrowExpr by evaluate like other calls to narrow
	   We want the case expression back (hence why matching non-exhaustive below *)
	   
	(case narrowExpr(c,generateFreshTypeVar(TYPE_VAR,theta),sigma,theta,[],cntr) of 

		  Config(Expression(Case(v1narrow,patExprList)),sigma1,theta1) => (case evaluate(Config(Expression(v1narrow),sigma1,theta1),cntr) of 
		  
			  Config(Expression(Value(v1narrow)),sigma1,theta1) => (case match(v1narrow,patExprList,sigma1,theta1,[]) of 
		  
				(* E-CASE-BAD1 *)
				  Fail => Config(Stuck(cntr),sigma1,theta1)
				  
				(* E-CASE-HOLE *)
				| Hole h => evaluate(Config(Expression(Value(VHole(CaseHole(VHole(h),patExprList)))),sigma1,theta1),cntr)
				
				(* E-CASE-GOOD *)
				| Success (expr,sigma2,theta2,gamma) => evaluate(Config(Expression(substitute(expr,gamma)),sigma2,theta2),cntr))
				
			| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
					
		(* E-CASE-BAD2 *)
		| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
   
(* (context-case-pair), i.e. where left-hand pair an expression *)
| 	evaluate (Config(Expression(Case(e1,patExprList)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 
	
		  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Case(Value(v1),patExprList)),sigma1,theta1),cntr)
		| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))

(* Implements evaluation & type inference rules for application expression with both operands a value *)
(* i.e. implement rules (E-APP-GOOD) and (E-APP-BAD) *) 			
| 	evaluate (Config(Expression(App(Value(v1),Value(v2))),sigma,theta),cntr) = (case typeof(v2,theta) of 
	
	  (NONE,_)		   => Config(Stuck(cntr),sigma,theta)
	| (SOME t1,theta1) => 
				
		let val freshType = generateFreshTypeVar(TYPE_VAR,theta1);
			val narrowType = TFun(t1,freshType);
				
		in (case evaluate(narrow(v1,narrowType,sigma,theta1,[],cntr),cntr) of
				
			  (* Rule E-APP-GOOD *)
			  Config(Expression(Value(Fun(x,t,e))),sigma2,theta2) => evaluate(Config(Expression(substitute(e,[(x,Value(v2))])),sigma2,theta2),cntr)
					
			(* Rule E-APP-HOLE *)
			| Config(Expression(Value(v1narrow)),sigma2,theta2) => evaluate(Config(Expression(Value(VHole(AppHole(v1narrow,v2)))),sigma2,theta2),cntr)
					
			(* rule E-APP-BAD *)
			| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
				
		end)

(* context-app-1 *)
| 	evaluate (Config(Expression(App(Value(v1),e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e2),sigma,theta),cntr) of 
		
	  Config(Expression(Value(v2)),sigma1,theta1) => evaluate(Config(Expression(App(Value(v1),Value(v2))),sigma1,theta1),cntr)	
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
			
(* context-app-2 *)
| 	evaluate (Config(Expression(App(e1,e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of

	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(App(Value(v1),e2)),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
	
(* Implements evaluation & type inference rules for let expressions with e1 a value *)
(* i.e. implements rules (E-LET-GOOD) and (E-LET-BAD) *)
|	evaluate (Config(Expression(Let(x,t,Value(v1),e2)),sigma,theta),cntr) = (case evaluate(narrow(v1,t,sigma,theta,[],cntr),cntr) of 

	(* (E-LET-GOOD) *)
	  Config(Expression(Value(v1narrow)),sigma1,theta1) => evaluate(Config(Expression(substitute(e2,[(x,Value(v1narrow))])),sigma1,theta1),cntr)
	 
	(* (E-LET-BAD) *)
	| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
	
(* context-let *)
|	evaluate (Config(Expression(Let(x,t,e1,e2)),sigma,theta),cntr) = (case evaluate(Config(Expression(e1),sigma,theta),cntr) of 

	  Config(Expression(Value(v1)),sigma1,theta1) => evaluate(Config(Expression(Let(x,t,Value(v1),e2)),sigma1,theta1),cntr)
	| Config(c,sigma1,theta1) 					  => Config(c,sigma1,theta1))
	
(* Implements evaluation & type inference rules for let-rec expressions *)
(* i.e. implements rules (E-LET-REC-GOOD) and (E-LET-REC-BAD) *)
|	evaluate (Config(Expression(l as LetRec(x,TFun(t1,t2),Value(Fun(y,t3,e1)),e2)),sigma,theta),cntr) = 

	(* Narrow whole expression to some general type variable, similarly to case expression
	   This will, for example, check types t1 and t3 unify, or that e1 is of type t2, etc. *)
	   
	(* Don't follow this call to narrowExpr by evaluate like other calls to narrow
	   We want the case expression back (hence why matching non-exhaustive below *)
	   
	(case narrowExpr(l,generateFreshTypeVar(TYPE_VAR,theta),sigma,theta,[],cntr) of 
	
		(* (E-LET-REC-GOOD) *)
		  Config(Expression(l as LetRec(x,tfun,Value(Fun(y,t3,e1)),e2)),sigma1,theta1) => 
		  
			evaluate(Config(Expression(substitute(e2,[(x,Value(Fun(y,t3,LetRec(x,tfun,Value(Fun(y,t3,e1)),e1))))])),sigma1,theta1),cntr)
		
		(* (E-LET-REC-BAD) *)
		| Config(c,sigma1,theta1) => Config(c,sigma1,theta1))
	
(* Drop CounterExpr part on function which binds to x *)
|	evaluate (Config(Expression(LetRec(x,tFun,CounterExpr(e1,_),e2)),sigma,theta),cntr) = 

		evaluate(Config(Expression(LetRec(x,tFun,e1,e2)),sigma,theta),cntr)
	
| 	evaluate (Config(Expression(LetRec(_,_,_,_)),sigma,theta),cntr) = Config(Stuck(cntr),sigma,theta);		