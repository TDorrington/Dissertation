(* Curried functions: Take operator 'a * 'a -> 'b,
   and returns function Value('a) * Value('a) => Value('b)
   i.e. wrap input & output into value datatype *)	

(* Function that wraps I/O of n+n, n-n, or n*n (not n/n) in value datatype *)
val arithInt = fn oper => fn (N(n1), N(n2)) => N(oper(n1,n2));
	
(* Function that wraps I/O of r+r, r-r, r/r or r*r in value datatype *)
val arithReal = fn oper => fn (R(r1), R(r2)) => R(oper(r1,r2));
	
(* Function that wraps I/O of n<n, n<=n, etc. in value datatype *)
val logicalInt = fn oper => fn (N(n1), N(n2)) => B(oper(n1,n2));
	
(* Function that wraps I/O of r<r, r<=r, etc. (not r=r) in value datatype *)
val logicalReal = fn oper => fn (R(r1), R(r2)) => B(oper(r1,r2));

(* Function that wraps I/O of b=b in value datatype *)
val logicalBool = fn oper => fn (B(b1), B(b2)) => B(oper(b1,b2));

(* -------------------------------------------------------------------------------- *)
(* Rules for arithmetic and boolean operations that can be evaluated 
   i.e. both arguments to operator not value holes *)
fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, oper, t) : config = 
	
	case narrow(v1,t,sigma,theta) of
			
	  (* rule E-OP-BAD1 *)
	  Config(Stuck,sigma1,theta1) => Config(Stuck,sigma1,theta1)

	| Config(Expression(Value(n1)),sigma1,theta1) => (case narrow(v2,t,sigma,theta) of
					
			  (* rule E-OP-BAD2 *)
			  Config(Stuck,sigma2,theta2) => Config(Stuck,sigma2,theta2)
						
			  (* rule E-OP--GOOD *)
			| _ =>
							
				(* get value using type & value substitutions from n1 *)
				let val Config(Expression(Value(n3)),sigma3,theta3) = narrow(v2,t,sigma1,theta1);	
						
				in Config(Expression(Value(oper(n1,n3))), sigma3, theta3) end);
				
(* -------------------------------------------------------------------------------- *)			
(* Rules for all arithmetic and boolean operations
   even when both arguments to operator are value holes
   in which case we handle its evaluation explicitly in this function   *)
fun elabPhraseOperation (v1,v2,sigma,theta,oper) =
	
	(* Wrap into value datatype equivalent versions of operator oper *)
	let val (intWrap, realWrap) = case oper of 
	
	     ArithOper(PLUS)     => (arithInt(op+),arithReal(op+))
	   | ArithOper(SUBTRACT) => (arithInt(op-),arithReal(op-))
	   | ArithOper(TIMES)    => (arithInt(op*),arithReal(op*))
	   | ArithOper(DIVIDE)   => (arithInt(op*),arithReal(op/)) (* intWrap arbitrary, never used *)
	   | BoolOper(LESS)      => (logicalInt(op<), logicalReal(op<))
	   | BoolOper(MORE)      => (logicalInt(op>), logicalReal(op>))
	   | BoolOper(LESSEQ)    => (logicalInt(op<=),logicalReal(op<=))
	   | BoolOper(MOREEQ)    => (logicalInt(op>=),logicalReal(op>=))
	   | BoolOper(EQ)        => (logicalInt(op=), logicalReal(op<)) (* realWrap never used, arbitrary *)
	
	in case (v1,v2) of 
	
		  (* op : int * int -> int/bool *)
		  (* need to first check operator is not /, since cannot apply to integer arguments *)
		  (N(_),N(_)) 	  	  => 
			if oper = ArithOper(DIVIDE) then Config(Stuck,sigma,theta)
										else elabPhraseOperationEvaluate(v1,v2,sigma,theta,intWrap,Int)
		| (N(_),VHole(_)) 	  => 
			if oper = ArithOper(DIVIDE) then Config(Stuck,sigma,theta)
										else elabPhraseOperationEvaluate(v1,v2,sigma,theta,intWrap,Int)
		| (VHole(_),N(_)) 	  => 
			if oper = ArithOper(DIVIDE) then Config(Stuck,sigma,theta)
										else elabPhraseOperationEvaluate(v1,v2,sigma,theta,intWrap,Int)
		
		  (* op : real * real -> real/bool *)
		  (* need to first check operator is not =, since cannot apply to real arguments *)
		| (R(_),R(_)) 	  	  => 
			if oper = BoolOper(EQ) then Config(Stuck,sigma,theta)
								   else elabPhraseOperationEvaluate(v1,v2,sigma,theta,realWrap,Real)
		| (R(_),VHole(_)) 	  => 
			if oper = BoolOper(EQ) then Config(Stuck,sigma,theta)
								   else elabPhraseOperationEvaluate(v1,v2,sigma,theta,realWrap,Real)
		| (VHole(_),R(_)) 	  => 
			if oper = BoolOper(EQ) then Config(Stuck,sigma,theta) 
								   else elabPhraseOperationEvaluate(v1,v2,sigma,theta,realWrap,Real)
		
		(* For bool arguments, can only be operator equal *)
		| (B(_),B(_)) 	     =>
			if oper = BoolOper(EQ) then elabPhraseOperationEvaluate(v1,v2,sigma,theta,logicalBool(op=),Bool)
								   else Config(Stuck,sigma,theta)
		| (B(_),VHole(_)) 	 =>
			if oper = BoolOper(EQ) then elabPhraseOperationEvaluate(v1,v2,sigma,theta,logicalBool(op=),Bool)
								   else Config(Stuck,sigma,theta)
		| (VHole(_),B(_)) 	 =>
			if oper = BoolOper(EQ) then elabPhraseOperationEvaluate(v1,v2,sigma,theta,logicalBool(op=),Bool)
								   else Config(Stuck,sigma,theta)
		
		(* For pair arguments, can only be operator equal
		   Pass the type which pair must narrow to to be the type of arbitrary pair *)
		| (ValuePair(_),ValuePair(_)) =>
			if oper = BoolOper(EQ) 
			then (case typeof(v1,theta) of
				    (NONE,_) => Config(Stuck,sigma,theta)
				  | (SOME(pairType),theta1) => elabPhraseOperationEvaluate(v1,v2,sigma,theta1,logicalBool(op=),pairType))
		    else Config(Stuck,sigma,theta)
		| (ValuePair(_),VHole(_)) =>
			if oper = BoolOper(EQ) 
			then (case typeof(v1,theta) of
				    (NONE,_) => Config(Stuck,sigma,theta)
				  | (SOME(pairType),theta1) => elabPhraseOperationEvaluate(v1,v2,sigma,theta1,logicalBool(op=),pairType))
		    else Config(Stuck,sigma,theta)		
		| (VHole(_),ValuePair(_)) =>
			if oper = BoolOper(EQ) 
			then (case typeof(v2,theta) of
				    (NONE,_) => Config(Stuck,sigma,theta)
				  | (SOME(pairType),theta1) => elabPhraseOperationEvaluate(v1,v2,sigma,theta1,logicalBool(op=),pairType))
		    else Config(Stuck,sigma,theta)
		
		| (VHole(SimpleHole(ValueHole(a))),VHole(SimpleHole(ValueHole(b)))) =>
		  (* We cannot evaluate v['a] op v['b] as it stands, so instead narrow 'a and 'b to be 
			 arithmetic type variables, and leave as 'a op 'b with new type restrictions
			 Except in the following cases:
				- substitution already contains a mapping for either v['a] 
				  and v['b], or both v['a] and v['b]
				- division, where we know 'a and 'b must be of type real
				  can also then evaluate after gen called 
				- after substitutions made in call to narrow, 'a or 'b are of a concrete type *)
				
			(* First check if substitution contains mappings for value holes *)
			if(Substitution.contains(ValueHole(a),sigma) orelse Substitution.contains(ValueHole(b),sigma))
			then let val bottomA = resolveChainSigma(VHole(SimpleHole(ValueHole(a))),sigma);
					 val bottomB = resolveChainSigma(VHole(SimpleHole(ValueHole(b))),sigma)
				 in elabPhraseOperation(bottomA,bottomB,sigma,theta,oper) end
				 
			else
			(* -- Carry on knowing both generic value holes, and no existing substitutions to concrete/more narrow values -- *)
			
			let (* Calculate types we must constrain type variables to *)
				val t = (case oper of 
					  ArithOper(DIVIDE) => Real 
					| BoolOper(EQ)      => generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
					| _      => generateFreshTypeVar(ARITH_TYPE_VAR,theta))
				
			in (case narrow(v1,t,sigma,theta) of
					
			  Config(Stuck,sigma1,theta1) => Config(Stuck,sigma1,theta1) (* rule E-OP-BAD1 *)
					  
			| Config(Expression(Value(n1)),sigma1,theta1) =>
			
				(* After narrow, we may now know first argument to be a concrete type *)
				(case n1 of 
				
					  N(_) => if oper = ArithOper(DIVIDE) then Config(Stuck,sigma1,theta1)
					          else elabPhraseOperationEvaluate(v1,v2,sigma1,theta1,intWrap,Int)
					| R(_) => if oper = BoolOper(EQ) then Config(Stuck,sigma1,theta1)
							  else elabPhraseOperationEvaluate(v1,v2,sigma1,theta1,realWrap,Real)
					| B(_) => if oper = BoolOper(EQ) then elabPhraseOperationEvaluate(v1,v2,sigma1,theta1,logicalBool(op=),Bool)
							  else Config(Stuck,sigma1,theta1)
					| ValuePair(_) => 
						if oper = BoolOper(EQ) 
						then (case typeof(v1,theta1) of
								(NONE,_) => Config(Stuck,sigma,theta1)
							  | (SOME(pairType),theta2) => elabPhraseOperationEvaluate(v1,v2,sigma,theta2,logicalBool(op=),pairType))
						else Config(Stuck,sigma,theta1)
					
					(* No more information gained from substitutions, carry on as normal *)
					| _ => (case narrow(v2,t,sigma,theta) of
						
						  Config(Stuck,sigma2,theta2) => Config(Stuck,sigma2,theta2)	(* rule E-OP-BAD2 *)
							
						| _ => 	
							(* get value using type & value substitutions from n1 *)
							let val Config(Expression(Value(n3)),sigma3,theta3) = narrow(v2,t,sigma1,theta1)
							in 
							(* After narrow, we may now know second argument to be a concrete type *)
							(case n3 of 
							
								  N(_) => if oper = ArithOper(DIVIDE) then Config(Stuck,sigma3,theta3)
										  else elabPhraseOperationEvaluate(v1,v2,sigma3,theta3,intWrap,Int)
								| R(_) => if oper = BoolOper(EQ) then Config(Stuck,sigma3,theta3)
										  else elabPhraseOperationEvaluate(v1,v2,sigma3,theta3,realWrap,Real)
								| B(_) => if oper = BoolOper(EQ) then elabPhraseOperationEvaluate(v1,v2,sigma1,theta3,logicalBool(op=),Bool)
										else Config(Stuck,sigma1,theta3)
								| ValuePair(_) => 
									if oper = BoolOper(EQ) 
									then (case typeof(v2,theta3) of
											(NONE,_) => Config(Stuck,sigma,theta1)
										  | (SOME(pairType),theta4) => elabPhraseOperationEvaluate(v1,v2,sigma,theta4,logicalBool(op=),pairType))
									else Config(Stuck,sigma,theta3)
									
								(* No more information gained from substitutions, carry on as normal *)
								| _ => (case oper of 
									  ArithOper(arithOp) => Config(Expression(ArithExpr(arithOp,Value(n1),Value(n3))),sigma3,theta3)
									| BoolOper(boolOp)   => Config(Expression(BoolExpr(boolOp,Value(n1),Value(n3))),sigma3,theta3)))
							end)))
			end
		
		| _			 	 	  => Config(Stuck,sigma,theta)
		
	end;