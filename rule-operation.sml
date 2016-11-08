(* Implements rules corresponding to evaluation & type inference of arithmetic/logical operations
   Both arguments are values
   v+v,v-v,v*v,v/v,v<v,v<=v,v>v,v>=v,v=v *)

(* -------------------------------------------------------------------------------- *)
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

(* -------------------------------------------------------------------------------- *)
(* Rules for arithmetic and boolean operations that can be evaluated 
   i.e. both arguments to operator not value holes
   except in case where operator is divide or equal   *)

fun elabPhraseOperationEvaluate (v1, v2, sigma, theta, oper, t1, t2) : config = 
	
	case narrow(v1,t1,sigma,theta) of
			
	  (* rule E-OP-BAD1 *)
	  c1 as Config(Stuck,sigma1,theta1) => c1

	| Config(Expression(Value(n1)),sigma1,theta1) =>
				
		(case narrow(v2,t2,sigma,theta) of
					
			  (* rule E-OP-BAD2 *)
			  c2 as Config(Stuck,sigma2,theta2) => c2
						
			  (* rule E-OP--GOOD *)
			| _ =>
							
				(* get value using type & value substitutions from n1 *)
				let val Config(Expression(Value(n3)),sigma3,theta3) = 
						narrow(v2,t2,sigma1,theta1);	
						
				in Config(Expression(Value(oper(n1,n3))), sigma3, theta3) end);
				
(* -------------------------------------------------------------------------------- *)
(* Gets the bottom value for a value substitution by following chains in the substitution sigma
   For example, if there is a chain v['a]->v['b], v['b]->v['''b], v['''b]->3
   it will return 3 all when given v['a], v['b] and v['''b]
   Similarly, if there is a chain v['a]->v['b], v['b]->(v['d],v['e]), v['d]->3, v['e]->4
   it will return (3,4) for v['a] and v['b]
   Doesn't detect cycles, but can there be? *)
   
fun resolveChainSigma(a,sigma) = case a of

	  VHole(hole) =>
		
		if(Substitution.contains(hole,sigma)) 
		then resolveChainSigma(Substitution.get(hole,sigma),sigma)
		else a
	  
	| ValuePair(v1,v2) => ValuePair(resolveChainSigma(v1,sigma),resolveChainSigma(v2,sigma))
	
	| _ => a; (* Bottom value of int, real or bool *)
	
(* -------------------------------------------------------------------------------- *)

datatype operations = PLUS | SUBTRACT | DIVIDE | TIMES    (* Arithmetic operations *)
					| LESS | LESSEQ | MORE | MOREEQ | EQ  (* Boolean operations    *)
					| COND | CASE | EXPRPAIR;
				
(* Rules for all arithmetic and boolean operations
   even when both arguments to operator are value holes
   in which case we handle its evaluation explicitly in this function   *)
fun elabPhraseOperation (v1,v2,sigma,theta,oper) =
	
	(* Wrap into value datatype equivalent versions of operator oper *)
	let val (intWrap, realWrap) = 
	
		case oper of PLUS     => (arithInt(op+),arithReal(op+))
				   | SUBTRACT => (arithInt(op-),arithReal(op-))
				   | TIMES    => (arithInt(op*),arithReal(op*))
				   | DIVIDE   => (arithInt(op*),arithReal(op/))		 (* intWrap arbitrary, never used *)
				   | LESS     => (logicalInt(op<), logicalReal(op<))
				   | MORE     => (logicalInt(op>), logicalReal(op>))
				   | LESSEQ   => (logicalInt(op<=),logicalReal(op<=))
				   | MOREEQ   => (logicalInt(op>=),logicalReal(op>=))
				   | EQ 	  => (logicalInt(op=), logicalReal(op<)) (* realWrap never used, arbitrary *)
	
	in case (v1,v2) of 
	
		  (* op : int * int -> int/bool *)
		  (* need to first check operator is not /, since cannot apply to integer arguments *)
		  (N(_),N(_)) 	  	  => 
			if oper = DIVIDE then Config(Stuck,sigma,theta)
							 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
		| (N(_),VHole(_)) 	  => 
			if oper = DIVIDE then Config(Stuck,sigma,theta)
							 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
		| (VHole(_),N(_)) 	  => 
			if oper = DIVIDE then Config(Stuck,sigma,theta)
							 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
		
		  (* op : real * real -> real/bool *)
		  (* need to first check operator is not =, since cannot apply to real arguments *)
		| (R(_),R(_)) 	  	  => 
			if oper = EQ then Config(Stuck,sigma,theta)
						 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		| (R(_),VHole(_)) 	  => 
			if oper = EQ then Config(Stuck,sigma,theta)
						 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		| (VHole(_),R(_)) 	  => 
			if oper = EQ then Config(Stuck,sigma,theta) 
						 else elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		
		| (VHole(ValueHole(a)),VHole(ValueHole(b))) =>
		  (* We cannot evaluate v['a] op v['b] as it stands, so instead narrow 'a and 'b to be 
			 arithmetic type variables, and leave as 'a op 'b with new type restrictions
			 Except in the following cases:
				- substitution already contains a mapping for either v['a] 
				  and v['b], or both v['a] and v['b]
				- division/equal case, where we know 'a and 'b must be of type real/int 
				  respectively - can also then evaluate after gen called 
				- after substitutions made in call to narrow, 'a or 'b are of a concrete type *)
				
			(* First check if substitution contains mappings for value holes *)
			if(Substitution.contains(ValueHole(a),sigma) orelse Substitution.contains(ValueHole(b),sigma))
			then let val bottomA = resolveChainSigma(VHole(ValueHole(a)),sigma);
					 val bottomB = resolveChainSigma(VHole(ValueHole(b)),sigma)
				 in elabPhraseOperation(bottomA,bottomB,sigma,theta,oper) end
				 
			else
		
			(* -- Carry on knowing both generic value holes, and no existing substitutions to concrete/more narrow values -- *)
			
			let (* Extract string from type variable datatype *)
				val s1 = case a of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s;
				val s2 = case b of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s;
				
				(* Calculate types we must constrain type variables to *)
				val t1 = case oper of DIVIDE => Real | EQ => Int | _ => THole(TypeHole(ArithTypeVar(s1)));
				val t2 = case oper of DIVIDE => Real | EQ => Int | _ => THole(TypeHole(ArithTypeVar(s2)))
					
			in (case narrow(v1,t1,sigma,theta) of
					
			  c1 as Config(Stuck,sigma1,theta1) => c1	(* rule E-OP-BAD1 *)
					  
			| Config(Expression(Value(n1)),sigma1,theta1) =>
			
				(* After substitutions, we may now know first argument to be a concrete type *)
				(case n1 of 
					(* If integer, real or boolean, we know enough information to call evaluation function/be stuck *)
					  N(_) => if oper = DIVIDE then Config(Stuck,sigma1,theta1)
					          else elabPhraseOperationEvaluate(v1,v2,sigma1,theta1, intWrap, Int,Int)
					| R(_) => if oper = EQ then Config(Stuck,sigma1,theta1)
							  else elabPhraseOperationEvaluate(v1,v2,sigma1,theta1, realWrap, Real,Real)
					| B(_) => Config(Stuck,sigma1,theta1)
					
					(* No more information gained from substitutions, carry on as normal *)
					| _ => 
						
						(case narrow(v2,t2,sigma1,theta1) of
						
							  c2 as Config(Stuck,sigma2,theta2) => c2	(* rule E-OP-BAD2 *)
							
							| _ => 	
								(* get value using type & value substitutions from n1 *)
								let val Config(Expression(Value(n3)),sigma3,theta3) = 
									narrow(v2,t2,sigma1,theta1)
								in 
								
								(* After substitutions, we may now know second argument to be a concrete type *)
								(case n3 of 
									(* If integer, real or boolean, we know enough information to call evaluation function/be stuck *)
									  N(_) => if oper = DIVIDE then Config(Stuck,sigma3,theta3)
											  else elabPhraseOperationEvaluate(v1,v2,sigma3,theta3, intWrap, Int,Int)
									| R(_) => if oper = EQ then Config(Stuck,sigma3,theta3)
											  else elabPhraseOperationEvaluate(v1,v2,sigma3,theta3, realWrap, Real,Real)
									| B(_) => Config(Stuck,sigma3,theta3)
									
									(* No more information gained from substitutions, carry on as normal *)
									| _ => 
							
									(case oper of PLUS     => Config(Expression(Plus(Value(n1),Value(n3))), sigma3, theta3) 
												| SUBTRACT => Config(Expression(Subtract(Value(n1),Value(n3))), sigma3, theta3) 
												| TIMES    => Config(Expression(Times(Value(n1),Value(n3))), sigma3, theta3) 
												| DIVIDE   => Config(Expression(Value(realWrap(n1,n3))), sigma3, theta3) 
												| LESS     => Config(Expression(LessThan(Value(n1),Value(n3))), sigma3, theta3) 
												| MORE     => Config(Expression(MoreThan(Value(n1),Value(n3))), sigma3, theta3) 
												| LESSEQ   => Config(Expression(LessThanEqual(Value(n1),Value(n3))), sigma3, theta3) 
												| MOREEQ   => Config(Expression(MoreThanEqual(Value(n1),Value(n3))), sigma3, theta3) 
												| EQ  	 => Config(Expression(Value(intWrap(n1,n3))), sigma3, theta3)))
								end)))
			end
		
		| _			 	 	  => Config(Stuck,sigma,theta)
		
	end;