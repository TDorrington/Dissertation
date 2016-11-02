(* Implements rules 
   corresponding to evaluation & type inference of arithmetic/logical operations
   e+e,e-e,e*e,e/e,e<e,e<=e,e>e,e>=e,e=e *)

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
				
		case narrow(v2,t2,sigma1,theta1) of
					
			  (* rule E-OP-BAD2 *)
			  c2 as Config(Stuck,sigma2,theta2) => c2
						
			  (* rule E-OP--GOOD *)
			| _ =>
							
				(* get value using type & value substitutions from n1 *)
				let val Config(Expression(Value(n3)),sigma3,theta3) = 
						narrow(v2,t2,sigma1,theta1);	
						
				in Config(Expression(Value(oper(n1,n3))), sigma3, theta3) end;
		
(* -------------------------------------------------------------------------------- *)

datatype operations = PLUS | SUBTRACT | DIVIDE | TIMES    (* Arithmetic operations *)
					| LESS | LESSEQ | MORE | MOREEQ | EQ; (* Boolean operations    *)
				
(* Rules for all arithmetic and boolean operations
   even when both arguments to operator are value holes
   in which case we handle its evaluation explicitly in this function   *)
fun elabPhraseOperationGeneral (v1,v2,sigma,theta,oper) =
	
	(* Wrap into value datatype equivalent versions of operator oper *)
	let val (intWrap, realWrap) = 
	
		case oper of PLUS     => (arithInt(op+),arithReal(op+))
				   | SUBTRACT => (arithInt(op-),arithReal(op-))
				   | TIMES    => (arithInt(op*),arithReal(op*))
				   | DIVIDE   => (arithInt(op*),arithReal(op/)) (* intWrap arbitrary, never used *)
				   | LESS     => (logicalInt(op<), logicalReal(op<))
				   | MORE     => (logicalInt(op>), logicalReal(op>))
				   | LESSEQ   => (logicalInt(op<=),logicalReal(op<=))
				   | MOREEQ   => (logicalInt(op>=),logicalReal(op>=))
				   | EQ 	  => (logicalInt(op=), logicalReal(op<)) (* realWrap never used, arbitrary *)
	
	in case (v1,v2) of 
	
		  (* op : int * int -> int/bool *)
		  (N(_),N(_)) 	  	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
		| (N(_),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
		| (VHole(_),N(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, intWrap, Int,Int)
					
		  (* op : real * real -> real/bool *)
		| (R(_),R(_)) 	  	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		| (R(_),VHole(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		| (VHole(_),R(_)) 	  => elabPhraseOperationEvaluate(v1,v2,sigma,theta, realWrap, Real,Real)
		
		| (VHole(ValueHole(a)),VHole(ValueHole(b))) =>
		  (* We cannot evaluate 'a op 'b as it stands, so instead narrow 'a and 'b to be 
			 arithmetic type variables, and leave as 'a op 'b with new type restrictions
			 Except in the following two cases:
				- division/equal case, where we know 'a and 'b must be of type real/int 
				  respectively - can also then evaluate after gen called 
				- after substitutions made in call to narrow, 'a or 'b are of a concrete type *)
		
		let (* Extract string from type variable datatype *)
			val s1 = case a of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s;
			val s2 = case b of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s;
			
			(* Calculate types we must constrain type variables to *)
			val t1 = case oper of DIVIDE => Real | EQ => Int | _ => THole(TypeHole(ArithTypeVar(s1)));
			val t2 = case oper of DIVIDE => Real | EQ => Int | _ => THole(TypeHole(ArithTypeVar(s2)))
				
		in case narrow(v1,t1,sigma,theta) of
				
		  c1 as Config(Stuck,sigma1,theta1) => c1	(* rule E-OP-BAD1 *)
				  
		| Config(Expression(Value(n1)),sigma1,theta1) =>
		
			(* After substitutions, we may now know first argument to be a concrete type *)
			(case n1 of 
				(* If integer, real or boolean, we know enough information to call evaluation function/be stuck *)
				  N(_) => elabPhraseOperationEvaluate(v1,v2,sigma1,theta1, intWrap, Int,Int)
				| R(_) => elabPhraseOperationEvaluate(v1,v2,sigma1,theta1, realWrap, Real,Real)
				| B(_) => Config(Stuck,sigma1,theta1)
				
				(* No more information gained from substitutions, carry on as normal *)
				| _ => 
					
					(case narrow(v2,t2,sigma1,theta1) of
					
						  c2 as Config(Stuck,sigma2,theta2) => c2	(* rule E-OP-BAD2 *)
						
						| _ => 	
							(* get value using type & value substitutions from n1 *)
							let val Config(Expression(Value(n3)),sigma3,theta3) = narrow(v2,t2,sigma1,theta1);
							
							in 
						
							(* After substitutions, we may now know second argument to be a concrete type *)
							(case n3 of 
								(* If integer, real or boolean, we know enough information to call evaluation function/be stuck *)
								  N(_) => elabPhraseOperationEvaluate(v1,v2,sigma3,theta3, intWrap, Int,Int)
								| R(_) => elabPhraseOperationEvaluate(v1,v2,sigma3,theta3, realWrap, Real,Real)
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
							end))
		end
		
		| _			 	 	  => Config(Stuck,sigma,theta)
		
	end;
		
(* Handles all expressions for boolean and arithmetic operations *)
fun elabPhraseOperation (Config(Expression(e),sigma,theta)) =

	case e of
		
		(* Arithmetic operations +,-,*,/ *)
		  Plus(Value(v1),Value(v2))		  	=> elabPhraseOperationGeneral(v1,v2,sigma,theta,PLUS)
		| Times(Value(v1),Value(v2))		=> elabPhraseOperationGeneral(v1,v2,sigma,theta,TIMES)
		| Subtract(Value(v1),Value(v2)) 	=> elabPhraseOperationGeneral(v1,v2,sigma,theta,SUBTRACT)
	   (* Divide only handles real arguments, not integer *)
		| Divide(Value(R(v1)),Value(R(v2))) => elabPhraseOperationGeneral(R(v1),R(v2),sigma,theta,DIVIDE)
		
		(* Boolean operations <,<=,>,>=,=*)
	    | LessThan(Value(v1),Value(v2))		 => elabPhraseOperationGeneral(v1,v2,sigma,theta,LESS)
		| MoreThan(Value(v1),Value(v2))		 => elabPhraseOperationGeneral(v1,v2,sigma,theta,MORE)
		| LessThanEqual(Value(v1),Value(v2)) => elabPhraseOperationGeneral(v1,v2,sigma,theta,LESSEQ)
		| MoreThanEqual(Value(v1),Value(v2)) => elabPhraseOperationGeneral(v1,v2,sigma,theta,MOREEQ)
	   (* Equal only handles integer arguments, not real *)
		| Equal(Value(N(v1)),Value(N(v2)))	 => elabPhraseOperationGeneral(N(v1),N(v2),sigma,theta,EQ);
		