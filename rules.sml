(* -------------------------------------------------------------------------------- *)
(* For now, only accept operators v * v -> int, or v * v -> bool
   where output wrapped in value datatype N(n) or B(b), respsectively *)
   
(* rules E-OP-ARITH-BOOL-GOOD, -BAD1, -BAD2 *)
fun elabPhraseArithBool (v1, v2, sigma, theta, oper) : config = 
	
	case narrow(v1,Int,sigma,theta) of
		
		  (* rule E-OP-ARITH-BOOL-BAD1 *)
		  c1 as Config(Stuck,sigma1,theta1) => c1

		| Config(Expression(Value(N(n1))),sigma1,theta1) =>
			
			case narrow(v2,Int,sigma,theta) of
				
				(* rule E-OP-ARITH-BOOL-BAD2 *)
				 c2 as Config(Stuck,sigma2,theta2) => c2
					
				(* rule E-OP-ARITH-BOOL-GOOD *)
				| _ =>
						
					(* get value using type & value substitutions from n1 *)
					let 
						val Config(Expression(Value(N(n3))),sigma3,theta3) = 
							narrow(v2,Int,sigma1,theta1);
						val n = oper(n1,n3)
					in
						case n of
						
							  (* Arithmetic operations +,-,* *)
							  N(i) => Config(Expression(Value(N(i))),sigma3,theta3)
							  
							  (* Boolean operations *)
							| B(b) => Config(Expression(Value(B(b))),sigma3,theta3)
					end;

(* -------------------------------------------------------------------------------- *)
(* Curried functions: Take operator 'a * 'a -> 'b,
   and returns function 'a * 'a => Value('b), i.e. wrap output into value datatype *)	

(* Function that wraps output of n+n, n-n, or n*n in value datatype *)
val arithInt = fn oper => fn (n1, n2) => 
	let val n = oper(n1,n2) in N(n) end;
	
(* Function that wraps output of n<n, n<=n, etc. in value datatype *)
val arithBool = fn oper => fn (n1, n2) =>
	let val b = oper(n1,n2) in B(b) end;

	
(* -------------------------------------------------------------------------------- *)
(* Arithmetic operations: +,-,* *)

fun elabPhrasePlus (Config(Expression(Plus(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithInt(op+));
	
fun elabPhraseSubtract (Config(Expression(Subtract(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithInt(op-));
	
fun elabPhraseTimes (Config(Expression(Times(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithInt(op*));

(* -------------------------------------------------------------------------------- *)
(* Boolean operations: <, <=, >=, >, = *)

fun elabPhraseLessThan (Config(Expression(LessThan(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithBool(op<) );

fun elabPhraseMoreThan (c as Config(Expression(MoreThan(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithBool(op>));

fun elabPhraseLessThanEqual (c as Config(Expression(LessThanEqual(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithBool(op<=));

fun elabPhraseMoreThanEqual (c as Config(Expression(MoreThanEqual(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithBool(op>=));

fun elabPhraseEqual	(c as Config(Expression(Equal(Value(v1),Value(v2))),sigma,theta))
	= elabPhraseArithBool(v1,v2,sigma,theta, arithBool(op=));

(* -------------------------------------------------------------------------------- *)
	
fun elabPhraseCond (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) : config =

	case narrow(v,Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b))),sigma2,theta2) =>
		
			if b (* rule E-IF-GOOD1 *)
				 then Config(Expression(e1),sigma2,theta2)
				
				 (* rule E-IF-GOOD2 *)
				 else Config(Expression(e2),sigma2,theta2);