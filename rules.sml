(* -------------------------------------------------------------------------------- *)
(* rules E-OP-ARITH-BOOL-GOOD, -BAD1, -BAD2 *)

fun elabPhraseArithBool ( N(v1), N(v2), sigma, theta, oper) : config = 
	
	case narrow(N(v1),Int,sigma,theta) of
		
		  (* rule E-OP-ARITH-BOOL-BAD1 *)
		  c1 as Config(Stuck,sigma1,theta1) => c1

		| Config(Expression(Value(N(n1))),sigma1,theta1) =>
			
			case narrow(N(v2),Int,sigma,theta) of
				
				(* rule E-OP-ARITH-BOOL-BAD2 *)
				 c2 as Config(Stuck,sigma2,theta2) => c2
					
				(* rule E-OP-ARITH-BOOL-GOOD *)
				| _ =>
						
					(* get value using type & value substitutions from n1 *)
					let 
						val Config(Expression(Value(N(n3))),sigma3,theta3) = 
							narrow(N(v2),Int,sigma1,theta1);
						val n = oper(n1,n3)
					in
						Config(Expression(Value(N(n))),sigma3,theta3)
					
					end;

(* -------------------------------------------------------------------------------- *)
(* Arithmetic operations: +,-,* *)
					
fun elabPhrasePlus (Config(Expression(Plus(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op+);
	
fun elabPhraseSub (c as Config(Expression(Subtract(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op-);
	
fun elabPhraseTimes (c as Config(Expression(Times(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op*);
	
(* -------------------------------------------------------------------------------- *)
(* Boolean operations: <, <=, >=, >, = *)

fun elabPhraseLessThan (c as Config(Expression(LessThan(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op<);

fun elabPhraseMoreThan (c as Config(Expression(MoreThan(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op>);

fun elabPhraseLessThanEqual (c as Config(Expression(LessThanEqual(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op<=);

fun elabPhraseMoreThanEqual (c as Config(Expression(MoreThanEqual(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op>=);

fun elabPhraseEqual	(c as Config(Expression(Equal(Value(N(v1)),Value(N(v2)))),sigma,theta))
	= elabPhraseArithBool(N(v1),N(v2),sigma,theta,op=);

(* -------------------------------------------------------------------------------- *)
	
fun elabPhraseCond (Config(Expression(Condition(Value(B(b)),e1,e2)),sigma,theta)) : config =

	case narrow(B(b),Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b1))),sigma2,theta2) =>
		
			if b 
			
				(* rule E-IF-GOOD1 *)
				then Config(Expression(e1),sigma2,theta2)
				
				(* rule E-IF-GOOD2 *)
				else Config(Expression(e2),sigma2,theta2);
				