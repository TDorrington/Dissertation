(* Implements rules E-IF-GOOD1, E-IF-GOOD2 and E-IF-BAD
   corresponding to evaluation & type inference of conditional expression
   if e then e else e *)

fun elabPhraseCondition (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) : config =

	case narrow(v,Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b))),sigma2,theta2) =>
		
			if b (* rule E-IF-GOOD1 *)
				 then Config(Expression(e1),sigma2,theta2)
				
				 (* rule E-IF-GOOD2 *)
				 else Config(Expression(e2),sigma2,theta2);