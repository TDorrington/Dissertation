fun elabPhrasePlus (Config(Expression(Plus(Value(N(v1)),Value(N(v2)))),
							sigma,theta)) : config = 
	
	case narrow(N(v1),Int,sigma,theta) of
		
		  (* rule E-PLUS-BAD1 *)
		  c1 as Config(Stuck,sigma1,theta1) => c1

		| Config(Expression(Value(N(n1))),sigma1,theta1) =>
			
			case narrow(N(v2),Int,sigma,theta) of
				
				(* rule E-PLUS-BAD2 *)
				 c2 as Config(Stuck,sigma2,theta2) => c2
					
				(* rule E-PLUS-GOOD *)
				| _ =>
						
					(* get value using new type & value substitutions from n1 *)
					let 
						val Config(Expression(Value(N(n3))),sigma3,theta3) = 
							narrow(N(v2),Int,sigma1,theta1);
						val n = n1+n3
					in
						Config(Expression(Value(N(n))),sigma3,theta3)
					end;