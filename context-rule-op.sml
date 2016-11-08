fun wrapExpression(e1,e2,oper) =
(* Wraps two expressions into the expression associated with the operation, oper
   E.g. wrap(e1,e2,PLUS) = Expression(Plus(e1,e2)) *)
   
   case oper of PLUS     => Expression(Plus(e1,e2))
			  | SUBTRACT => Expression(Subtract(e1,e2))
			  | TIMES    => Expression(Times(e1,e2))
			  | DIVIDE   => Expression(Divide(e1,e2)) 
			  | LESS     => Expression(LessThan(e1,e2)) 
			  | MORE     => Expression(MoreThan(e1,e2))
			  | LESSEQ   => Expression(LessThanEqual(e1,e2)) 
			  | MOREEQ   => Expression(MoreThanEqual(e1,e2))
			  | EQ  	 => Expression(Equal(e1,e2));
   
   
fun contextRuleOp(e1,e2,sigma,theta,oper,evaluate) =

	let val cLeft as Config(leftE ,sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in case leftE of
	
		  Stuck => cLeft
		  
		| Expression(leftExpr) =>

			let val cRight as Config(rightE,sigma2,theta2) = evaluate(Config(Expression(e2),sigma1,theta1))
			in (case rightE of 
			
			  Stuck => cRight
			  
			| Expression(rightExpr) => case (leftExpr,rightExpr) of
	
				(Value(_),Value(_)) => evaluate(Config(wrapExpression(leftExpr,rightExpr,oper),sigma2,theta2))
		
				(* could potentially do more here, but leave for now
				e.g. recursive applications to substitute and narrow further and further *)
				| _ => Config(wrapExpression(leftExpr,rightExpr,oper),sigma2,theta2))
				
			end
	end;