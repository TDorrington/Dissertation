fun wrapExpression([e1,e2],oper) =
(* Wraps two expressions into the expression associated with the operation, oper
   E.g. wrap(e1,e2,PLUS) = Expression(Plus(e1,e2)) *)
   
   (case oper of PLUS     => Expression(Plus(e1,e2))
			   | SUBTRACT => Expression(Subtract(e1,e2))
			   | TIMES    => Expression(Times(e1,e2))
			   | DIVIDE   => Expression(Divide(e1,e2)) 
			   | LESS     => Expression(LessThan(e1,e2)) 
			   | MORE     => Expression(MoreThan(e1,e2))
			   | LESSEQ   => Expression(LessThanEqual(e1,e2)) 
			   | MOREEQ   => Expression(MoreThanEqual(e1,e2))
			   | EQ  	  => Expression(Equal(e1,e2))
			   | EXPRPAIR => Expression(ExpressionPair(e1,e2)))

| wrapExpression([e1,e2,e3],oper) =

	case oper of COND => Expression(Condition(e1,e2,e3))
			   | CASE => Expression(Case(e1,e2,e3));
   
(* Takes a pair of expressions, e1 and e2, and will keep evaluating them in turn,
   e1 before e2, until nothing changes. To check whether anything has changed
   we pass the expressions from the previous iteration and check them equal 
  (using the axillary function equalExpressions, since we cannot check equal on pattern simply
   by e1 = e2 because values contains a real value, and no operation op such that op:real*real->bool - 
   we have to approximate r1'='r2 with r1<=r2 and r2<=r2
   Intuitively, this allows us to evalaute the expressions repeatedly, hopefully gaining more knowledge
   about the types in each expressions to be applied in the other.
   For example
   (v['a]+v['b]) * (v['a]+1)
   e1 = v['a]+v['b] which evaluates to v['''a]+v['''b], 
	with v['a]->v['''a], v['b]->v['''b], and 'a->'''a,'b->'''b
   e2 = v['a]+1 which evaluates to 2,
	with v['a]->1, 'a->Int
*)
fun recursiveApply2(e1,e2,e1_old,e2_old,sigma,theta,evaluate) =
   
	if equalExpressions(e1,e1_old) andalso equalExpressions(e2,e2_old) 
	then (Expression(e1),Expression(e2),sigma,theta)
	else
		let val cLeft as Config(leftE,sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
		in (case leftE of
		
			  Stuck => (Stuck,Expression(e2),sigma1,theta1)
			  
			| Expression(leftExpr) =>

				let val cRight as Config(rightE,sigma2,theta2) = evaluate(Config(Expression(e2),sigma1,theta1))
				in (case rightE of 
				
				  Stuck => (Expression(e1),Stuck,sigma2,theta2)
				  
				| Expression(rightExpr) => recursiveApply2(leftExpr,rightExpr,e1,e2,sigma2,theta2,evaluate))
					
				end)
		end;
		
fun recursiveApply3(e1,e2,e3,e1_old,e2_old,e3_old,sigma,theta,evaluate) =

	if equalExpressions(e1,e1_old) andalso equalExpressions(e2,e2_old) andalso equalExpressions(e3,e3_old)
	then (Expression(e1),Expression(e2),Expression(e3),sigma,theta)
	else
		let val cLeft as Config(leftE ,sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
		in (case leftE of
	
		  Stuck => (Stuck,Expression(e2),Expression(e3),sigma1,theta1)
		  
		| Expression(leftExpr) =>
		
			let val cMiddle as Config(middleE,sigma2,theta2) = evaluate(Config(Expression(e2),sigma1,theta1))
			in (case middleE of
			
				  Stuck => (Expression(e1),Stuck,Expression(e3),sigma2,theta2)
				
				| Expression(middleExpr) =>

					let val cRight as Config(rightE,sigma3,theta3) = evaluate(Config(Expression(e3),sigma2,theta2))
					in (case rightE of 
					
					  Stuck => (Expression(e1),Expression(e2),Stuck,sigma3,theta3)
					  
					| Expression(rightExpr) => recursiveApply3(leftExpr,middleExpr,rightExpr,e1,e2,e3,sigma3,theta3,evaluate))
					end)
			end)
		end;
   
fun contextRuleOp2(e1,e2,sigma,theta,oper,evaluate) =

	(* keep evaluating left expression followed by right expression
	   using the new sigma and theta until nothing changes *)

	let val cLeft as Config(leftE ,sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in case leftE of
	
		  Stuck => cLeft
		  
		| Expression(leftExpr) =>

			let val cRight as Config(rightE,sigma2,theta2) = evaluate(Config(Expression(e2),sigma1,theta1))
			in case rightE of 
			
			  Stuck => cRight
			  
			| Expression(rightExpr) => 
			
				case recursiveApply2(leftExpr,rightExpr,e1,e2,sigma2,theta2,evaluate) of
				 
					  (_,Stuck,sigma,theta) => Config(Stuck,sigma,theta)
				 
					| (Stuck,_,sigma,theta) => Config(Stuck,sigma,theta)
					
					| (Expression(Value(_)),Expression(Value(_)),sigma,theta) => 
						evaluate(Config(wrapExpression([leftExpr,rightExpr],oper),sigma,theta))
					
					| (Expression(e1),Expression(e2),sigma,theta) => 
						Config(wrapExpression([e1,e2],oper),sigma,theta)
				
			end
	end;
	
fun  contextRuleOp3(e1,e2,e3,sigma,theta,oper,evaluate) =
	
	(* keep evaluating left expression followed by middle expression followed by right expression
	   using the new sigma and theta until nothing changes *)

	let val cLeft as Config(leftE ,sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in (case leftE of
	
		  Stuck => cLeft
		  
		| Expression(leftExpr) =>
		
			let val cMiddle as Config(middleE,sigma2,theta2) = evaluate(Config(Expression(e2),sigma1,theta1))
			in (case middleE of
			
				  Stuck => cMiddle
				
				| Expression(middleExpr) =>

					let val cRight as Config(rightE,sigma3,theta3) = evaluate(Config(Expression(e3),sigma2,theta2))
					in (case rightE of 
					
					  Stuck => cRight
					  
					| Expression(rightExpr) => 
					
						(case recursiveApply3(leftExpr,middleExpr,rightExpr,e1,e2,e3,sigma3,theta3,evaluate) of
						 
							  (_,Stuck,_,sigma,theta) => Config(Stuck,sigma,theta)
						 
							| (Stuck,_,_,sigma,theta) => Config(Stuck,sigma,theta)
							
							| (_,_,Stuck,sigma,theta) => Config(Stuck,sigma,theta)
							
							| (Expression(e1),Expression(e2),Expression(e3),sigma,theta) => 
							
								(case (e1,e2,e3,oper) of 
								
									  (Value(v1),_,_,COND) => evaluate(Config(Expression(Condition(e1,e2,e3)),sigma,theta))
									  
									| (Value(v1),ExpressionPair(Variable(_),Variable(_)),_,CASE) =>
															  evaluate(Config(Expression(Case(e1,e2,e3)),sigma,theta))
				
									| _ => Config(wrapExpression([e1,e2,e3],oper),sigma,theta))))
						
					end)
			end)
	end;