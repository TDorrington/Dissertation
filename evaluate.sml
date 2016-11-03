(* No evaluation necessary for a value *)
fun evaluate (c as Config(Expression(Value(v)),sigma,theta)) = c

(* Resolves the slight loophole that <v,v> can be an expression and a value *)
|	evaluate (Config(Expression(ExpressionPair(Value(v1),Value(v2))),sigma,theta)) =
		Config(Expression(Value(ValuePair(v1,v2))),sigma,theta)

(* Arithmetic operations +,-,*,/ with both arguments as values   ------------------- *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op an arithmetic operator *)
|   evaluate (Config(Expression(Plus(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,PLUS)
|   evaluate (Config(Expression(Times(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,TIMES)
|   evaluate (Config(Expression(Subtract(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,SUBTRACT)
|   evaluate (Config(Expression(Divide(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,DIVIDE)
		
(* Boolean operations <,<=,>,>=,= with both arguments as values *)
(* I.e. rules (E-OP-GOOD), (E-OP-BAD1) and (E-OP-BAD2) for op a boolean operator *)
|  evaluate (Config(Expression(LessThan(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,LESS)
|  evaluate (Config(Expression(MoreThan(Value(v1),Value(v2))),sigma,theta)) =	
		elabPhraseOperation(v1,v2,sigma,theta,MORE)
|  evaluate (Config(Expression(LessThanEqual(Value(v1),Value(v2))),sigma,theta)) =	
		elabPhraseOperation(v1,v2,sigma,theta,LESSEQ)
|  evaluate (Config(Expression(MoreThanEqual(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,MOREEQ)
|  evaluate (Config(Expression(Equal(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseOperation(v1,v2,sigma,theta,EQ)

(* Context rule (context-op-2): left argument a value, right argument a general expression *)

|  evaluate (Config(Expression(Plus(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(Plus(Value(v1),Value(v2))),sigma1,theta1)) end
		
|  evaluate (Config(Expression(Times(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(Times(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(Subtract(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(Subtract(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(Divide(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(Divide(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(LessThan(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(LessThan(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(MoreThan(Value(v1),e2)),sigma,theta)) =	
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(MoreThan(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(LessThanEqual(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(LessThanEqual(Value(v1),Value(v2))),sigma1,theta1)) end	

|  evaluate (Config(Expression(MoreThanEqual(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(MoreThanEqual(Value(v1),Value(v2))),sigma1,theta1)) end

|  evaluate (Config(Expression(Equal(Value(v1),e2)),sigma,theta)) =
   let val Config(Expression(Value(v2)),sigma1,theta1) = evaluate(Config(Expression(e2),sigma,theta))
   in evaluate(Config(Expression(Equal(Value(v1),Value(v2))),sigma1,theta1)) end
   
(* Context rule (context-op-1): both arguments generic expressions -----------------  *)

|   evaluate (Config(Expression(Plus(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(Plus(Value(n1),e2)),sigma1,theta1)) end
	
|   evaluate (Config(Expression(Times(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(Times(Value(n1),e2)),sigma1,theta1)) end
	
|	evaluate (Config(Expression(Subtract(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(Subtract(Value(n1),e2)),sigma1,theta1)) end

|	evaluate (Config(Expression(Divide(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(Divide(Value(n1),e2)),sigma1,theta1)) end

|	evaluate (Config(Expression(LessThan(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(LessThan(Value(n1),e2)),sigma1,theta1)) end

| 	evaluate (Config(Expression(MoreThan(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(MoreThan(Value(n1),e2)),sigma1,theta1)) end

| 	evaluate (Config(Expression(LessThanEqual(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(LessThanEqual(Value(n1),e2)),sigma1,theta1)) end

| 	evaluate (Config(Expression(MoreThanEqual(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(MoreThanEqual(Value(n1),e2)),sigma1,theta1)) end

| 	evaluate (Config(Expression(Equal(e1,e2)),sigma,theta)) =
	let val Config(Expression(Value(n1)),sigma1,theta1) = evaluate(Config(Expression(e1),sigma,theta))
	in evaluate(Config(Expression(Equal(Value(n1),e2)),sigma1,theta1)) end
	
(* Implements evaluation & type inference rules for if expression with boolean operand a value *)
(* i.e. implements rules (E-IF-GOOD1), (E-IF-GOOD2) and (E-IF-BAD) *)
|  evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) =

	(case narrow(v,Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b))),sigma2,theta2) =>
		
			if b (* rule E-IF-GOOD1 *)
				 then evaluate(Config(Expression(e1),sigma2,theta2))
				
				 (* rule E-IF-GOOD2 *)
				 else evaluate(Config(Expression(e2),sigma2,theta2)))
				 
(* Handles context rule (context-if): boolean argument a general expression ---------  *)		
|  evaluate (Config(Expression(Condition(e,e1,e2)),sigma,theta)) =
   let val Config(Expression(Value(v)),sigma1,theta1) = evaluate(Config(Expression(e),sigma,theta))
   in evaluate (Config(Expression(Condition(Value(v),e1,e2)),sigma1,theta1)) end
 
 (* TODO: here below and substitute file, pretty print and lots of test *)
 
(* Implements evaluation & type inference rules for case-pair expression with first operand a value *)
(* i.e. implement rules (E-CASE-PAIR-GOOD) and (E-CASE-PAIR-BAD) *) 
|  evaluate (Config(Expression(Case(Value(v),ExpressionPair(Variable(x1),Variable(x2)),e)),sigma,theta)) =

	(* generate fresh type variables, alpha1 and alpha2, using unique counter *)
	let val alpha1 = TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())));
		val alpha2 = TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())))
	in
		(* double check fresh type variables not already in type substitution
		   this should not be the case if we begin evaluation with empty substitution since all
		   the type variables generated by the program are unique,
		   but may be exceptional circumstance where type substitution already contains 
		   mapping for alpha1 or alpha2
		   If already in map, keep re-calling function until they are different from any already there *)
		if (Substitution.contains(alpha1,theta) orelse Substitution.contains(alpha2,theta))
		then evaluate(Config(Expression(Case(Value(v),ExpressionPair(Variable(x1),Variable(x2)),e)),sigma,theta))
		else
	   
			case narrow(v,Pair(THole(alpha1),THole(alpha2)),sigma,theta) of
			
			  (* rule E-CASE-PAIR-BAD *)
			  c as Config(Stuck,sigma1,theta1) => c
			  
			  (* rule E-CASE-PAIR-GOOD *)
			| Config(Expression(Value(ValuePair(v1,v2))),sigma1,theta1) =>
			
				let val (e',sigma2,theta2)  = substitute(e,v1,x1,sigma1,theta1,evaluate);
					val (e'',sigma3,theta3) = substitute(e',v2,x2,sigma2,theta2,evaluate)
				in Config(Expression(e''),sigma3,theta3) end
				
   end
   
| evaluate (Config(Expression(Case(e1,ExpressionPair(Variable(x1),Variable(x2)),e2)),sigma,theta) =

	
 
 (* TESTS FOR SUBSTITUTE *)
 
 substitute(Value(N(4)),N(3),Var("x"),[],[],evaluate); 			(* gives Value(N(4)),[],[]) *)
 substitute(Variable(Var("x")),N(3),Var("x"),[],[],evaluate);   (* gives Value(N(3)),[],[]) *)
 substitute(Plus(Value(N(3)),Value(N(4))),N(3),Var("x"),[],[], evaluate); 	(* gives Plus(Value(N(3)),Value(N(4))),[],[] *)
 substitute(Plus(Variable(Var("x")),Variable(Var("x"))),N(3),Var("x"),[],[],evaluate); (* gives Plus(Value(N(3)),Value(N(3))),[],[] *)
 substitute(Condition(Value(B(true)),Variable(Var("x")),Plus(Variable(Var("x")),Value(N(1)))),N(3),Var("x"),[],[],evaluate);
 (* gives (if true then 3 else 3 + 1,[],[]) *)
 
 evaluate(Config(Expression(Case(Value(ValuePair(N(1),N(2))),
								 ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
								 Plus(Variable(Var("x1")),Variable(Var("x2"))))),
							[],[]));
(* < case (1,2) of (x1,x2) -> x1 + x2 > -> < 1 + 2 > *)

evaluate(Config(Expression(
	Case(Value(ValuePair(N(1),N(2))),
		 ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
		 Case(ExpressionPair(Variable(Var("x1")),Variable(Var("x2"))),
			  ExpressionPair(Variable(Var("x3")),Variable(Var("x4"))),
			  Plus(Variable(Var("x3")),Variable(Var("x3")))))), [], []));
(* <case (1,2) of (x1,x2) ->
		case (x1,x2) of (x3,x4) -> x3+x4 
	maps to
	*)