fun findWitness(f) =

	(* Assumptions:
		- v['a] is a fresh value hole, i.e. not used in given f argument expression
		- 0 not associated with any counter expression in f: only top-level app, f v['a] *)
	let val f = toCounterExpr(f);
		val topLevelApp = CounterExpr(App(f,Value(VHole(SimpleHole(ValueHole(TypeVar("a")))))),0)
	
	in (case evaluate(Config(Expression(topLevelApp),[],[]),0) of 
	
		  Config(Stuck(i),sigma,theta) => 
			"Witness value: "    ^ prettyPrintExpression(Expression(Value(gen(THole(TypeHole(TypeVar("a"))),theta)))) ^ "\n" ^
			"Stuck expression: " ^ (case getExpression(topLevelApp,i) of 
				  NONE    => " "
				| SOME(s) => s)
			
		| Config(Expression(e),_,_) => 
			"Result :" ^ prettyPrintExpression(Expression(e)))
		
	end;





