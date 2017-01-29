fun findWitness(f) =

	(* Assumptions:
		- v['a] is a fresh value hole, i.e. not used in given f argument expression
		- 0 not associated with any counter expression in f: only top-level app, f v['a] *)
	let val f = toCounterExpr(f);
		val topLevelApp = CounterExpr(App(f,Value(VHole(SimpleHole(ValueHole(TypeVar("a")))))),0)
	
	in (case evaluate(Config(Expression(topLevelApp),[],[]),0) of 
	
		  Config(Stuck(i),sigma,theta) => Value(gen(THole(TypeHole(TypeVar("a"))),theta))
		| Config(Expression(e),_,_)    => e)
		
	end;





