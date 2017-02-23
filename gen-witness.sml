fun findWitness(f) =

	(* Assumptions:
		- v['a] is a fresh value hole, i.e. not used in given f argument expression
		- 0 not associated with any counter expression in f: only top-level app, f v['a] *)
	let val f = toCounterExpr(f);
		val va' = Value(VHole(SimpleHole(ValueHole(TypeVar("a")))));
		val topLevelApp = CounterExpr(App(f,va'),0)
	
	in (case evaluate(Config(Expression(topLevelApp),[],[]),0) of 
	
		  Config(Stuck(i),_,theta) => 
			"Witness: "    ^ prettyPrintExpression(Expression(Value(gen(THole(TypeHole(TypeVar("a"))),theta))))
			^ ", at expression " ^ Int.toString(i)
			
		| Config(Expression(e),_,theta) => 
			"Result :" ^ prettyPrintExpression(Expression(e)) ^ ", with witness: " ^
			 prettyPrintExpression(Expression(Value(gen(THole(TypeHole(TypeVar("a"))),theta)))))
		
	end;

fun prettyPrintIntList(l) = (case l of 

	  []      => ""
	| [x]     => Int.toString(x)
	| x::rest => Int.toString(x) ^ ", " ^ prettyPrintIntList(rest));
	
(* Takes a list of fuzzed expressions, and calls findWitness on each expression individually
   Returns a list of witnesses, corresponding to the order of the expressions in the original list, as pretty-printed strings *)
fun findWitnessList(l) = (case l of 
 
	  []          => []
	| (e,l)::rest => (findWitness(e) ^ "; expressions changed were [" ^ prettyPrintIntList(l) ^ "]")::findWitnessList(rest));

(* --- Pretty printer --- *)	
fun prettyPrintFindWitnessList(l) = (case l of 

	  []      => ""
	| [s]     => "[ " ^ s ^ " ]"
	| s::rest => "[ " ^ s ^ "], " ^ prettyPrintFindWitnessList(rest));
