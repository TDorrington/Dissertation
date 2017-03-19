(* We need to determine how many concrete values to generate to make execution go wrong. 
   This function addresses this issue of currying by taking an input expression e,
   and producing a saturated expression of the form e v1[alpha1] ... vn[alphan]
   that does not evaluate to a lambda. Keep adding holes to target application
   until evaluating the term yields a non-lambda value 
   Returns both the saturated expression,
   and a list of the type variables we need to generate witnesses for *)
fun saturate(e) = (case evaluate(Config(Expression(e),[],[]),0) of 

	  (* Saturate(e v[alpha]), for v and alpha fresh *)
	    Config(Expression(Value(Fun(_,_,_))),_,_) => 
		
			let val t = TypeVar("a"^Int.toString(getCounterAndUpdate()));
				val (sat,l) = saturate(App(e,Value(VHole(SimpleHole(ValueHole(t))))))
			in (sat,t::l) end
	  
	  | _ => (e,[]));

fun printWitnessList(l,theta) = (case l of 

	  []      => ""
	| [t]     => prettyPrintExpression(Expression(Value(gen(THole(TypeHole(t)),theta))))
	| t::rest => prettyPrintExpression(Expression(Value(gen(THole(TypeHole(t)),theta)))) ^ ", " ^ printWitnessList(rest,theta));
	  
fun findWitness(f) =

	(* Assumptions:
		- v['a] is a fresh value hole, i.e. not used in given f argument expression
		- 0 not associated with any counter expression in f: only top-level app, f v['a] *)
	let val (f,l) = saturate(f);
		val f = toCounterExpr(f);
	
	in (case evaluate(Config(Expression(f),[],[]),0) of 
	
		  Config(Stuck(i),_,theta) => 
			"Witness: "    ^ printWitnessList(l,theta) ^ ",stuck at expression " ^ Int.toString(i)
			
		| Config(Expression(e),_,theta) => 
			"Result: " ^ prettyPrintExpression(Expression(e)) ^ ", with witness: " ^ printWitnessList(l,theta))
		
	end;

fun prettyPrintIntList(l) = (case l of 

	  []      => ""
	| [x]     => Int.toString(x)
	| x::rest => Int.toString(x) ^ ", " ^ prettyPrintIntList(rest));
	
(* Takes a list of fuzzed expressions, and calls findWitness on each expression individually
   Returns a list of witnesses, corresponding to the order of the expressions in the original list, as pretty-printed strings *)
fun findWitnessList(l) = (case l of 
 
	  []          => []
	| (e,i,l)::rest => (findWitness(e) ^ "; expression changed was " ^ Int.toString(i) ^ "; expressions impacted were [" ^ prettyPrintIntList(l) ^ "]")::findWitnessList(rest));

(* --- Pretty printer --- *)	
fun prettyPrintFindWitnessList(l) = (case l of 

	  []      => ""
	| [s]     => "[ " ^ s ^ " ]"
	| s::rest => "[ " ^ s ^ "], " ^ prettyPrintFindWitnessList(rest));
