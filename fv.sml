(* Returns a set containing the free variables in a pattern 
   Cannot make local to fv as is used in finding out free variables
   of a pattern to make substitutions capture avoiding *)
fun fvPat(pat) = 

	let fun fvPRecord(r) = (case r of 
		  [] 		   => []
		| (_,pat1)::r1 => union(fvPat(pat1),fvPRecord(r1)))
	
	in (case pat of 
		  PWildcard => []
		| PVar(x) => [x]
		| PVal(_) => []
		| PRecord(r) => fvPRecord(r))
	end;

(* Takes a list of expressions and returns a set containing the free variables in 
   all the expressions in the list *)
fun fv ([]) = []
|	fv (e::eList) = 
	
	let fun fvExpr(e) = (case e of 
	
			  Value(v) => fvVal(v)
			| Variable(x) => [x]
			| ArithExpr(_,e1,e2) => union(fvExpr(e1),fvExpr(e2))
			| BoolExpr (_,e1,e2) => union(fvExpr(e1),fvExpr(e2))
			| Case(e1,pat,e2) => union(fvExpr(e1),remove(fvExpr(e2),fvPat(pat)))
			| Condition(e1,e2,e3) => union(union(fvExpr(e1),fvExpr(e2)),fvExpr(e3))
			| App(e1,e2) => union(fvExpr(e1),fvExpr(e2))
			| Record(r) => fvERecord(r))
		
		and fvVal(v) = (case v of 

			  Concrete(_) => []
			| Fun(x,_,e) => remove(fvExpr(e),[x])
			| VHole(hole) => fvHole(hole)
			| VRecord(r) => fvVRecord(r))
		
		and fvHole(hole) = (case hole of 
		
			  SimpleHole(_) => []
			| BinaryOpHole(_,v1,v2) => union(fvVal(v1),fvVal(v2))
			| ConditionHole(v1,e1,e2) => union(union(fvVal(v1),fvExpr(e1)),fvExpr(e2))
			| CaseHole(v1,pat,e) => union(fvVal(v1),remove(fvExpr(e),fvPat(pat)))
			| AppHole(v1,v2) => union(fvVal(v1),fvVal(v2))
			| RecordHole(r) => fvVRecord(r))
		
		and fvVRecord(r) = (case r of 
			  [] 		 => []
			| (_,v1)::r1 => union(fvVal(v1),fvVRecord(r1)))
		
		and fvERecord(r) = (case r of 
			  []		 => []
			| (_,e1)::r1 => union(fvExpr(e1),fvERecord(r1)))
	
	in union(fvExpr(e),fv(eList)) end; 

(* ----------------------------------------------------------------------------------- *)
(* Takes an expression and returns a set containing the free type variables in that expression *)

fun ftv (Bool) = []
| 	ftv (Int)  = []
| 	ftv (Real) = []
|	ftv (TFun(t1,t2))  = append(ftv(t1),ftv(t2))
|	ftv (TRecord(r)) = (case r of 
		  [] => []
		| (_,t1)::r1 => append(ftv(t1),ftv(TRecord(r1))))
|  	ftv (THole(a))    = [THole(a)];
