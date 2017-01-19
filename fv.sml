(* Returns a set containing the free variables in a pattern 
   Cannot make local to fv as is used in finding out free variables
   of a pattern to make substitutions in case expressions capture avoiding *)
fun fvPat(pat) = 

	let fun fvPRecord(r) = (case r of 
		  [] 		   => []
		| (_,pat1)::r1 => union(fvPat(pat1),fvPRecord(r1)))
	
	in (case pat of 
		  PWildcard        => []
		| PVar(x)          => [x]
		| PVal(_)          => []
		| PCons(pat1,pat2) => union(fvPat(pat1),fvPat(pat2))
		| PRecord(r)       => fvPRecord(r))
	end;

(* Takes a list of expressions and returns a set containing the free variables in 
   all the expressions in the list *)
fun fv ([]) = []
|	fv (e::eList) = 
	
	let fun fvExpr(e) = (case e of 
	
			  Value(v)             => fvVal(v)
			| Variable(x)          => [x]
			| ArithExpr(_,e1,e2)   => union(fvExpr(e1),fvExpr(e2))
			| BoolExpr (_,e1,e2)   => union(fvExpr(e1),fvExpr(e2))
			| Case(e1,patExprList) => union(fvExpr(e1),fvPatExprList(patExprList))
			| Condition(e1,e2,e3)  => union(union(fvExpr(e1),fvExpr(e2)),fvExpr(e3))
			| App(e1,e2)           => union(fvExpr(e1),fvExpr(e2))
			| Record(r)            => fvERecord(r)
			| Let(x,_,e1,e2)       => union(remove(fvExpr(e2),[x]),fvExpr(e1))
			| LetRec(x,_,v,e2)     => union(remove(fvExpr(e2),[x]),remove(fvVal(v),[x])) (* y binds in e1; x binds in (fn y:T => e1) and in e2 *)
			| List(l)              => fvEList(l)
			| Cons(e1,e2)		   => union(fvExpr(e1),fvExpr(e2)))
		
		and fvPatExprList(l) = (case l of 
		
			  []            => []
			| (pat1,e1)::l1 => union(remove(fvExpr(e1),fvPat(pat1)),fvPatExprList(l1)))
		
		and fvVal(v) = (case v of 

			  Concrete(_) => []
			| Fun(x,_,e)  => remove(fvExpr(e),[x])
			| VHole(hole) => fvHole(hole)
			| VRecord(r)  => fvVRecord(r)
			| VList(l)    => fvVList(l))
		
		and fvHole(hole) = (case hole of 
		
			  SimpleHole(_)            => []
			| BinaryOpHole(_,v1,v2)    => union(fvVal(v1),fvVal(v2))
			| ConditionHole(v1,e1,e2)  => union(union(fvVal(v1),fvExpr(e1)),fvExpr(e2))
			| CaseHole(v1,patExprList) => union(fvVal(v1),fvPatExprList(patExprList))
			| AppHole(v1,v2)           => union(fvVal(v1),fvVal(v2))
			| RecordHole(r)            => fvVRecord(r)
			| ListHole(l)              => fvVList(l)
			| ConsHole(v1,v2)		   => union(fvVal(v1),fvVal(v2)))
		
		and fvVRecord(r) = (case r of 
			  [] 		 => []
			| (_,v1)::r1 => union(fvVal(v1),fvVRecord(r1)))
		
		and fvERecord(r) = (case r of 
			  []		 => []
			| (_,e1)::r1 => union(fvExpr(e1),fvERecord(r1)))
			
		and fvVList(l) = (case l of 
		
			  []     => []
			| v1::l1 => union(fvVal(v1),fvVList(l1)))
			
		and fvEList(l) = (case l of 
		
			  []     => []
			| e1::l1 => union(fvExpr(e1),fvEList(l1)))
	
	in union(fvExpr(e),fv(eList)) end; 

(* ----------------------------------------------------------------------------------- *)
(* Takes an expression and returns a set containing the free type variables in that expression *)

fun ftv (Bool) = []
| 	ftv (Int)  = []
| 	ftv (Real) = []
|	ftv (TFun(t1,t2)) = union(ftv(t1),ftv(t2))
|	ftv (TRecord(r))  = (case r of 
		  []         => []
		| (_,t1)::r1 => union(ftv(t1),ftv(TRecord(r1))))
|  	ftv (THole(a))   = [THole(a)]
|	ftv (TList(t1))  = ftv(t1);
