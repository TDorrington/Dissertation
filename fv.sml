(* Takes an expression and returns a set containing the free variables in that expression
   Only non-trivial case is the case expression
   For case e1 of (x1,x2) -> e2
   We find the free variables of e2, and remove from that set the variables x1 and x2, and
   then finally union that with the free variables of e1
   For example, 
   case (x,y) of (y,z) -> x+y = ({x,y}-{y,z}) union ({x,y}) = {x,y} *)

fun fvExpr(e) = case e of 

	(* Compound value holes can contain expressions *)
	
	  Value(VHole(hole)) => (case hole of 
	  
		  SimpleHole(_) => []
		| BinaryOp(_,v1,v2) => union(fvExpr(Value(v1)),fvExpr(Value(v2)))
		| ConditionHole(v1,e1,e2) => union(union(fvExpr(Value(v1)),fvExpr(e1)),fvExpr(e2))
		| CaseHole(v1,VariablePair(x,y),e) => union(fvExpr(Value(v1)),remove(fvExpr(e),[x,y])))
		
	| Value(ValuePair(v1,v2)) => union(fvExpr(Value(v1)),fvExpr(Value(v2)))
	
	| Value(_) => [] (* int, real or bool *)
	| Variable(x) => [x]
	| ArithExpr(_,e1,e2)    => union(fvExpr(e1),fvExpr(e2))
	| BoolExpr (_,e1,e2)    => union(fvExpr(e1),fvExpr(e2))
	| ExpressionPair(e1,e2) => union(fvExpr(e1),fvExpr(e2))
	| Condition(e1,e2,e3)   => union(union(fvExpr(e1),fvExpr(e2)),fvExpr(e3))
	| Case (e1,VariablePair(x,y),e2) => union(fvExpr(e1),remove(fvExpr(e2),[x,y]));
	
fun fv ([]) = []
|   fv (e::eList) = union(fvExpr(e),fv(eList));

(* ----------------------------------------------------------------------------------- *)
(* Takes an expression and returns a set containing the free type variables in that expression *)

fun ftv (Real) = []
| 	ftv (Int)  = []
| 	ftv (Bool) = []
|  	ftv (THole(a)) = [THole(a)]
|	ftv (Pair(t1,t2)) = append(ftv(t1),ftv(t2));


