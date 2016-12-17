(* gen dynamically performs hole-filling 	
   gen takes as input a type t and returns value of that type *)

fun gen (t, theta:typeSub) = case t of

	(* For base types, returns arbitrary value of that type *)
	  Bool => Concrete(B(true))
	| Int => Concrete(N(1))
    | Real => Concrete(R(1.0))
	
	| TFun(t1,t2) => Fun(Var("x"),t1,Value(gen(t2,theta)))
	
	| TRecord(r) => 
		let fun genVRecord(r) = (case r of 
			  [] => []
			| (lab1,t1)::r1 => (lab1,gen(t1,theta))::genVRecord(r1))
		in VRecord(genVRecord(r)) end

	(* For unconstrained types, yields fresh hole constrained to that type *)
	| THole(TypeHole(a)) => if Substitution.contains(TypeHole(a),theta) 
							then gen(resolveChainTheta(THole(TypeHole(a)),theta), theta)
							else VHole(SimpleHole(ValueHole(a))); 
			
(* ---------------------------------------------------------------------------------- *)	
(* Generates a fresh type variable of the form depending on enumeration argument 
   (i.e. general type variable, equality type variable, or arithmetic type variable)
   We also need to check the generated type variable is neither in the domain or range
   of the current substitution. If it is, keep re-calling function *)
fun generateFreshTypeVar(typeVarEnum,theta:typeSub) =

	let val freshTypeVar = case typeVarEnum of
	
		  TYPE_VAR => TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())))
		  
		| EQUALITY_TYPE_VAR => TypeHole(EqualityTypeVar("a" ^ Int.toString(getCounterAndUpdate())))
		
		| ARITH_TYPE_VAR => TypeHole(ArithTypeVar("a" ^ Int.toString(getCounterAndUpdate())))

	in 
		if element(Substitution.domain(theta),freshTypeVar) orelse element(Substitution.range(theta),THole(freshTypeVar))
		then generateFreshTypeVar(typeVarEnum,theta)
		else THole(freshTypeVar)
	end;
	
(* ---------------------------------------------------------------------------------- *)
(* Takes a list of labels, a type var enumeration constructor and current
   type substitution theta,
   and generates a record type consisting of the labels in the list of labels,
   all of type of a unique fresh type variable *)
fun genFreshTRecord(l,tVar,theta) = 
	let fun genList(l) = (case l of 
		  [] => []
		| (lab1::l1) => (lab1,generateFreshTypeVar(tVar,theta))::genList(l1))
	in TRecord(genList(l)) end;
								