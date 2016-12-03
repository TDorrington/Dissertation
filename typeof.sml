fun typeOp(ArithOper(DIVIDE),t1,t2,theta:typeSub) = (case (t1,t2) of

	  (Real,Real) => (SOME Real,theta)
				  
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Real, Substitution.union(theta, TypeHole(ArithTypeVar(a)),Real))
			
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Real, Substitution.union(theta, TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),Real),
									   TypeHole(TypeVar(b)),Real))
												   
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(TypeVar(b)))) => 
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real),
									   TypeHole(TypeVar(b)),Real))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),Real),
									   TypeHole(TypeVar(a)),Real))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real),
									   TypeHole(ArithTypeVar(b)),Real))
				
	| _ => (NONE,theta))
	
|	typeOp(ArithOper(_),t1,t2,theta) = (case (t1,t2) of

	  (Real,Real) => (SOME Real, theta)
				  
	| (Int,Int)   => (SOME Int, theta)
				
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (Int, THole(hole)) => (SOME Int, Substitution.union(theta,hole,Int))
					
	| (THole(hole), Int) => (SOME Int, Substitution.union(theta,hole,Int))
				
	| (THole(TypeHole(hole1)),THole(TypeHole(EqualityTypeVar(b)))) =>
		(SOME Int, Substitution.union(Substitution.union(theta,TypeHole(hole1),Int),
									  TypeHole(EqualityTypeVar(b)),Int))
												  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(hole2))) =>
		(SOME Int, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Int),
									  TypeHole(hole2),Int))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a=b
		then (SOME (THole(TypeHole(ArithTypeVar(a)))), theta)
		else let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			 in (SOME freshVar, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),freshVar),
												   TypeHole(ArithTypeVar(a)),freshVar))
			 end
				
	| (THole(TypeHole(thole1)),THole(TypeHole(thole2))) => 
		let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
		in (SOME freshVar, Substitution.union(Substitution.union(theta,TypeHole(thole1),freshVar),
											  TypeHole(thole2),freshVar))
		end
					
	| _ => (NONE,theta))

|	typeOp(BoolOper(EQ),t1,t2,theta) = 

	let fun isEqualArgs(t1,t2,theta) =  (case (t1,t2) of 
	
	  (Int,Int) => (SOME Bool,theta)
				  
	| (Int,THole(hole)) => (SOME Bool,Substitution.union(theta,hole,Int))
				
	| (THole(hole),Int) => (SOME Bool,Substitution.union(theta,hole,Int))
				
	| (Bool,Bool) => (SOME Bool,theta)
				
	| (Bool,THole(TypeHole(TypeVar(a)))) => 
		(SOME Bool,Substitution.union(theta,TypeHole(TypeVar(a)),Bool))
					
	| (Bool,THole(TypeHole(EqualityTypeVar(a)))) => 
		(SOME Bool,Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Bool))
					
	| (THole(TypeHole(TypeVar(a))),Bool) => 
		(SOME Bool,Substitution.union(theta,TypeHole(TypeVar(a)),Bool))
					
	| (THole(TypeHole(EqualityTypeVar(a))),Bool) => 
		(SOME Bool,Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Bool))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a = b
		then (SOME Bool, theta)
		else let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
			 in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),freshVar),
											   TypeHole(TypeVar(b)),freshVar))
			 end
											  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),freshVar),
										  TypeHole(TypeVar(b)),freshVar))
		end
												  
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(EqualityTypeVar(b)))) =>
		let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),freshVar),
										  TypeHole(EqualityTypeVar(b)),freshVar))
		end
						
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(EqualityTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a = b
		then (SOME Bool, theta)
		else let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
			 in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),freshVar),
											   TypeHole(EqualityTypeVar(b)),freshVar))
			 end
				
	| (THole(hole1),THole(hole2)) => 
		(SOME Bool, Substitution.union(Substitution.union(theta,hole1,Int),
											 hole2,Int))
					
	| (Pair(t11,t12),Pair(t21,t22)) =>
		let val (arg0,theta1) = isEqualArgs(t11,t21,theta);
			val (arg1,theta2) = isEqualArgs(t12,t22,theta1)
		in case (arg0,arg1) of 
			(SOME Bool, SOME Bool) => (SOME Bool,theta2)
							   | _ => (NONE,theta2)
		end
					 
	| _ => (NONE,theta))
	
	in isEqualArgs(t1,t2,theta) end

|	typeOp(BoolOper(_),t1,t2,theta) = (case (t1,t2) of 

	  (Real,Real) => (SOME Bool, theta)
				  
	| (Int,Int)   => (SOME Bool, theta)
				
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Bool, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
				
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Bool, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Bool, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Bool, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (Int, THole(hole)) => (SOME Bool, Substitution.union(theta,hole,Int))
					
	| (THole(hole), Int) => (SOME Bool, Substitution.union(theta,hole,Int))
					
	| (THole(TypeHole(hole1)),THole(TypeHole(EqualityTypeVar(b)))) =>
		(SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(hole1),Int),
									   TypeHole(EqualityTypeVar(b)),Int))
												  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(hole2))) =>
		(SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Int),
									   TypeHole(hole2),Int))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a=b
		then (SOME Bool, theta)
		else let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),freshVar),
											  TypeHole(ArithTypeVar(a)),freshVar))
			 end
				
	| (THole(TypeHole(thole1)),THole(TypeHole(thole2))) => 
		let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(thole1),freshVar),
										  TypeHole(thole2),freshVar))
		end
					
	| _ => (NONE,theta))
	
(* for match warning - never actually used *)
| 	typeOp(EXPR_PAIR,_,_,theta) = (NONE,theta);
	
(* ----------------------------------------------------------------------------------- *)
(* Returns the type of a value hole, which may be simple or compound
   Use typeofexpr to implement typeofhole by 'unwrapping' the value hole
   e.g. typeofhole(v[case 'a of (x,y) -> (x+y)]) => 
        typeofexpr(case v['a] of (x,y) -> (x+y))*)
   
fun typeofhole(SimpleHole(ValueHole(tyVar)),theta:typeSub) =  
	(SOME (resolveChainTheta(THole(TypeHole(tyVar)),theta)), theta)
	
|  	typeofhole (BinaryOp(EXPR_PAIR,v1,v2),theta) =
	typeofexpr(ExpressionPair(Value(v1),Value(v2)),theta)

|  	typeofhole (BinaryOp(ArithOper(oper),v1,v2),theta) =
	typeofexpr(ArithExpr(oper,Value(v1),Value(v2)),theta)

|	typeofhole (BinaryOp(BoolOper(oper),v1,v2),theta) =
	typeofexpr(BoolExpr(oper,Value(v1),Value(v2)),theta)
	
|	typeofhole (CaseHole(v,pat,expr),theta) =
	typeofexpr(Case(Value(v),pat,expr),theta)

| 	typeofhole (ConditionHole(v,e2,e3),theta) =
	typeofexpr(Condition(Value(v),e2,e3),theta)
	
| 	typeofhole (AppHole(v1,v2),theta) =
	typeofexpr(App(Value(v1),Value(v2)),theta)
	
(* no semi-colon: mutually recursive with typeof and typeofexpr *)
(* ----------------------------------------------------------------------------------- *)	
(* typeof returns dynamic type of a value
   Returns pair of type (wrapped in option datatype) and a type substitution *)
   
and typeof (v,theta) = (case v of

	  N(_) => (SOME Int,theta)
	| B(_) => (SOME Bool,theta)
    | R(_) => (SOME Real,theta)
	
	| Func(x,t1,e) => (case typeofexpr(substitute(e, [(x,Value(gen(t1,theta)))]),theta) of
	
		  (NONE,theta1) => (NONE,theta1)
		  (* get the latest type substitution for t1, after calculating t2 *)
		| (SOME(t2),theta1) => (SOME (Fun(resolveChainTheta(t1,theta1),t2)),theta1))
			
	| ValuePair(v1,v2) => (case typeof(v1,theta) of
		
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(a),theta1) => (case typeof(v2,theta1) of
			  
				  (NONE,theta2) => (NONE,theta2)
				  (* get the latest type substitution for a, after calculating b *)
				| (SOME(b),theta2) => (SOME (Pair(resolveChainTheta(a,theta2),b)),theta2)))
					
	| VHole(h) => typeofhole(h,theta))
	
(* no semi-colon: mutually recursive with typeofhole and typeofexpr *)
(* ----------------------------------------------------------------------------------- *)
(* typeofexpr returns dynamic type of an expression and type substitution theta *)
   
and typeofexpr(Value(v),theta) = typeof(v,theta)

	(* should not occur as all bound variables are substituted for
	   but if free variable, throw exception *)
| 	typeofexpr(Variable(x),theta) = raise FreeVariable

| 	typeofexpr(ArithExpr(oper,e1,e2),theta) = (case typeofexpr(e1,theta) of
	  
	  (NONE,theta1) => (NONE,theta1)
	| (SOME t1,theta1) => (case typeofexpr(e2,theta1) of
		  
		  (NONE,theta2) => (NONE,theta2)
		| (SOME t2,theta2) => typeOp(ArithOper(oper),t1,t2,theta2)))
				
|	typeofexpr(BoolExpr(oper,e1,e2),theta) = (case typeofexpr(e1,theta) of 
	
	  (NONE,theta1) => (NONE,theta1)
	| (SOME(t1),theta1) => (case typeofexpr(e2,theta1) of
		
		  (NONE,theta2) => (NONE,theta2)
		| (SOME(t2),theta2) => typeOp(BoolOper(oper),t1,t2,theta2)))

| 	typeofexpr(ExpressionPair(e1,e2),theta) = (case typeofexpr(e1,theta) of 
	
	  (NONE,theta1) => (NONE,theta1)
	| (SOME(t1),theta1) => (case typeofexpr(e2,theta1) of
		
		  (NONE,theta2) => (NONE,theta2)
		  (* get the latest type substitution for t1, after calculating t2 *)
		| (SOME(t2),theta2) => (SOME (Pair(resolveChainTheta(t1,theta2),t2)),theta2)))

|	typeofexpr(c as Case(e1,VariablePair(x1,x2),e2),theta) = (case typeofexpr(e1,theta) of 

	(* should always be capture avoiding as substitutions in any higher level
	   expressions from evaluate function will have made alpha variant *)
		
	  (SOME (THole(TypeHole(ArithTypeVar(_)))),theta1) => (NONE,theta1)
	
	| (SOME (THole(TypeHole(tyvar))),theta1) => 
		let val typevar_type = case tyvar of 
				  TypeVar(_)    	 => TYPE_VAR
				| EqualityTypeVar(_) => EQUALITY_TYPE_VAR
				| ArithTypeVar(_)  	 => ARITH_TYPE_VAR;
				(* arith cannot occur - matched above, but here for non-exhaustive warnings *)
			val t1 = generateFreshTypeVar(typevar_type,theta1);
			val t2 = generateFreshTypeVar(typevar_type,theta1);
			val gent1 = Value(gen(t1,theta1));
			val gent2 = Value(gen(t2,theta1));
			val subExpr = substitute(e2,[(x1,gent1),(x2,gent2)]);
			val theta2 = Substitution.union(theta1,TypeHole(tyvar),Pair(t1,t2))
		in typeofexpr(subExpr,theta2) end
		
	| (SOME (Pair(t1,t2)),theta1) => 
		let val gent1 = Value(gen(t1,theta1))
			val gent2 = Value(gen(t2,theta1)) 
			val gamma = [(x1,gent1),(x2,gent2)]
			val subExpr = substitute(e2,gamma)
		in typeofexpr(subExpr,theta1) end
		 
	| (_,theta1) => (NONE,theta1))
		
|	typeofexpr(Condition(e1,e2,e3),theta) = (case typeofexpr(e1,theta) of 
	
	  (NONE,theta1) => (NONE,theta1)
	| (SOME(t1),theta1) => 
		
		let fun calculateReturn(theta1) = (case typeofexpr(e2,theta1) of
		(* In the cases where we failed to type the two sub expressions to the exact
		   same type, just return the original type substitution map passed in *)
			
			  (NONE,theta2) => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta1)
			| (SOME(t2),theta2) => (case typeofexpr(e3,theta2) of
				
				  (NONE,theta3) => (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta1)
				| (SOME(t3),theta3) => 
						
					if (t2=t3) 
					then (SOME t2,theta3)
					else (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta1)))
					
		in (case t1 of
			
			  Bool => calculateReturn(theta1)
				  
			| THole(TypeHole(TypeVar(a))) => calculateReturn(Substitution.union(theta1,TypeHole(TypeVar(a)),Bool))
					
			| THole(TypeHole(EqualityTypeVar(a))) => calculateReturn(Substitution.union(theta1,TypeHole(EqualityTypeVar(a)),Bool))
					
			| _ => (NONE, theta1))
				
		end)
		
|	typeofexpr(App(e1,e2),theta) = (case typeofexpr(e1,theta) of 

	  (SOME (Fun(tA,tB)),theta1) => (case typeofexpr(e2,theta1) of 
	
			  (NONE,theta2) => (SOME tB,theta2)
			  (* even in the case we cannot type t2, return same type tb as its already known *)
			  
			| (SOME tC,theta2) => 
				(* even in the case not equal, return same type tb as its already known *)
				if tA=tC 
				then (SOME tB,theta2)
				else (SOME tB,theta2))
			
	| (SOME (THole(TypeHole(TypeVar(a)))),theta1) => (case typeofexpr(e2,theta1) of 
		
		  (NONE,theta2) => (NONE,theta2)
			  
		| (SOME tC,theta2) => 
		
			let val freshType = generateFreshTypeVar(TYPE_VAR,theta2)
			in (case unify([THole(TypeHole(TypeVar(a))),Fun(tC,freshType)],theta2) of 
			
				  NONE => (NONE,theta2)
				| SOME theta3 => (SOME freshType,theta3))
			end)
		
	| (_,theta1) => (NONE,theta1));