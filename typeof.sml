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
		
	(* Need to apply isEqualArgs to each pair of types in the record type 
	   for which the labels are equal 
	   First sort, then merge into a list of pairs of types for equal labels
	   Then apply isEqualArgs to each pair returning (SOME Bool,theta') 
	   iff each type pair returns SOME Bool,
	   and theta' is the accumulated type substitution at end of applying
	   isEqualArgs to all the type pairs *)
	| (TRecord(r1),TRecord(r2)) =>
	
		let fun applyIsEqualArgs(l,theta) = (case (l,theta) of 
		
			  ([],theta) => (SOME Bool,theta)
			| ((t1,t2)::rest,theta) => (case isEqualArgs(t1,t2,theta) of 
			
				  (SOME Bool,theta1) => applyIsEqualArgs(rest,theta1)
				| (_,theta1)         => (NONE,theta1)))
			
		in (case Record.merge(r1,r2) of
			  NONE => (NONE,theta)
			| SOME l => applyIsEqualArgs(l,theta))
		end
		
	| (THole(TypeHole(EqualityTypeVar(tyvar))),TRecord(r)) => 
	
		let val genType = genFreshTRecord(Record.getLabels(r),EQUALITY_TYPE_VAR,theta);
			val theta1 = Substitution.union(theta,TypeHole(EqualityTypeVar(tyvar)),genType)
		in typeOp(BoolOper(EQ),genType,TRecord(r),theta1) end

	| (TRecord(r),THole(TypeHole(EqualityTypeVar(tyvar)))) =>
	
		let val genType = genFreshTRecord(Record.getLabels(r),EQUALITY_TYPE_VAR,theta);
			val theta1 = Substitution.union(theta,TypeHole(EqualityTypeVar(tyvar)),genType)
		in typeOp(BoolOper(EQ),genType,TRecord(r),theta1) end
		
	| (TRecord(r),THole(TypeHole(TypeVar(tyvar)))) =>
	
		let val genType = genFreshTRecord(Record.getLabels(r),TYPE_VAR,theta);
			val theta1 = Substitution.union(theta,TypeHole(TypeVar(tyvar)),genType)
		in typeOp(BoolOper(EQ),genType,TRecord(r),theta1) end
		
	| (THole(TypeHole(TypeVar(tyvar))),TRecord(r)) =>
	
		let val genType = genFreshTRecord(Record.getLabels(r),TYPE_VAR,theta);
			val theta1 = Substitution.union(theta,TypeHole(TypeVar(tyvar)),genType)
		in typeOp(BoolOper(EQ),genType,TRecord(r),theta1) end
		
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
					
	| _ => (NONE,theta));
	
(* ----------------------------------------------------------------------------------- *)
   
fun typeofhole (SimpleHole(ValueHole(tyVar)),theta:typeSub) =  
	(SOME (resolveChainTheta(THole(TypeHole(tyVar)),theta)), theta)
	
|  	typeofhole (BinaryOpHole(ArithOper(oper),v1,v2),theta) =
	typeofexpr(ArithExpr(oper,Value(v1),Value(v2)),theta)

|	typeofhole (BinaryOpHole(BoolOper(oper),v1,v2),theta) =
	typeofexpr(BoolExpr(oper,Value(v1),Value(v2)),theta)
	
|	typeofhole (CaseHole(v,pat,expr),theta) =
	typeofexpr(Case(Value(v),pat,expr),theta)

| 	typeofhole (ConditionHole(v,e2,e3),theta) =
	typeofexpr(Condition(Value(v),e2,e3),theta)
	
| 	typeofhole (AppHole(v1,v2),theta) =
	typeofexpr(App(Value(v1),Value(v2)),theta)
	
|	typeofhole (RecordHole(r),theta) =
	let fun valToERecord(r) = (case r of 
		  []		    => []
		| (lab1,v1)::r1 => (lab1,Value(v1))::valToERecord(r1))
	in typeofexpr(Record(valToERecord(r)),theta) end
   
and typeofVRecord (r,theta) = (case r of 
	(* calculate type of a value record (i.e. a list of (label,value) pairs)
	   in left-to-right manner *)
	   
	  [] => (SOME (TRecord([])),theta)
	| (lab1,v1)::r1 => (case typeof(v1,theta) of
		
			  (NONE,theta1) => (NONE,theta1)
			| (SOME t1,theta1) => (case typeofVRecord(r1,theta1) of 
		
				  (SOME (TRecord(tList)),theta2) => 
				  (* get latest type substitution for t1, after calculating type of rest of record *)
					(SOME (TRecord((lab1,resolveChainTheta(t1,theta2))::tList)),theta2)
					
				| (_,theta2) => (NONE,theta2))))
	 
and typeof (v,theta) = (case v of

	  Concrete(N(_)) => (SOME Int,theta)
	| Concrete(B(_)) => (SOME Bool,theta)
    | Concrete(R(_)) => (SOME Real,theta)
	
	| Fun(x,t1,e) => (case typeofexpr(substitute(e, [(x,Value(gen(t1,theta)))]),theta) of
	
		  (NONE,theta1) => (NONE,theta1)
		  (* get the latest type substitution for t1, after calculating t2 *)
		| (SOME(t2),theta1) => (SOME (TFun(resolveChainTheta(t1,theta1),t2)),theta1))
		
	| VHole(h) => typeofhole(h,theta)
	
	| VRecord(r) => typeofVRecord(r,theta))
 
and typeofERecord (r,theta) = (case r of 
	(* calculate type of an expression record (i.e. a list of (label,expression) pairs)
	   in left-to-right manner *)
	   
	  [] => (SOME (TRecord([])),theta)
	| (lab1,e1)::r1 => (case typeofexpr(e1,theta) of
		
			  (NONE,theta1) => (NONE,theta1)
			| (SOME t1,theta1) => (case typeofERecord(r1,theta1) of 
		
				  (SOME (TRecord(tList)),theta2) => 
				  (* get latest type substitution for t1, after calculating type of rest of record *)
					(SOME (TRecord((lab1,resolveChainTheta(t1,theta2))::tList)),theta2)
					
				| (_,theta2) => (NONE,theta2))))
  
and typeofexpr(Value(v),theta) = typeof(v,theta)

| 	typeofexpr(Variable(x),theta) = 
	(* should not occur as all bound variables are substituted for
	   but if free variable, throw exception *)
	raise FreeVariable

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

|	typeofexpr(c as Case(e1,pat,e2),theta) = (case typeofexpr(e1,theta) of 

		  (NONE,theta1) => (NONE,theta1)
		| (SOME t,theta1) => (case matchTypes(t,pat,[],theta1) of 
		  
			  NONE => (NONE,theta1)
			| SOME (sub,theta2) => typeofexpr(substitute(e2,sub),theta2)))
			
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

	  (SOME (TFun(tA,tB)),theta1) => (case typeofexpr(e2,theta1) of 
	
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
			in (case unify([THole(TypeHole(TypeVar(a))),TFun(tC,freshType)],theta2) of 
			
				  NONE => (NONE,theta2)
				| SOME theta3 => (SOME freshType,theta3))
			end)
		
	| (_,theta1) => (NONE,theta1))
	
| 	typeofexpr(Record(l),theta) = typeofERecord(l,theta);