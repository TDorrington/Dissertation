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
		(* No need to unify for 'type variable = record/list' cases below 
		   The generated record/lists contain only fresh type variables
		   so no need for occurs check *)
		   
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
			
		| (TRecord(r),THole(tyvar as TypeHole(EqualityTypeVar(_)))) =>
			let val genType = TRecord(genFreshTRecord(Record.getLabels(r),EQUALITY_TYPE_VAR,theta));
				val theta1 = Substitution.union(theta,tyvar,genType)
			in isEqualArgs(genType,TRecord(r),theta1) end
			
		| (h as THole(TypeHole(EqualityTypeVar(_))),r as TRecord(_)) => isEqualArgs(r,h,theta)
		
		| (TRecord(r),THole(tyvar as TypeHole(TypeVar(_)))) =>
			let val genType = TRecord(genFreshTRecord(Record.getLabels(r),TYPE_VAR,theta));
				val theta1 = Substitution.union(theta,tyvar,genType)
			in isEqualArgs(genType,TRecord(r),theta1) end
			
		| (h as THole(TypeHole(TypeVar(_))),r as TRecord(_)) => isEqualArgs(r,h,theta)
			
		| (TList(tl1),TList(tl2)) => isEqualArgs(tl1,tl2,theta)
		
		| (TList(t1),THole(tyvar as TypeHole(TypeVar(_)))) =>
			let val genType = TList(generateFreshTypeVar(TYPE_VAR,theta));
				val theta1 = Substitution.union(theta,tyvar,genType)
			in isEqualArgs(genType,TList(t1),theta1) end
			
		| (h as THole(TypeHole(TypeVar(_))),l as TList(_)) => isEqualArgs(l,h,theta)	
		
		| (TList(t1),THole(tyvar as TypeHole(EqualityTypeVar(_)))) =>
			let val genType = TList(generateFreshTypeVar(EQUALITY_TYPE_VAR,theta));
				val theta1 = Substitution.union(theta,tyvar,genType)
			in isEqualArgs(genType,TList(t1),theta1) end
			
		| (h as THole(TypeHole(EqualityTypeVar(_))),l as TList(_)) => isEqualArgs(l,h,theta)	
		
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
	
|	typeofhole (CaseHole(v,patExprList),theta) =
	typeofexpr(Case(Value(v),patExprList),theta)

| 	typeofhole (ConditionHole(v,e2,e3),theta) =
	typeofexpr(Condition(Value(v),e2,e3),theta)
	
| 	typeofhole (AppHole(v1,v2),theta) =
	typeofexpr(App(Value(v1),Value(v2)),theta)
	
| 	typeofhole (ConsHole(v1,v2),theta) =
	typeofexpr(Cons(Value(v1),Value(v2)),theta)
	
|	typeofhole (RecordHole(r),theta) =
	let fun valToERecord(r) = (case r of 
		  []		    => []
		| (lab1,v1)::r1 => (lab1,Value(v1))::valToERecord(r1))
	in typeofexpr(Record(valToERecord(r)),theta) end
	
|	typeofhole (ListHole(lh),theta) =
	let fun valToEList(l) = (case l of 
		  []     => []
		| v1::l1 => Value(v1)::valToEList(l1))
	in typeofexpr(List(valToEList(lh)),theta) end
	
and typeofVRecord (r,theta) = (case r of 
	(* calculate type of a value record (i.e. a list of (label,value) pairs)
	   in left-to-right manner *)
	   
	  [] 			=> (SOME (TRecord([])),theta)
	| (lab1,v1)::r1 => (case typeof(v1,theta) of
		
			  (NONE,theta1)    => (NONE,theta1)
			| (SOME t1,theta1) => (case typeofVRecord(r1,theta1) of 
			
				  (* get latest type substitution for t1, after calculating type of rest of record *)
				  (SOME (TRecord(tList)),theta2) => (SOME (TRecord((lab1,resolveChainTheta(t1,theta2))::tList)),theta2)
				| (_,theta2) 				     => (NONE,theta2))))
			
(* To calculate the type of a list of values,
	(i)  Generate a list of all the types of the values, i.e. [1,2,true] -> [int,int,bool]
	(ii) See if all the types are equal
			- If they are, return that type
			- Otherwise, return a fresh general type variable (too early to restrict) *)
and typeofVList (l,theta) = 

	(* Takes a list of values, and generates a list of their types *)
	let fun getTypesList(l,theta) = (case l of 
	
		  []     => (SOME [],theta)
		| v1::l1 => (case typeof(v1,theta) of 
		
			  (NONE,theta1)	   => (NONE,theta1)
			| (SOME t1,theta1) => (case getTypesList(l1,theta1) of
				
				  (NONE,theta2) 	  => (NONE,theta2)
				| (SOME tList,theta2) => (SOME (t1::tList),theta2))))
				
	in (case getTypesList(l,theta) of
	
		  (NONE,theta1) 		 => (NONE,theta1)
		| (SOME typeList,theta1) => 
		
			if allElementsEqual(typeList)
						
			(* Doesn't matter which type we pick, as long as type list not empty
			   If it is empty, fail instead of generating a fresh type variable
			   Only allow for empty list of values via EmptyList constructor *)
			then (case typeList of []       => (NONE,theta1)
								 | t::tRest => (SOME (TList(t)),theta1))
											 
			(* If we fail to match all types to the exact same type, return original theta *)
			else (SOME (TList(generateFreshTypeVar(TYPE_VAR,theta1))),theta))

	end
	 
and typeof (v,theta) = (case v of

	  Concrete(N(_)) 	  => (SOME Int,theta)
	| Concrete(B(_)) 	  => (SOME Bool,theta)
    | Concrete(R(_)) 	  => (SOME Real,theta)
	| Concrete(EmptyList) => (SOME (TList(generateFreshTypeVar(TYPE_VAR,theta))),theta)
	| VHole(h)  		  => typeofhole(h,theta)
	| VList(l)   		  => typeofVList(l,theta)
	| VRecord(r) 		  => typeofVRecord(r,theta)
	
	| Fun(x,t1,e) => (case typeofexpr(substitute(e, [(x,Value(gen(t1,theta)))]),theta) of
	
		  (NONE,theta1)     => (NONE,theta1)
		  (* get the latest type substitution for t1, after calculating t2 *)
		| (SOME(t2),theta1) => (SOME (TFun(resolveChainTheta(t1,theta1),t2)),theta1)))
		
and typeofERecord (r,theta) = (case r of 
	(* calculate type of an expression record (i.e. a list of (label,expression) pairs)
	   in left-to-right manner *)
	   
	  [] 		    => (SOME (TRecord([])),theta)
	| (lab1,e1)::r1 => (case typeofexpr(e1,theta) of
		
			  (NONE,theta1)    => (NONE,theta1)
			| (SOME t1,theta1) => (case typeofERecord(r1,theta1) of 
			
				  (* get latest type substitution for t1, after calculating type of rest of record *)
				  (SOME (TRecord(tList)),theta2) => (SOME (TRecord((lab1,resolveChainTheta(t1,theta2))::tList)),theta2)	
				| (_,theta2) 				     => (NONE,theta2))))
				
(* To calculate the type of a list of expressions,
	(i)  Generate the list of all the types of the values, 
	     i.e. [1+1,2=2,if true then 3.0 else 4.0] -> [int,bool,real]
	(ii) See if all the types are equal
			- If they are, return that type
			- Otherwise, return a fresh general type variable (too early to restrict) *)
and typeofEList (l,theta) = 

	(* Takes a list of expressions, and generates a list of their types *)
	let fun getTypesList(l,theta) = (case l of 
	
		  []     => (SOME [],theta)
		| e1::l1 => (case typeofexpr(e1,theta) of 
		
			  (NONE,theta1)    => (NONE,theta1)
			| (SOME t1,theta1) => (case getTypesList(l1,theta1) of
				
				  (NONE,theta2)       => (NONE,theta2)
				| (SOME tList,theta2) => (SOME (t1::tList),theta2))))
				
	in (case getTypesList(l,theta) of
	
		  (NONE,theta1) 		 => (NONE,theta1)
		| (SOME typeList,theta1) => 
		
			if allElementsEqual(typeList)
			then (case typeList of []       => (NONE,theta1)
								 | t::tRest => (SOME (TList(t)),theta1))						 
			else (SOME (TList(generateFreshTypeVar(TYPE_VAR,theta1))),theta))
			
	end
  
and typeofexpr(Value(v),theta) = typeof(v,theta)

| 	typeofexpr(Variable(x),theta) = 
	(* should not occur as all bound variables are substituted for
	   but if free variable, throw exception *)
	raise FreeVariable

| 	typeofexpr(ArithExpr(oper,e1,e2),theta) = (case typeofexpr(e1,theta) of
	  
	  (NONE,theta1)    => (NONE,theta1)
	| (SOME t1,theta1) => (case typeofexpr(e2,theta1) of
		  
		  (NONE,theta2)    => (NONE,theta2)
		| (SOME t2,theta2) => typeOp(ArithOper(oper),t1,t2,theta2)))
				
|	typeofexpr(BoolExpr(oper,e1,e2),theta) = (case typeofexpr(e1,theta) of 
	
	  (NONE,theta1)     => (NONE,theta1)
	| (SOME(t1),theta1) => (case typeofexpr(e2,theta1) of
		
		  (NONE,theta2)	    => (NONE,theta2)
		| (SOME(t2),theta2) => typeOp(BoolOper(oper),t1,t2,theta2)))

(* For 'case e of pat1=>e1 | pat2 => e2 | ... | patn=>en'
	(i)   get type of e
	(ii)  match all patterns to type e, forming a list of expression-gamma pairs
	(iii) calculate all types of resulting expressions in the list
		  (after performing substitution) to obtain a list of all these types
	(iv)  if all the types are equal (using ML = operator), return that type
	      otherwise return a fresh generic type variable 
	c.f. if-then-else expression about not restricting too early *)
|	typeofexpr(c as Case(e1,patExprList),theta) = (case typeofexpr(e1,theta) of 
		
		  (NONE,theta1)   => (NONE,theta1)
		| (SOME t,theta1) => (case matchTypesList(t,patExprList,[],theta1) of 
		  
			  NONE => (NONE,theta1)
			  
			| SOME (exprSubList,theta2) => 
			
				(* function to transform expression-substitution pairs
				   into list of types of those expressions
				   after performing the relevant substitution *)
				let fun iterCalcExprType(l,theta) = (case l of 
				
					  [] 			   => (SOME([]),theta)
					| (expr1,sub1)::l1 => (case typeofexpr(substitute(expr1,sub1),theta) of 
					
						  (NONE,theta1)   => (NONE,theta1)
						| (SOME t,theta1) => (case iterCalcExprType(l1,theta1) of 
						
							  (NONE,theta2)          => (NONE,theta2)
							| (SOME typeList,theta2) => (SOME (t::typeList),theta2))))
						
				in (case iterCalcExprType(exprSubList,theta2) of 
				
					(* If any of expressions fail to type, return original theta *)
					  (NONE,theta3) => (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta)
					  
					| (SOME typeList,theta3) => 
						
						if allElementsEqual(typeList)
						
						(* doesn't matter which type we pick, as long as type list not empty
						   if it is empty, fail instead of generating a fresh type variable
						   "case e of []" is not valid *)
						then (case typeList of []       => (NONE,theta3)
											 | t::tRest => (SOME t,theta3))
											 
						(* if we fail to match all types to the exact same type,
						   return original theta *)
						else (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta))
						
				end))
			
|	typeofexpr(Condition(e1,e2,e3),theta) = (case typeofexpr(e1,theta) of 
	
	  (NONE,theta1)     => (NONE,theta1)
	| (SOME(t1),theta1) => 
		
		let fun calculateReturn(theta1) = (case typeofexpr(e2,theta1) of
		(* In the cases where we failed to type the two sub expressions to the exact
		   same type, just return the original type substitution map passed in *)
			
			  (NONE,theta2)     => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta1)
			| (SOME(t2),theta2) => (case typeofexpr(e3,theta2) of
				
				  (NONE,theta3)     => (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta1)
				| (SOME(t3),theta3) => if (t2=t3) 
									   then (SOME t2,theta3)
									   else (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta1)))
					
		in (case t1 of
			
			  Bool 								  => calculateReturn(theta1)
			| THole(TypeHole(TypeVar(a))) 		  => calculateReturn(Substitution.union(theta1,TypeHole(TypeVar(a)),Bool))
			| THole(TypeHole(EqualityTypeVar(a))) => calculateReturn(Substitution.union(theta1,TypeHole(EqualityTypeVar(a)),Bool))
			| _                                   => (NONE,theta1))
				
		end)
		
|	typeofexpr(App(e1,e2),theta) = (case typeofexpr(e1,theta) of 

	  (SOME (TFun(tA,tB)),theta1) => (SOME tB,theta1)
			
	| (SOME (THole(TypeHole(TypeVar(a)))),theta1) => (case typeofexpr(e2,theta1) of 
		
		  (NONE,theta2)    => (NONE,theta2)
		  
		| (SOME tC,theta2) => 
		
			let val freshType = generateFreshTypeVar(TYPE_VAR,theta2)
			in (case unify([THole(TypeHole(TypeVar(a))),TFun(tC,freshType)],theta2) of 
			
				  NONE        => (NONE,theta2)
				| SOME theta3 => (SOME freshType,theta3))
			end)
		
	| (_,theta1) => (NONE,theta1))
	
| 	typeofexpr(Record(l),theta) = typeofERecord(l,theta)

|	typeofexpr(List(l),theta) = typeofEList(l,theta)

| 	typeofexpr(Cons(e1,e2),theta) = (case typeofexpr(e2,theta) of

	  (NONE,theta1)    => (NONE,theta1)
	| (SOME t2,theta1) => (case typeofexpr(e1,theta1) of 
	
		  (NONE,theta2)    => (NONE,theta2)
		| (SOME t1,theta2) => (case t2 of 
	
			  TList(listType) => (case unify([t1,listType],theta2) of 
			  
				  NONE 		  => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta)
				| SOME theta3 => (SOME (resolveChainTheta(t2,theta3)),theta3))
			  
			| THole(TypeHole(TypeVar(a))) => 
			
				let val freshTypeVar = generateFreshTypeVar(TYPE_VAR,theta2) 
				in (case unify([t1,freshTypeVar],theta2) of 
			
					  NONE        => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta)
					| SOME theta3 => (case unify([t2,TList(resolveChainTheta(freshTypeVar,theta3))],theta3) of
					
						  NONE 		  => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta)
						| SOME theta4 => (SOME (TList(resolveChainTheta(freshTypeVar,theta4))),theta4)))
						
				end
			
			| THole(TypeHole(EqualityTypeVar(a))) => 
				
				let val freshTypeVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta2) 
				in (case unify([t1,freshTypeVar],theta2) of 
			
					  NONE        => (SOME (generateFreshTypeVar(TYPE_VAR,theta2)),theta)
					| SOME theta3 => (case unify([t2,TList(resolveChainTheta(freshTypeVar,theta3))],theta3) of
					
						  NONE 		  => (SOME (generateFreshTypeVar(TYPE_VAR,theta3)),theta)
						| SOME theta4 => (SOME (TList(resolveChainTheta(freshTypeVar,theta4))),theta4)))
						
				end
			
			| _ => (NONE,theta1))))

|	typeofexpr(CounterExpr(e1,_),theta) = typeofexpr(e1,theta)
			
	(* Don't check type of e1 is unifiable to type t: done in narrow *)
| 	typeofexpr(Let(x,t,_,e2),theta) = typeofexpr(substitute(e2, [(x,Value(gen(t,theta)))]),theta)

	(* c.f. typeofexpr(LetRec(x,TFun(t1,t2),Fun(y,t3,e1),e2),theta)
	   Don't check type of e1 is unifiable to type t2, or that t3 is unifiable to t1; done in narrow *)
|	typeofexpr(LetRec(x,t,_,e2),theta) = typeofexpr(substitute(e2, [(x,Value(gen(t,theta)))]),theta);