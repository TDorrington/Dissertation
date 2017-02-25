(* Thrown if we attempt to fuzz a value hole - this case should never occur in our top-level expressions *)
exception ValueHoleExn;

(* Thrown if input expression not in the correct format, 
   i.e. all expressions should be wrapped in a CounterExpr datatype *)
exception CounterExpression;

local 

(*  Returns a type different to that of the argument if possible
    We cannot fuzz a type hole, since it could be any type, and hence are unable 
    to generate a different type: we could not check if it is different till run time *)
   fun fuzzType(t) = (case t of 
		
		  Bool 		 => SOME (Int)
		| Int  		 => SOME (Real)
		| Real 		 => SOME (TList(Int))
		| TFun(_,_)  => SOME (TRecord([(Lab("a"),Int)]))
		| TRecord(_) => SOME (TFun(Int,TList(Bool)))
		| TList(_)   => SOME (Bool)
		| THole(_)   => NONE); 
	
	(* Takes the hand-written variables->types list,
       and the variable we want to find out the type for *)
	fun getVarType(x,var_types) = (case var_types of 
	
		  []            => raise FreeVariable
		| (y,t_y)::rest => if x=y then t_y else getVarType(x,rest));
	
	(* Takes a type of the variable we want to fuzz, and returns (if possible) a different variable that is
	   a) of another type, another
	   b) in the same scope (i.e. in the environment, a list of variables) *)
	fun getDifferentVariable(x,t_x,var_types,environment) = (case var_types of
	 
		  []            => NONE
		  
		| (y,t_y)::rest => if x<>y andalso t_x<>t_y andalso element(environment,y)
						   then SOME y
						   else getDifferentVariable(x,t_x,rest,environment));
	
(*  Takes the sub expression we wish to fuzz, and returns a triple containing
        - the fuzzed sub expression, 
		- the number of the expression actually changed
		- a list of numbers, representing the expressions that might now
		  get stuck due to the change (container expressions & sometimes daughters)
		  takes an evironment, which contains the variables currently in scope,
	and a list of variables->types (hand-generated) *)
	fun fuzzSingleExpr(e,n,var_types,environment) = (case e of 
	
		(* For values, switch to a value of another type *)
		  Value(Concrete(N(_)))      => (Value(Concrete(R(2.0))),n,[n])
		| Value(Concrete(R(_)))      => (Value(Concrete(B(false))),n,[n])
		| Value(Concrete(B(_)))      => (Value(Concrete(N(2))),n,[n])
		| Value(Concrete(EmptyList)) => (Value(VRecord([])),n,[n])
		
		(* Convert list to a record, keeping the same expressions, and generating unique lables i.e.
		   [v0,v1,...,vn] -> {a0=v0,...,a1=vn} *)
		| Value(VList(l)) => 
		
			let fun listtoVRecord(l,i) = (case l of 
				
				  []	   => []
				| v1::rest => (Lab("a"^Int.toString(i)),v1)::listtoVRecord(rest,i+1))
		
			in (Value(VRecord(listtoVRecord(l,0))),n,[n]) end
		
		(* If we can, remove a (label,value) pair *)
		| Value(VRecord(entry::rest)) => (Value(VRecord(rest)),n,[n])
		
		(* If no (label,value) pairs in record, convert to an empty list *)
		| Value(VRecord([])) => (Value(Concrete(EmptyList)),n,[n])
		
		(* Add function container number if we recurse *)
		| Value(Fun(x,t,e1)) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Value(Fun(x,fuzzT,e1)),n,[n])
			| NONE 		 => let val (fuzzE1,i,l) = fuzzSingleExpr(e1,n,var_types,x::environment)
							in (Value(Fun(x,t,fuzzE1)),i,n::l) end)
		 
		(* Value holes should not occur in programs - raise exception *)
		| Value(VHole(_)) => raise ValueHoleExn
		
		(* Change to a different variable of another type which is in the same scope as this one
		   If no such variable, fuzz to a value of a different type than the variable *)
		| Variable(x) => (case getDifferentVariable(x,getVarType(x,var_types),var_types,environment) of 
		
			  SOME y => (Variable(y),n,[n])
		
			| NONE   => (case fuzzType(getVarType(x,var_types)) of 
			  
				  SOME fuzzT => (Value(gen(fuzzT,[])),n,[n])
				  
				(* Shouldnt really happen - hand-generated variables->types list
				   should have enough information rather than just a plain type variable *)
				| NONE       => raise FreeVariable))
				
		(* Add daughters *)
		| ArithExpr(_,e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (Cons(e1,e2),n,[j1,j2,n])
		
		| ArithExpr(_,_,_) => raise CounterExpression
		
		(* Add daughters *)
		| BoolExpr(_,e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2))  => (App(e1,e2),n,[j1,j2,n])
		
		| BoolExpr(_,_,_) => raise CounterExpression
		
		| Case(e1,patExprList) => let val (fuzzE1,i,l) = fuzzSingleExpr(e1,n,var_types,environment)
								  in (Case(fuzzE1,patExprList),i,n::l) end
		
		| Condition(e1,e2,e3)  => let val (fuzzE1,i,l) = fuzzSingleExpr(e1,n,var_types,environment)
								  in (Condition(fuzzE1,e2,e3),i,n::l) end

		(* Add daughters *)
		| App(e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (ArithExpr(PLUS,e1,e2),n,[j1,j2,n])
		
		| App(_,_) => raise CounterExpression
		
		(* If we can, remove a (label,expression) pair *)
		| Record(entry::rest)  => (Record(rest),n,[n])
		
		(* If no (label,expression) pairs in recor, convert to an empty list *)
		| Record([])		   => (Value(Concrete(EmptyList)),n,[n])
		
		(* Convert list to a record, keeping the same expressions, and generating unique labels i.e.
		   [e0,e1,...,en] -> {a0=e0,a1=e1,...,an=en} *)
		| List(l) => 
		
			let fun listToERecord(l,i) = (case l of 
			
				  []       => []
				| e1::rest => (Lab("a"^Int.toString(i)),e1)::listToERecord(rest,i+1))
	
			in (Record(listToERecord(l,0)),n,[n]) end
		
		| Let(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Let(x,fuzzT,e1,e2),n,[n])
			| NONE       => let val (fuzzE1,i,l) = fuzzSingleExpr(e1,n,var_types,environment)
						    in (Let(x,t,fuzzE1,e2),i,n::l) end)
				
		| LetRec(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (LetRec(x,fuzzT,e1,e2),n,[n])
			| NONE 	     => let val (fuzzE2,i,l) = fuzzSingleExpr(e2,n,var_types,x::environment)
							in (LetRec(x,t,e1,fuzzE2),i,n::l) end)
					
		| Cons(e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (BoolExpr(EQ,e1,e2),n,[j1,j2,n])
		
		| Cons(_,_) => raise CounterExpression
			
		| CounterExpr(e,i) => let val (fuzzE,j,l) = fuzzSingleExpr(e,i,var_types,environment)
						      in (CounterExpr(fuzzE,i),j,l) end);
							  
(* Takes the whole top-level expression, and the number of the sub expression we wish to fuzz
   Returns the whole fuzzed expression, paired with the actual number of the expression changed
   and a list of expression numbers that may now fail due to the effect of fuzzing the sub-expression
   Assumes all expression datatypes are wrapped in the counterExpr datatype
   Takes a hand-generated list of variables->types,
   and the environment representing what variables are currently in scope (which initially should be called from top level as [])
   In each sub-call, 'siblings' contains the container number and siblings numbers for the immediate expression higher in the tree
   in the event that the sub-expression is the one we are trying to change so we can add to list of impacted expressions
   This avoids adding all continers & siblings from root of tree, as on each recursive call to fuzzExpr we replace with the latest siblings *)
in fun fuzzExpr(e,n,siblings,var_types,environment) = (case e of 
	
	  CounterExpr(e,container) => 
	  
		if container = n 
		then let val (fuzzE,j,l) = fuzzSingleExpr(e,n,var_types,environment) in SOME (CounterExpr(fuzzE,container),j,append(siblings,l)) end
		else (case e of
	
			  Value(v) => 
			
				let fun fuzzValue(v) = (case v of 
	
					  Concrete(_) => NONE
			
					| Fun(x,t,e1) => (case fuzzExpr(e1,n,[container],var_types,x::environment) of 
			
						  SOME (fuzzE1,i,l) => SOME (Fun(x,t,fuzzE1),i,l)
						| NONE			    => NONE)
				
					(* Value holes shouldn't occur in programs *)
					| VHole(_) => raise ValueHoleExn
				
					| VRecord(r) => 
			
						let fun iterVRecord(r) = (case r of 
							
							  []      		  => NONE
							| (lab1,v1)::rest => (case fuzzValue(v1) of
								
								  SOME (fuzzV1,i,l) => SOME ((lab1,fuzzV1)::rest,i,l)
								| NONE			  	=> (case iterVRecord(rest) of 
								
									  SOME (fuzzVRecord,i,l) => SOME ((lab1,v1)::fuzzVRecord,i,l)
									| NONE 				     => NONE)))
									
						in (case iterVRecord(r) of 
						
							  SOME (fuzzVRecord,i,l) => SOME (VRecord(fuzzVRecord),i,l)
							| NONE				     => NONE)
							
						end
				
					| VList(l) =>
			
						let fun iterVList(l) = (case l of 
						
							  []	   => NONE
							| v1::rest => (case fuzzValue(v1) of 
							
								  SOME (fuzzV1,i,l) => SOME (fuzzV1::rest,i,l)
								| NONE 			    => (case iterVList(rest) of 
								
									  SOME (fuzzVList,i,l) => SOME (v1::fuzzVList,i,l)
									| NONE				   => NONE)))
									
						in (case iterVList(l) of 
						
							  SOME (fuzzVList,i,l) => SOME (VList(fuzzVList),i,l)
							| NONE				   => NONE)
							
						end)
						
					in (case fuzzValue(v) of 
					
						  SOME (fuzzV,i,l) => SOME (CounterExpr(Value(fuzzV),container),i,l)
						| NONE 			   => NONE)
						
					end
			
			| Variable(_) => NONE
			
			| ArithExpr(oper,e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (case fuzzExpr(e1,n,[j2,container],var_types,environment) of 
				
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(ArithExpr(oper,fuzzE1,e2),container),i,l)
				| NONE			    => (case fuzzExpr(e2,n,[j1,container],var_types,environment) of 
				
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(ArithExpr(oper,e1,fuzzE2),container),i,l)
					| NONE			    => NONE))
				
			| ArithExpr(_,_,_) => raise CounterExpression
				
			| BoolExpr(oper,e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (case fuzzExpr(e1,n,[j2,container],var_types,environment) of 	
			
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(BoolExpr(oper,fuzzE1,e2),container),i,l)
				| NONE			    => (case fuzzExpr(e2,n,[j1,container],var_types,environment) of 
				
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(BoolExpr(oper,e1,fuzzE2),container),i,l)
					| NONE			    => NONE))
					
			| BoolExpr(_,_,_) => raise CounterExpression
					
			| Case(e,patExprList) => (case fuzzExpr(e,n,[container],var_types,environment) of 
			
				  SOME (fuzzE,i,l) => SOME (CounterExpr(Case(fuzzE,patExprList),container),i,l)
				| NONE 			   => 
				
						(* Takes a pattern-expression list corresponding to a (a subset of a) case expression 
						   and returns a list containing all the top-most numbers of the expression branches
						   Used to calculate siblings to add to list of all potential places an expression could get stuck *)
					let fun getSiblingNumbers(l) = (case l of 
						
						  []                         => []
						| (_,CounterExpr(_,j))::rest => j::getSiblingNumbers(rest)
						| _                          => raise CounterExpression)
						
						(* 'extras' is all the expression numbers of the sibling branches up to the pattern 
						   we are currently processing, initially called as [] *)
						fun iterPatExprList(l,extras) = (case l of 
					
						  [] 		         				  => NONE
						| (pat1,e1 as CounterExpr(_,j))::rest => (case fuzzExpr(e1,n,container::append(extras,getSiblingNumbers(rest)),var_types,append(fvPat(pat1),environment)) of 
						
							  SOME (fuzzE1,i,errors) => SOME ((pat1,fuzzE1)::rest,i,errors)
							| NONE 			         => (case iterPatExprList(rest,j::extras) of 
							
								  SOME (fuzzPatExprList,i,errors) => SOME ((pat1,e1)::fuzzPatExprList,i,errors)
								| NONE 					          => NONE))
								
						| _ => raise CounterExpression)
							
					in (case iterPatExprList(patExprList,[]) of 
					
						  SOME (fuzzPatExprList,i,l) => SOME (CounterExpr(Case(e,fuzzPatExprList),container),i,l)
						| NONE 					     => NONE)
						
					end)
			
			| Condition(e1,e2 as CounterExpr(_,j2),e3 as CounterExpr(_,j3)) => (case fuzzExpr(e1,n,[container],var_types,environment) of 
			
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(Condition(fuzzE1,e2,e3),container),i,l)
				| NONE 			    => (case fuzzExpr(e2,n,[j3,container],var_types,environment) of 
				
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(Condition(e1,fuzzE2,e3),container),i,l)
					| NONE 			    => (case fuzzExpr(e3,n,[j2,container],var_types,environment) of 
					
						  SOME (fuzzE3,i,l) => SOME (CounterExpr(Condition(e1,e2,fuzzE3),container),i,l)
						| NONE 			    => NONE)))
						
			| Condition(_,_,_) => raise CounterExpression
			
			| App(e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (case fuzzExpr(e1,n,[j2,container],var_types,environment) of 
			
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(App(fuzzE1,e2),container),i,l)
				| NONE 			    => (case fuzzExpr(e2,n,[j1,container],var_types,environment) of 
				
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(App(e1,fuzzE2),container),i,l)
					| NONE 			    => NONE))
					
			| App(_,_) => raise CounterExpression
					
			| Record(r) => 
			
					(* Takes a list of expression-label pairs, and returns all the top-most level numbers of lists siblings *)
				let fun getSiblingNumbers(l) = (case l of 
						
						  []                         => []
						| (_,CounterExpr(_,j))::rest => j::getSiblingNumbers(rest)
						| _                          => raise CounterExpression)
				
					fun iterERecord(r,extras) = (case r of 
						
						  []      		  				      => NONE
						| (lab1,e1 as CounterExpr(_,j))::rest => (case fuzzExpr(e1,n,container::append(extras,getSiblingNumbers(rest)),var_types,environment) of 
							
							  SOME (fuzzE1,i,errors) => SOME ((lab1,fuzzE1)::rest,i,errors)
							| NONE 			         => (case iterERecord(rest,j::extras) of 
							
								  SOME (fuzzERecord,i,errors) => SOME ((lab1,e1)::fuzzERecord,i,errors)
								| NONE 				          => NONE))
								
						| _ => raise CounterExpression)
					
				in (case iterERecord(r,[]) of 
				
					  SOME (fuzzERecord,i,l) => SOME (CounterExpr(Record(fuzzERecord),container),i,l)
					| NONE				     => NONE)
					
				end
			
			| Let(x,t,e1,e2) => (case fuzzExpr(e1,n,[container],var_types,environment) of 
			
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(Let(x,t,fuzzE1,e2),container),i,l)
				| NONE			    => (case fuzzExpr(e2,n,[container],var_types,x::environment) of 
				
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(Let(x,t,e1,fuzzE2),container),i,l)
					| NONE			    => NONE))
					
			| LetRec(x,t,e1,e2) => (case fuzzExpr(e1,n,[container],var_types,x::environment) of 
			
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(LetRec(x,t,fuzzE1,e2),container),i,l)
				| NONE			    => (case fuzzExpr(e2,n,[container],var_types,x::environment) of 
			
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(LetRec(x,t,e1,fuzzE2),container),i,l)
					| NONE 			    => NONE))
			
			| List(l) =>
			
				let fun getSiblingNumbers(l) = (case l of 
						
						  []                       => []
						| (CounterExpr(_,j))::rest => j::getSiblingNumbers(rest)
						| _                        => raise CounterExpression)
				
					fun iterEList(l,extras) = (case l of 
				
						  []	   => NONE
						| (e1 as CounterExpr(_,j))::rest => (case fuzzExpr(e1,n,container::append(extras,getSiblingNumbers(rest)),var_types,environment) of 
						
							  SOME (fuzzE1,i,errors) => SOME (fuzzE1::rest,i,errors)
							| NONE 			         => (case iterEList(rest,j::extras) of 
							
								  SOME (fuzzEList,i,errors) => SOME (e1::fuzzEList,i,errors)
								| NONE					    => NONE))
								
						| _ => raise CounterExpression)
								
				in (case iterEList(l,[]) of 
				
					  SOME (fuzzEList,i,l) => SOME (CounterExpr(List(fuzzEList),container),i,l)
					| NONE				   => NONE)
					
				end
			
			(* Add container and sibling number *)
			| Cons(e1 as CounterExpr(_,j1),e2 as CounterExpr(_,j2)) => (case fuzzExpr(e1,n,[j2,container],var_types,environment) of 
				
				  SOME (fuzzE1,i,l) => SOME (CounterExpr(Cons(fuzzE1,e2),container),i,l)
				| NONE 			    => (case fuzzExpr(e2,n,[j1,container],var_types,environment) of 
					
					  SOME (fuzzE2,i,l) => SOME (CounterExpr(Cons(e1,fuzzE2),container),i,l) 
					| NONE			    => NONE))
					
			| Cons(_,_) => raise CounterExpression)
		
	| _ => raise CounterExpression)
	
end;
			
(* Takes an expression (assuming that all datatypes are wrapped in counterExpr),
   and returns a list of fuzzed expressions, paired with the expression number changed
   Listed comprised of calling (single) fuzzExpr on each counterExpr number
   List may contain duplicates since calling fuzz on an expression for a given number may actually fuzz
   a different expression number (if fuzzing expression recurses to a sub expression) *)
fun fuzz(expr,var_types) = 

	(* Maximum number of times we can uniquely call fuzz is one for each number of sub-expressions in the whole expression, i.e. one 
	   for each time CounterExpr occurs
	   By the way toCounterExpr works, the numbers associated with the CounterExpr datatypes starts at 1, and ends with its highest in the top-most 
	   CounterExpr datatype wrapper, i.e. 
	   CounterExpr(...rest-of-sub-expressions...,maxNo) *)
	let val maxNoExpr = (case expr of CounterExpr(_,i) => i
									| _ 			   => raise CounterExpression);
		
		fun iterFuzz(n) = (case n of 
		
			  0 => SOME []
			| _ => (case fuzzExpr(expr,n,[],var_types,[]) of
			
					  NONE              => NONE
					| SOME (e,i,errors) => (case iterFuzz(n-1) of 
					  
						  NONE   => NONE
					    | SOME l => SOME ((e,i,errors)::l))))
						
	in iterFuzz(maxNoExpr) end;
	
(* Pretty printer *)
fun prettyPrintFuzzList(e,var_types) = (case fuzz(e,var_types) of 

	  NONE   => "FAIL"
	| SOME l => 
		
		let fun iterPrintList(l) = (case l of 
			
				  []          => ""
				| [(e,_,_)]     => "[" ^ prettyPrintExpression(Expression(e)) ^ "]"
				| (e,_,_)::rest => "[" ^ prettyPrintExpression(Expression(e)) ^ "], " ^ iterPrintList(rest))
				
		in iterPrintList(iterDropCounterExpr(l)) end);