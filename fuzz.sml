(* Thrown if we attempt to fuzz a value hole - this case should never occur in our top-level expressions *)
exception ValueHoleExn;

local 

(*  Returns a type different to that of the argument, if it is possible
    We cannot fuzz a type hole, since it could be any type, and hence are unable 
    to generate a different type: we cannot check if it is different till run time *)
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
	
(*  Takes the sub expression we wish to fuzz, and returns a pair containing
    the fuzzed sub expression, and the number of the expression actually changed
	Actually returns a list of numbers, which represents the expressions that might now
	get stuck due to change. The actual changed expression will of course be in this list.
	At a minimum the list contains the actual changed expression, and the expression number
	of the containing expression 
	(but may contain more in case of, for example, recurse fuzzing on sub expressions)
	Also takes an evironment, which contains the variables currently in scope,
	and a list of variables->types (hand-generated) *)
	fun fuzzSingleExpr(e,n,var_types,environment) = (case e of 
	
		(* For values, switch to a value of another type *)
		  Value(Concrete(N(_)))      => (Value(Concrete(R(2.0))),[n])
		| Value(Concrete(R(_)))      => (Value(Concrete(B(false))),[n])
		| Value(Concrete(B(_)))      => (Value(Concrete(N(2))),[n])
		| Value(Concrete(EmptyList)) => (Value(VRecord([])),[n])
		
		(* Convert list to a record, keeping the same expressions, and generating unique lables i.e.
		   [v0,v1,...,vn] -> {a0=v0,...,a1=vn} *)
		| Value(VList(l)) => 
		
			let fun listtoVRecord(l,i) = (case l of 
				
				  []	   => []
				| v1::rest => (Lab("a"^Int.toString(i)),v1)::listtoVRecord(rest,i+1))
		
			in (Value(VRecord(listtoVRecord(l,0))),[n]) end
		
		(* If we can, remove a (label,value) pair *)
		| Value(VRecord(entry::rest)) => (Value(VRecord(rest)),[n])
		
		(* If no (label,value) pairs in record, convert to an empty list *)
		| Value(VRecord([])) => (Value(Concrete(EmptyList)),[n])
		
		| Value(Fun(x,t,e1)) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Value(Fun(x,fuzzT,e1)),[n])
			| NONE 		 => let val (fuzzE1,i) = fuzzSingleExpr(e1,n,var_types,x::environment)
							in (Value(Fun(x,t,fuzzE1)),n::i) end)
		 
		(* Value holes should not occur in programs - raise exception *)
		| Value(VHole(_)) => raise ValueHoleExn
		
		(* Change to a different variable of another type which is in the same scope as this one
		   If no such variable, fuzz to a value of a different type than the variable *)
		| Variable(x) => (case getDifferentVariable(x,getVarType(x,var_types),var_types,environment) of 
		
			  SOME y => (Variable(y),[n])
		
			| NONE   => (case fuzzType(getVarType(x,var_types)) of 
			  
				  SOME fuzzT => (Value(gen(fuzzT,[])),[n])
				  
				(* Shouldnt really happen - hand-generated variables->types list
				   should have enough information rather than just a plain type variable *)
				| NONE       => raise FreeVariable))
				
		| ArithExpr(_,e1,e2) => (Cons(e1,e2),[n])
		
		| BoolExpr(_,e1,e2)  => (App(e1,e2),[n])
		
		| Case(e1,patExprList) => let val (fuzzE1,i) = fuzzSingleExpr(e1,n,var_types,environment)
								  in (Case(fuzzE1,patExprList),n::i) end
		
		| Condition(e1,e2,e3)  => let val (fuzzE2,i) = fuzzSingleExpr(e2,n,var_types,environment)
								  in (Condition(e1,fuzzE2,e3),n::i) end

		| App(e1,e2) => (ArithExpr(PLUS,e1,e2),[n])
		
		(* If we can, remove a (label,expression) pair *)
		| Record(entry::rest)  => (Record(rest),[n])
		
		(* If no (label,expression) pairs in recor, convert to an empty list *)
		| Record([])		   => (Value(Concrete(EmptyList)),[n])
		
		(* Convert list to a record, keeping the same expressions, and generating unique labels i.e.
		   [e0,e1,...,en] -> {a0=e0,a1=e1,...,an=en} *)
		| List(l) => 
		
			let fun listToERecord(l,i) = (case l of 
			
				  []       => []
				| e1::rest => (Lab("a"^Int.toString(i)),e1)::listToERecord(rest,i+1))
	
		in (Record(listToERecord(l,0)),[n]) end
		
		| Let(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Let(x,fuzzT,e1,e2),[n])
			| NONE       => let val (fuzzE1,i) = fuzzSingleExpr(e1,n,var_types,environment)
						    in (Let(x,t,fuzzE1,e2),n::i) end)
				
		| LetRec(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (LetRec(x,fuzzT,e1,e2),[n])
			| NONE 	     => let val (fuzzE2,i) = fuzzSingleExpr(e2,n,var_types,x::environment)
							in (LetRec(x,t,e1,fuzzE2),n::i) end)
					
		| Cons(e1,e2) => (BoolExpr(EQ,e1,e2),[n])
			
		| CounterExpr(e,i) => let val (fuzzE,j) = fuzzSingleExpr(e,i,var_types,environment)
						      in (CounterExpr(fuzzE,i),j) end);
							  
(* Takes the whole top-level expression, and the number of the sub expression we wish to fuzz
   Returns the whole fuzzed expression, paired with the number of the expression changed
   The returned number may be different to the number we wish to fuzz, if for example, 
   we recursed when fuzzing that subexpression
   Assumes all expression datatypes are wrapped in the counterExpr datatype
   Takes a hand-generated list of variables->types,
   and the environment representing what variables are currently in scope (initially should
   be called from top level as []) *)
in fun fuzzExpr(e,n,var_types,environment) = (case e of 
	
	  CounterExpr(e1,i) => if i = n 
						   then let val (fuzzE1,j) = fuzzSingleExpr(e1,n,var_types,environment) in SOME (CounterExpr(fuzzE1,i),j) end
						   else (case fuzzExpr(e1,n,var_types,environment) of 
						   
							  SOME (fuzzE1,j) => SOME (CounterExpr(fuzzE1,i),j)
							| NONE			  => NONE)
	
	(* All expressions are assumed to be wrapped in a CounterExpr datatype
	   Thus from here downards we are assuming expression is inside a wrapped CounterExpr datatype,
	   which we dropped on the previous call in the chain,
	   whose counter did not match the number of the expression we are wanting to change *)
	
	| Value(Concrete(_)) => NONE
	
	| Value(Fun(x,t,e1)) => (case fuzzExpr(e1,n,var_types,x::environment) of 
	
		  SOME (fuzzE1,i) => SOME (Value(Fun(x,t,fuzzE1)),i)
		| NONE			  => NONE)
		
	(* Value holes shouldn't occur in programs *)
	| Value(VHole(_)) => raise ValueHoleExn
		
	| Value(VRecord(r)) => 
	
		let fun iterVRecord(r) = (case r of 
			
			  []      		  => NONE
			| (lab1,v1)::rest => (case fuzzExpr(Value(v1),n,var_types,environment) of 
				
				  SOME (Value(fuzzV1),i) => SOME ((lab1,fuzzV1)::rest,i)
				| _ 			  		 => (case iterVRecord(rest) of 
				
					  SOME (fuzzVRecord,i) => SOME ((lab1,v1)::fuzzVRecord,i)
					| NONE 				   => NONE)))
					
		in (case iterVRecord(r) of 
		
			  SOME (fuzzVRecord,i) => SOME (Value(VRecord(fuzzVRecord)),i)
			| NONE				   => NONE)
			
		end
		
	| Value(VList(l)) =>
	
		let fun iterVList(l) = (case l of 
		
			  []	   => NONE
			| v1::rest => (case fuzzExpr(Value(v1),n,var_types,environment) of 
			
				  SOME (Value(fuzzV1),i) => SOME (fuzzV1::rest,i)
				| _ 				     => (case iterVList(rest) of 
				
					  SOME (fuzzVList,i) => SOME (v1::fuzzVList,i)
					| NONE				 => NONE)))
					
		in (case iterVList(l) of 
		
			  SOME (fuzzVList,i) => SOME (Value(VList(fuzzVList)),i)
			| NONE				 => NONE)
			
		end
	
	| Variable(_) => NONE
	
	| ArithExpr(oper,e1,e2) => (case fuzzExpr(e1,n,var_types,environment) of 
		
		  SOME (fuzzE1,i) => SOME (ArithExpr(oper,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n,var_types,environment) of 
		
			  SOME (fuzzE2,i) => SOME (ArithExpr(oper,e1,fuzzE2),i)
			| NONE			  => NONE))
			
	| BoolExpr(oper,e1,e2) => (case fuzzExpr(e1,n,var_types,environment) of 	
	
		  SOME (fuzzE1,i) => SOME (BoolExpr(oper,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n,var_types,environment) of 
		
			  SOME (fuzzE2,i) => SOME (BoolExpr(oper,e1,fuzzE2),i)
			| NONE			  => NONE))
			
	| Case(e,patExprList) => (case fuzzExpr(e,n,var_types,environment) of 
	
		  SOME (fuzzE,i) => SOME (Case(fuzzE,patExprList),i)
		| NONE 			 => 
		
			let fun iterPatExprList(l) = (case l of 
			
				  [] 		         => NONE
				| (pat1,e1)::rest    => (case fuzzExpr(e1,n,var_types,append(fvPat(pat1),environment)) of 
				
					  SOME (fuzzE1,i) => SOME ((pat1,fuzzE1)::rest,i)
					| NONE 			  => (case iterPatExprList(rest) of 
					
						  SOME (fuzzPatExprList,i) => SOME ((pat1,e1)::fuzzPatExprList,i)
						| NONE 					   => NONE)))
					
			in (case iterPatExprList(patExprList) of 
			
				  SOME (fuzzPatExprList,i) => SOME (Case(e,fuzzPatExprList),i)
				| NONE 					   => NONE)
				
			end)
	
	| Condition(e1,e2,e3) => (case fuzzExpr(e1,n,var_types,environment) of 
	
		  SOME (fuzzE1,i) => SOME (Condition(fuzzE1,e2,e3),i)
		| NONE 			  => (case fuzzExpr(e2,n,var_types,environment) of 
		
			  SOME (fuzzE2,i) => SOME (Condition(e1,fuzzE2,e3),i)
			| NONE 			  => (case fuzzExpr(e3,n,var_types,environment) of 
			
				  SOME (fuzzE3,i) => SOME (Condition(e1,e2,fuzzE3),i)
				| NONE 			  => NONE)))
				
	| App(e1,e2) => (case fuzzExpr(e1,n,var_types,environment) of 
	
		  SOME (fuzzE1,i) => SOME (App(fuzzE1,e2),i)
		| NONE 			  => (case fuzzExpr(e2,n,var_types,environment) of 
		
			  SOME (fuzzE2,i) => SOME (App(e1,fuzzE2),i)
			| NONE 			  => NONE))
			
	| Record(r) => 
	
		let fun iterERecord(r) = (case r of 
			
			  []      		  => NONE
			| (lab1,e1)::rest => (case fuzzExpr(e1,n,var_types,environment) of 
				
				  SOME (fuzzE1,i) => SOME ((lab1,fuzzE1)::rest,i)
				| NONE 			  => (case iterERecord(rest) of 
				
					  SOME (fuzzERecord,i) => SOME ((lab1,e1)::fuzzERecord,i)
					| NONE 				   => NONE)))
					
		in (case iterERecord(r) of 
		
			  SOME (fuzzERecord,i) => SOME (Record(fuzzERecord),i)
			| NONE				   => NONE)
			
		end
	
	| Let(x,t,e1,e2) => (case fuzzExpr(e1,n,var_types,environment) of 
	
		  SOME (fuzzE1,i) => SOME (Let(x,t,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n,var_types,x::environment) of 
		
			  SOME (fuzzE2,i) => SOME (Let(x,t,e1,fuzzE2),i)
			| NONE			  => NONE))
				
	| LetRec(x,t,e1,e2) => (case fuzzExpr(e1,n,var_types,x::environment) of 
	
		  SOME (fuzzE1,i) => SOME (LetRec(x,t,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n,var_types,x::environment) of 
	
			  SOME (fuzzE2,i) => SOME (LetRec(x,t,e1,fuzzE2),i)
			| NONE 			  => NONE))
			
	| List(l) =>
	
		let fun iterEList(l) = (case l of 
		
			  []	   => NONE
			| e1::rest => (case fuzzExpr(e1,n,var_types,environment) of 
			
				  SOME (fuzzE1,i) => SOME (fuzzE1::rest,i)
				| NONE 			  => (case iterEList(rest) of 
				
					  SOME (fuzzEList,i) => SOME (e1::fuzzEList,i)
					| NONE				 => NONE)))
					
		in (case iterEList(l) of 
		
			  SOME (fuzzEList,i) => SOME (List(fuzzEList),i)
			| NONE				 => NONE)
			
		end
		
	| Cons(e1,e2) => (case fuzzExpr(e1,n,var_types,environment) of 
		
		  SOME (fuzzE1,i) => SOME (Cons(fuzzE1,e2),i)
		| NONE 			  => (case fuzzExpr(e2,n,var_types,environment) of 
			
			  SOME (fuzzE2,i) => SOME (Cons(e1,fuzzE2),i) 
			| NONE			  => NONE)))
	
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
	let val maxNoExpr = (case expr of 
	
			  CounterExpr(_,i) => SOME i
			| _ 			   => NONE);
		
		fun iterFuzz(n) = (case n of 
		
			  0 => SOME []
			| _ => (case fuzzExpr(expr,n,var_types,[]) of
			
					  NONE       => NONE
					| SOME (e,i) => (case iterFuzz(n-1) of 
					  
						  NONE   => NONE
					    | SOME l => SOME ((e,i)::l))))
						
	in (case maxNoExpr of 
	
		  SOME i => iterFuzz(i)
		| NONE   => NONE)
		
	end; 
	
(* Pretty printer *)
fun prettyPrintFuzzList(e,var_types) = (case fuzz(e,var_types) of 

	  NONE   => "FAIL"
	| SOME l => 
		
		let fun iterPrintList(l) = (case l of 
			
				  []          => ""
				| [(e,_)]     => "[" ^ prettyPrintExpression(Expression(e)) ^ "]"
				| (e,_)::rest => "[" ^ prettyPrintExpression(Expression(e)) ^ "], " ^ iterPrintList(rest))
				
		in iterPrintList(iterDropCounterExpr(l)) end);