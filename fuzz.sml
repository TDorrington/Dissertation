(* Thrown if we attempt to fuzz a value hole - this case should never occur in our top-level expressions *)
exception ValueHole;

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
			
(*  Takes a list of labels, generated from an existing record, and returns a new fresh label
    such that the label does not occur in the labels set given *)
	fun getFreshLabel(labels) = 
	
		let val freshLabel = Lab("a" ^ Int.toString(getCounterAndUpdate()))
	
		in if element(labels,freshLabel) 
		   then getFreshLabel(labels)
		   else freshLabel
		end;
		
(*  Takes a list of expressions, [e0,e1,...,en],
    and converts to a record, by appending a unique label to each expression, {a0=e0,...,an=en} *)
	fun listToERecord(l,n) = (case l of 
			
	  []       => []
	| e1::rest => (Lab("a"^Int.toString(n)),e1)::listToERecord(rest,n+1));
		
	fun listtoVRecord(l,n) = (case l of 
	
	  [] 	   => []
	| v1::rest => (Lab("a"^Int.toString(n)),v1)::listtoVRecord(rest,n+1));
		
(*  Takes the sub expression we wish to fuzz, and returns a pair containing
    the fuzzed sub expression, and the number of the expression actually changed
    Cannot assume changed expression number is the one passed in as an argument
    as we may fuzz a different sub expression when we recurse *)
	fun fuzzSingleExpr(e,n) = (case e of 
	
		(* For values, switch to a value of another type *)
		  Value(Concrete(N(_)))      => (Value(Concrete(R(2.0))),n)
		| Value(Concrete(R(_)))      => (Value(Concrete(B(false))),n)
		| Value(Concrete(B(_)))      => (Value(Concrete(N(2))),n)
		| Value(Concrete(EmptyList)) => (Value(Fun(Var("x"),Int,Value(Concrete(R(1.0))))),n)
		| Value(VRecord(_))			 => (Value(VList([Concrete(B(true))])),n)
		| Value(VList(l))			 => (Value(VRecord(listtoVRecord(l,0))),n)
		
		| Value(Fun(x,t,e1)) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Value(Fun(x,fuzzT,e1)),n)
			| NONE 		 => let val (fuzzE1,i) = fuzzSingleExpr(e1,n)
							in (Value(Fun(x,t,fuzzE1)),i) end)
		 
		(* Value holes should not occur in programs - raise exception *)
		| Value(VHole(_)) 	   => raise ValueHole
		
		(* TODO: For now, fuzz to any random value *)
		| Variable(x)		   => (Value(Concrete(N(3))),n)
		
		| ArithExpr(_,e1,e2)   => (Cons(e1,e2),n)
		
		| BoolExpr(_,e1,e2)    => (App(e1,e2),n)
		
		| Case(e1,patExprList) => let val (fuzzE1,i) = fuzzSingleExpr(e1,n)
								  in (Case(fuzzE1,patExprList),i) end
		
		| Condition(e1,e2,e3)  => let val (fuzzE2,i) = fuzzSingleExpr(e2,n)
								  in (Condition(e1,fuzzE2,e3),i) end

		| App(e1,e2)		   => (ArithExpr(PLUS,e1,e2),n)
		
		(* Get a label that is gauranteed not to already be in the record's label set, say l, 
		   and add {l=fn x:bool list => 1} to record *)
		| Record(r) => 
			let val freshLabel = getFreshLabel(Record.getLabels(r));
				val fuzzR = (freshLabel,Value(Fun(Var("x"),TList(Bool),Value(Concrete(N(1))))))::r;
			in (Record(fuzzR),n) end
		
		(* Convert list to a record, keeping the same expressions, and generating unique labels i.e.
		   [e0,e1,...,en] -> {a0=e0,a1=e1,...,an=en} *)
		| List(l) => (Record(listToERecord(l,0)),n) 
		
		| Let(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (Let(x,fuzzT,e1,e2),n)
			| NONE       => let val (fuzzE1,i) = fuzzSingleExpr(e1,n)
						    in (Let(x,t,fuzzE1,e2),i) end)
				
		| LetRec(x,t,e1,e2) => (case fuzzType(t) of 
		
			  SOME fuzzT => (LetRec(x,fuzzT,e1,e2),n)
			| NONE 	     => let val (fuzzE2,i) = fuzzSingleExpr(e2,n)
							in (LetRec(x,t,e1,fuzzE2),i) end)
					
		| Cons(e1,e2) => (BoolExpr(EQ,e1,e2),n)
			
		| CounterExpr(e,i) => let val (fuzzE,j) = fuzzSingleExpr(e,i)
						      in (CounterExpr(fuzzE,i),j) end);
							  
(* Takes a whole expression, and the number of the sub expression we wish to fuzz
   Returns the whole fuzzed expression, paired with the number of the expression changed
   The returned number may be different to the number we wish to fuzz, if for example, 
   we recursed when fuzzing that subexpression
   Assumes all expression datatypes are wrapped in the counterExpr datatype *)
in fun fuzzExpr(e,n) = (case e of 
	
	  CounterExpr(e1,i) => if i = n 
						   then let val (fuzzE1,j) = fuzzSingleExpr(e1,n) in SOME (CounterExpr(fuzzE1,i),j) end
						   else (case fuzzExpr(e1,n) of 
						   
							  SOME (fuzzE1,j) => SOME (CounterExpr(fuzzE1,i),j)
							| NONE			  => NONE)
	
	(* All expressions are assumed to be wrapped in a CounterExpr datatype
	   Thus from here downards we are assuming expression is inside a wrapped CounterExpr datatype,
	   which we dropped on the previous call in the chain,
	   whose counter did not match the number of the expression we are wanting to change *)
	
	| Value(Concrete(_)) => NONE
	
	| Value(Fun(x,t,e1)) => (case fuzzExpr(e1,n) of 
	
		  SOME (fuzzE1,i) => SOME (Value(Fun(x,t,fuzzE1)),i)
		| NONE			  => NONE)
		
	(* Value holes shouldn't occur in programs *)
	| Value(VHole(_)) => raise ValueHole
		
	| Value(VRecord(r)) => 
	
		let fun iterVRecord(r) = (case r of 
			
			  []      		  => NONE
			| (lab1,v1)::rest => (case fuzzExpr(Value(v1),n) of 
				
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
			| v1::rest => (case fuzzExpr(Value(v1),n) of 
			
				  SOME (Value(fuzzV1),i) => SOME (fuzzV1::rest,i)
				| _ 				     => (case iterVList(rest) of 
				
					  SOME (fuzzVList,i) => SOME (v1::fuzzVList,i)
					| NONE				 => NONE)))
					
		in (case iterVList(l) of 
		
			  SOME (fuzzVList,i) => SOME (Value(VList(fuzzVList)),i)
			| NONE				 => NONE)
			
		end
	
	| Variable(_) => NONE
	
	| ArithExpr(oper,e1,e2) => (case fuzzExpr(e1,n) of 
		
		  SOME (fuzzE1,i) => SOME (ArithExpr(oper,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n) of 
		
			  SOME (fuzzE2,i) => SOME (ArithExpr(oper,e1,fuzzE2),i)
			| NONE			  => NONE))
			
	| BoolExpr(oper,e1,e2) => (case fuzzExpr(e1,n) of 	
	
		  SOME (fuzzE1,i) => SOME (BoolExpr(oper,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n) of 
		
			  SOME (fuzzE2,i) => SOME (BoolExpr(oper,e1,fuzzE2),i)
			| NONE			  => NONE))
			
	| Case(e,patExprList) => (case fuzzExpr(e,n) of 
	
		  SOME (fuzzE,i) => SOME (Case(fuzzE,patExprList),i)
		| NONE 			 => 
		
			let fun iterPatExprList(l) = (case l of 
			
				  [] 		      => NONE
				| (pat1,e1)::rest => (case fuzzExpr(e1,n) of 
				
					  SOME (fuzzE1,i) => SOME ((pat1,fuzzE1)::rest,i)
					| NONE 			  => (case iterPatExprList(rest) of 
					
						  SOME (fuzzPatExprList,i) => SOME ((pat1,e1)::fuzzPatExprList,i)
						| NONE 					   => NONE)))
					
			in (case iterPatExprList(patExprList) of 
			
				  SOME (fuzzPatExprList,i) => SOME (Case(e,fuzzPatExprList),i)
				| NONE 					   => NONE)
				
			end)
	
	| Condition(e1,e2,e3) => (case fuzzExpr(e1,n) of 
	
		  SOME (fuzzE1,i) => SOME (Condition(fuzzE1,e2,e3),i)
		| NONE 			  => (case fuzzExpr(e2,n) of 
		
			  SOME (fuzzE2,i) => SOME (Condition(e1,fuzzE2,e3),i)
			| NONE 			  => (case fuzzExpr(e3,n) of 
			
				  SOME (fuzzE3,i) => SOME (Condition(e1,e2,fuzzE3),i)
				| NONE 			  => NONE)))
				
	| App(e1,e2) => (case fuzzExpr(e1,n) of 
	
		  SOME (fuzzE1,i) => SOME (App(fuzzE1,e2),i)
		| NONE 			  => (case fuzzExpr(e2,n) of 
		
			  SOME (fuzzE2,i) => SOME (App(e1,fuzzE2),i)
			| NONE 			  => NONE))
			
	| Record(r) => 
	
		let fun iterERecord(r) = (case r of 
			
			  []      		  => NONE
			| (lab1,e1)::rest => (case fuzzExpr(e1,n) of 
				
				  SOME (fuzzE1,i) => SOME ((lab1,fuzzE1)::rest,i)
				| NONE 			  => (case iterERecord(rest) of 
				
					  SOME (fuzzERecord,i) => SOME ((lab1,e1)::fuzzERecord,i)
					| NONE 				   => NONE)))
					
		in (case iterERecord(r) of 
		
			  SOME (fuzzERecord,i) => SOME (Record(fuzzERecord),i)
			| NONE				   => NONE)
			
		end
	
	| Let(x,t,e1,e2) => (case fuzzExpr(e1,n) of 
	
		  SOME (fuzzE1,i) => SOME (Let(x,t,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n) of 
		
			  SOME (fuzzE2,i) => SOME (Let(x,t,e1,fuzzE2),i)
			| NONE			  => NONE))
				
	| LetRec(x,t,e1,e2) => (case fuzzExpr(e1,n) of 
	
		  SOME (fuzzE1,i) => SOME (LetRec(x,t,fuzzE1,e2),i)
		| NONE			  => (case fuzzExpr(e2,n) of 
	
			  SOME (fuzzE2,i) => SOME (LetRec(x,t,e1,fuzzE2),i)
			| NONE 			  => NONE))
			
	| List(l) =>
	
		let fun iterEList(l) = (case l of 
		
			  []	   => NONE
			| e1::rest => (case fuzzExpr(e1,n) of 
			
				  SOME (fuzzE1,i) => SOME (fuzzE1::rest,i)
				| NONE 			  => (case iterEList(rest) of 
				
					  SOME (fuzzEList,i) => SOME (e1::fuzzEList,i)
					| NONE				 => NONE)))
					
		in (case iterEList(l) of 
		
			  SOME (fuzzEList,i) => SOME (List(fuzzEList),i)
			| NONE				 => NONE)
			
		end
		
	| Cons(e1,e2) => (case fuzzExpr(e1,n) of 
		
		  SOME (fuzzE1,i) => SOME (Cons(fuzzE1,e2),i)
		| NONE 			  => (case fuzzExpr(e2,n) of 
			
			  SOME (fuzzE2,i) => SOME (Cons(e1,fuzzE2),i) 
			| NONE			  => NONE)))
	
end;
			

(* Takes an expression (assuming that all datatypes are wrapped in counterExpr),
   and returns a list of fuzzed expressions, paired with the expression number changed
   Listed comprised of calling (single) fuzzExpr on each counterExpr number *)
(*fun fuzz(expr) = 

	let fun getNumOfExpr =
	
		fun iterFuzz(e) = (case e of 
		
			  
*)
(* 
Generate lists of possible fuzzed expressions:
	- recurse on subexpressions
	- arithexpr: switch *,-,+ <=> /, OR switch to bool expression, for arbitrary bool operator
	- case: change patterns
	- app: put in function of different arg/return type
	- cons: change types of e1 e2
*)