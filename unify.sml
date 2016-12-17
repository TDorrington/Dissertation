	(* Takes two type variables, i.e. either TypeVar, EqualityTypeVar or ArithTypeVar
       which must be equal, and returns a list of pairs which are equivalent constraints *)
local fun getNewConstraints(TypeHole(a),TypeHole(b)) = (case (a,b) of

			(TypeVar(_),_) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
						
		| (EqualityTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))] 
		
		| (EqualityTypeVar(_), TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))]
		
		| (EqualityTypeVar(_), ArithTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)] 
											
		| (ArithTypeVar(_),ArithTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))] 
											
		| (ArithTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)]
		
		| (ArithTypeVar(_),TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))])
			
in fun unifyAlg([], theta) = SOME theta

| 	unifyAlg(constraint::rest, theta) =  case constraint of

	  (Int,Int)   => unifyAlg(rest, theta)
	| (Real,Real) => unifyAlg(rest, theta)
	| (Bool,Bool) => unifyAlg(rest, theta)
	| (TFun(t1,t2),TFun(t1',t2')) => unifyAlg( (t1,t1')::(t2,t2')::rest, theta)
	
	| (TRecord(r1),TRecord(r2)) => (case Record.merge(r1,r2) of 
			
   		  SOME l => unifyAlg(append(l,rest),theta)
		| NONE   => NONE)
	
	(* Idea is that we replace the constraint 'a = 'b,
	   where 'a and 'b are either TypeVar's, EqualityTypeVar's, or ArithTypeVar's,
	   with new constraint(s) returned from auxiliary function getNewConstraints *) 
	| (THole(TypeHole(a)),THole(TypeHole(b))) => 
	
		if a = b then unifyAlg(rest,theta) (* last case of unify({A=A}::C) *)
		
		else
		let val newConstraints = getNewConstraints(TypeHole(a),TypeHole(b))
		in (case newConstraints of
				
			  [(THole(TypeHole(c)),THole(TypeHole(d)))]   => (case unifyAlg(rest,theta) of 
			  
				  NONE => NONE
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(c),resolveChainTheta(THole(TypeHole(d)),newMap))))
			     
			| l => unifyAlg(append(l,rest),theta))
			
		end
		
	(* For unifying functions and type holes
		unify('a,t1->t2)   => unify('a0->'a1,t1->t2) with mapping 'a->('a0->'a1) for fresh 'a0,'a1
		unify(''a,t1->t2)  => FAIL
		unify('''a,t1->t2) => FAIL
	*)
		
	| (THole(TypeHole(a)),f as TFun(t1,t2)) =>
	
		(* First check cases that cannot occur *)
		(case a of 
			  ArithTypeVar(_) => NONE
			| EqualityTypeVar(_) => NONE
			
			| TypeVar(_) =>
				if element(ftv(f),THole(TypeHole(a)))
				then NONE
				else let val fresh1 = generateFreshTypeVar(TYPE_VAR,theta);
						 val fresh2 = generateFreshTypeVar(TYPE_VAR,theta); 
						 val newType = TFun(fresh1,fresh2);
						 val newRest = replace(rest,THole(TypeHole(a)),newType)
					in unifyAlg((newType,f)::newRest,Substitution.union(theta,TypeHole(a),newType)) end)
	
	| (f as TFun(t1,t2),THole(TypeHole(a))) =>
		
		(* First check cases that cannot occur *)
		(case a of 
			  ArithTypeVar(_) => NONE
			| EqualityTypeVar(_) => NONE
			
			| TypeVar(_) =>
				if element(ftv(f),THole(TypeHole(a)))
				then NONE
				else let val fresh1 = generateFreshTypeVar(TYPE_VAR,theta);
						 val fresh2 = generateFreshTypeVar(TYPE_VAR,theta); 
						 val newType = TFun(fresh1,fresh2);
						 val newRest = replace(rest,THole(TypeHole(a)),newType)
					in unifyAlg((f,newType)::newRest,Substitution.union(theta,TypeHole(a),newType)) end)
		
	(* For unifying records and type holes,
	   unify({lab1=t1,...,labn=tn},'a)   => unify({lab1='a1,...,labn='an},{lab1=t1,...,labn=tn})
	   unify({lab1=t1,...,labn=tn},''a)  => unify({lab1=t1,...,labn=tn},{lab1=''a1,...,labn=''an})
	   unify({lab1=t1,...,labn=tn},'''a) => FAIL
	   for fresh 'a1,''a1,...,'an,''an *)

	| (THole(TypeHole(a)),record as TRecord(r)) =>
	
		(* First check case that cannot occur *)
		(case a of
			  ArithTypeVar(_) => NONE
			  
			| _ => 
				if element(ftv(record),THole(TypeHole(a)))
				then NONE
				else let val freshTypeVar = case a of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											   | TypeVar(_) 			 => TYPE_VAR
											   | ArithTypeVar(_)		 => ARITH_TYPE_VAR;
											   (* arith should never occur as matches above *)
						val newType = genFreshTRecord(Record.getLabels(r),freshTypeVar,theta);
						val newRest = replace(rest,THole(TypeHole(a)),newType)
					 in unifyAlg((newType,record)::newRest,Substitution.union(theta,TypeHole(a),newType)) end)
	
	| (record as TRecord(r),THole(TypeHole(a))) => 
	
		(* First check case that cannot occur *)
		(case a of
			  ArithTypeVar(_) => NONE
			  
			| _ => 
				if element(ftv(record),THole(TypeHole(a)))
				then NONE
				else let val freshTypeVar = case a of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
											   | TypeVar(_) 			 => TYPE_VAR
											   | ArithTypeVar(_)		 => ARITH_TYPE_VAR;
											   (* arith should never occur as matches above *)
						val newType = genFreshTRecord(Record.getLabels(r),freshTypeVar,theta);
						val newRest = replace(rest,THole(TypeHole(a)),newType)
					 in unifyAlg((record,newType)::newRest,Substitution.union(theta,TypeHole(a),newType)) end)
	
		
	| (THole(TypeHole(a)),t) =>
		(* Assert t one of: Int, Real, Bool - no need for ftv check *)
		
		(* First check cases that cannot occur *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ => (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
				
				  NONE => NONE
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(a),resolveChainTheta(t,newMap)))))
					
	| (t,THole(TypeHole(a))) => 
		(* Assert t one of: Int, Real, Bool - no need for ftv check *)
		
		(* First check cases that cannot occur *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ => (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
					
				  NONE => NONE
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(a),resolveChainTheta(t,newMap)))))
		
	| _ => NONE (* e.g (Int,Real) *)
	
end;
	
(* ----------------------------------------------------------------------------------- *)
(* Takes list of types, and current substitution,
   If there is a map a->b (i.e. a chain of maps from a to b  in theta, 
   it replaces all occurrences of a in constraints by b *)	
   
fun normalize ([],_) = []
|   normalize (x::l,theta) = resolveChainTheta(x,theta)::normalize(l,theta);
	
(* ----------------------------------------------------------------------------------- *)
(* Wrapper function. Takes a list of types which must ALL be equal,
   generates all the constraints stemming from this list, and calls unify algorithm
   At the moment, unify can only be called with 2 or 3 types in the list from narrow,  
   hence 2 cases *)
   
fun unify(l,theta) =
	
	let val constraints = normalize(l,theta)
	in case constraints of
		  [a,b]   => unifyAlg([(a,b)],theta)
		| [a,b,c] => unifyAlg([(a,b),(a,c),(b,c)],theta)
		| _ 	  => NONE
   end;
