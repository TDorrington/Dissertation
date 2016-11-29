(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which updates a current type based on the latest substitution *)

fun update(Int,_)  = Int
|	update(Real,_) = Real
|  	update(Bool,_) = Bool
| 	update(THole(TypeHole(d)),theta) = resolveChainTheta(THole(TypeHole(d)),theta)
| 	update(Pair(t1,t2),theta) = 
		Pair(update(t1,theta),update(t2,theta));

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function used in unifyAlg
   It takes two type variables, i.e. either TypeVar, EqualityTypeVar or ArithTypeVar
   which must be equal, and returns a list of pairs which are equivalent constraints *)
	
fun getNewConstraints(TypeHole(a),TypeHole(b)) = case (a,b) of

		(TypeVar(_),_) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
					
	| (EqualityTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))] 
	
	| (EqualityTypeVar(_), TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))]
	
	| (EqualityTypeVar(_), ArithTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)] 
										
	| (ArithTypeVar(_),ArithTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))] 
										
	| (ArithTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)]
	
	| (ArithTypeVar(_),TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))];

(* ----------------------------------------------------------------------------------- *)
(* Implements unification algorithm *)

fun unifyAlg([], theta) = SOME theta

| 	unifyAlg(constraint::rest, theta) =  case constraint of

	  (Int,Int)   => unifyAlg(rest, theta)
	| (Real,Real) => unifyAlg(rest, theta)
	| (Bool,Bool) => unifyAlg(rest, theta)
	| (Pair(t1,t2),Pair(t1',t2')) => unifyAlg( (t1,t1')::(t2,t2')::rest, theta)
	
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
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(c),update(THole(TypeHole(d)),newMap))))
			     
			| l => unifyAlg(append(l,rest),theta))
			
		end

	(* For unifying pairs and type holes,
		unify('a,t1*t2)   => unify('a0*'a1,t1*t2)   with mapping 'a->'a0*'a1 for fresh 'a0, 'a1
		unify(''a,t1*t2)  => unify(''a0*''a1,t1*t2) with mapping ''a->''a0*''a1 for fresh ''a0, ''a1
		unify('''a,t1*t2) => FAIL
	*)
		
	| (THole(TypeHole(a)),p as Pair(p1,p2)) =>
	
		(* First check case that cannot occur *)
		(case a of
			  ArithTypeVar(_) => NONE
			  
			| _ => 
			if element(ftv(p),THole(TypeHole(a)))
			then NONE
			else let val freshTypeVar = case a of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										   | TypeVar(_) 			 => TYPE_VAR
										   | ArithTypeVar(_)		 => ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
					val fresh1 = generateFreshTypeVar(freshTypeVar,theta);
					val fresh2 = generateFreshTypeVar(freshTypeVar,theta);
					val newType = Pair(fresh1,fresh2)
				in unifyAlg((newType,p)::rest,Substitution.union(theta,TypeHole(a),newType)) end)
	
	| (p as Pair(p1,p2),THole(TypeHole(a))) => 
	
		(* First check case that cannot occur *)
		(case a of
			  ArithTypeVar(_) => NONE
			  
			| _ => 
			(* side condition: alpha not in ftv(t) *)
			if element(ftv(p),THole(TypeHole(a)))
			then NONE
			else let val freshTypeVar = case a of EqualityTypeVar(_) => EQUALITY_TYPE_VAR
										   | TypeVar(_) 			 => TYPE_VAR
										   | ArithTypeVar(_)		 => ARITH_TYPE_VAR;
										   (* arith should never occur as matches above *)
					val fresh1 = generateFreshTypeVar(freshTypeVar,theta);
					val fresh2 = generateFreshTypeVar(freshTypeVar,theta);
					val newType = Pair(fresh1,fresh2)
				in unifyAlg((p,newType)::rest,Substitution.union(theta,TypeHole(a),newType)) end)
		
	| (THole(TypeHole(a)),t) =>
		(* Assert t one of: Int, Real, Bool - no need for ftv check *)
		
		(* First check cases that cannot occur *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ => (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
				
				  NONE => NONE
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(a),update(t,newMap)))))
					
	| (t,THole(TypeHole(a))) => 
		(* Assert t one of: Int, Real, Bool - no need for ftv check *)
		
		(* First check cases that cannot occur *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ => (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
					
				  NONE => NONE
				| SOME(newMap) => SOME (Substitution.union(newMap,TypeHole(a),update(t,newMap)))))
		
	| _ => NONE (* e.g (Int,Real) *)
	
(* ----------------------------------------------------------------------------------- *)
(* Takes list of types, and current substitution,
   If there is a map a->b (i.e. a chain of maps from a to b  in theta, 
   it replaces all occurrences of a in constraints by b *)	
   
fun normalize ([],_) = []
|   normalize (x::l,theta) = update(x,theta)::normalize(l,theta);
	
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
   end;
