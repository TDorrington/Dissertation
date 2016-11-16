(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which updates a current type based on the latest substitution
   Regardless of the current type substitutions we cannot update int, real, or bool
   so only non-trivial case is type holes: if the latest substitution contains the type
   hole, we can replace it with the (more restricted) type it is mapped to
   Used to 'normalize' a list of constraints before calling the unification algorithm 
   e.g. unify([('a,'b)],['a -> Int])
   will be reduced to unify([(Int,'b),['a -> Int]) *)

fun update(Int,_)  = Int
|	update(Real,_) = Real
|  	update(Bool,_) = Bool
| 	update(THole(TypeHole(d)),theta) = 
		if Substitution.contains(TypeHole(d),theta) 
		then resolveChainTheta(THole(TypeHole(d)),theta)
		else THole(TypeHole(d))
| 	update(Pair(t1,t2),theta) = 
		Pair(update(t1,theta),update(t2,theta));

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function used in unifyAlg
   It takes two type variables, i.e. either TypeVar, EqualityTypeVar or ArithTypeVar
   which must be equal, and returns a list of pairs which are equivalent constraints 
   
   9 possible cases - order corresponds to order of case clauses below
   Idea is that 
   
		- TypeVariable can be unified with any types in TypeVariable:
			'a -> 'b, 	'a -> ''b,	'a -> '''b
				
		- EqualityTypeVar can map to EqualityTypeVar:
			''a -> ''b
					
		- EqualityTypeVar cannot map to TypeVar, but we switch to other way round:
			'b -> ''a
					
		- Intersection of EqualityTypeVar and ArithTypeVar is Int, so 
		  when unifying ''a and '''b we get the two mappings
			''a -> Int, 	'''b -> Int
				  
		- ArithTypeVar can map to ArithTypeVar:
			'''a -> '''b
					
		- ArithTypeVar cannot map to TypeVar, but again we switch as follows:
			'b -> '''a
					
		- Similarly to above, when unifying '''a and ''b we get the mapping:
			'''a -> Int, 	''b -> Int	
	*)
	
fun getNewConstraints(TypeHole(a),TypeHole(b)) = case (a,b) of

		(TypeVar(_),_) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
					
	| (EqualityTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
	
	| (EqualityTypeVar(_), TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))]
	
	| (EqualityTypeVar(_), ArithTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)] 
										
	| (ArithTypeVar(_),ArithTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
										
	| (ArithTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)]
	
	| (ArithTypeVar(_),TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))];

(* ----------------------------------------------------------------------------------- *)
(* Unify algorithm:
   
   T = Int | Bool | Real |
   A = T | alpha | A x A
   where alpha a type var (i.e. either TypeVar, EqualityTypeVar or ArithTypeVar)
   
   unify : ConstraintSet -> Substitution option
   unify(no constraints) = empty set
   unify({A=A}::C) = unify(C)
   unify({alpha = beta}::C) = unify(newConstraints union C)
   unify({alpha = T}::C) = unify( [alpha -> T]C) union [alpha -> T], alpha not in ftv(A)
   unify({T = alpha}::C) = unify( [alpha -> T]C) union [alpha -> T], alpha not in ftv(A)
   unify({A1 x A2 = A1' x A2'}::C) = unify({A1=A1',A2=A2'} union C)
   unify(anything else) = FAIL
*)

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
		in case newConstraints of
		
			  [(THole(TypeHole(c)),THole(TypeHole(d)))]   =>  
			  
				(* The latestMapped variable prevents cases like unify(['a,'b,Int]) 
				   returning [a'->'b, 'b->Int]
				   but instead returns ['a->Int,'b->Int]
				   That is, we check if any constraints further down the line have since 
				   restricted the constraint we are about to add any further before adding
				   An alternative would be to call unifyAlg with an empty substitution list,
				   rather than current theta, and union them after this method returns
				   in a special way, but this is more efficient *)
				(case unifyAlg(replace(rest,THole(TypeHole(c)),THole(TypeHole(d))),theta) of
				
					  NONE => NONE
					| SOME(newMap) => 
						let val latestMapped = update(THole(TypeHole(d)),newMap)
						in SOME (Substitution.union(newMap,TypeHole(c),latestMapped)) end)
				
			| [a,b] => unifyAlg(a::b::rest,theta)
		end

	| (THole(TypeHole(a)),t) =>
		(* Assert t one of: Int, Real, Bool, t1 x t2 *)
		
		(* First check cases that cannot occur 
			EqualityTypeVar -> Real, or ArithTypeVar -> Bool *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ =>  (* side condition: alpha not in ftv(t) *)
					if element(ftv(t),THole(TypeHole(a)))
					then NONE
					else (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
					
						  NONE => NONE
						| SOME(newMap) =>
							 let val latestMapped = update(t,newMap)
							 in SOME (Substitution.union(newMap,TypeHole(a),latestMapped)) end))
					
	| (t,THole(TypeHole(a))) => 
		(* Assert t one of: Int, Real, Bool, t1 x t2 *)
		(* Same as above case *)
		
		(* First check cases that cannot occur 
			EqualityTypeVar -> Real, or ArithTypeVar -> Bool *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => NONE
			| (ArithTypeVar(_),Bool) => NONE
			
			| _ =>  (* side condition: alpha not in ftv(t) *)
					if element(ftv(t),THole(TypeHole(a)))
					then NONE
					else (case unifyAlg(replace(rest,THole(TypeHole(a)),t),theta) of
					
						  NONE => NONE
						| SOME(newMap) =>
							 let val latestMapped = update(t,newMap)
							 in SOME (Substitution.union(newMap,TypeHole(a),latestMapped)) end))
		
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
