(* Set of constraints represented as list of pairs
   Pair of form (a,b) is the constraint that types a and b must be equal *)

(* Auxiliary function to replace all occurrences of a (which will be a type variable)
   with type of b
   E.g. replace( [('a,'b)], 'a, Int ) -> [(Int,'b)] *)
fun replace ([],_,_) = []
|   replace ((x,y)::l,a,b) =
	if a=x then (b,y)::l else (x,y)::replace(l,a,b);
	
(* Auxiliary function used in unify
   It takes two variables of type typeVar, i.e. either TypeVar, EqualityTypeVar or ArithTypeVar
   and returns a list of pairs which are new constraints
 *)
fun getNewConstraints(TypeHole(a),TypeHole(b)) = case (a,b) of

	(* 9 possible cases - order corresponds to order of case clauses below
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
	
	  (TypeVar(_),_) => [(TypeHole(a), THole(TypeHole(b)))]
					
	| (EqualityTypeVar(_), EqualityTypeVar(_)) => [(TypeHole(a), THole(TypeHole(b)))]
	
	| (EqualityTypeVar(_), TypeVar(_)) => [(TypeHole(b), THole(TypeHole(a)))]
			
	| (EqualityTypeVar(_), ArithTypeVar(_)) => [(TypeHole(a), Int),(TypeHole(b), Int)] 
										
	| (ArithTypeVar(_),ArithTypeVar(_)) => [(TypeHole(a), THole(TypeHole(b)))]
										
	| (ArithTypeVar(_), EqualityTypeVar(_)) => [(TypeHole(a), Int),(TypeHole(b), Int)]
					
	| (ArithTypeVar(_),TypeVar(_)) => [(TypeHole(b), THole(TypeHole(a)))];


(* Unify algorithm:
   
   T = Int | Bool | Real
   A = T | alpha/beta/gamma/...
   where alpha/... a type var (i.e. either TypeVar, EqualityTypeVar or ArithTypeVar)
   
   unify(no constraints) = empty set
   unify({A=A}::C) = unify(C)
   unify({alpha = T}::C) = unify( [alpha -> T]C) union [alpha -> T]
   unify({alpha = beta}::C) = unify(newConstraints union C)
   unify({T = alpha}::C) = unify( [alpha -> T]C) union [alpha -> T]
   unify(anything else) = FAIL
*)
fun unifyWrapper([], theta) = (theta, true)

| 	unifyWrapper(constraint::rest, theta) =  case constraint of

	  (Int,Int)   => unifyWrapper(rest)
	| (Real,Real) => unifyWrapper(rest)
	| (Bool,Bool) => unifyWrapper(rest)
	| (THole(TypeHole(a)),THole(TypeHole(a))) => unifyWrapper(rest)
	
	(* Idea is that we replace the constraint 'a = 'b,
	   where 'a and 'b are either TypeVar's, EqualityTypeVar's, or ArithTypeVar's,
	   with new constraint(s) returned from auxiliary function getNewConstraints *) 
	| (THole(TypeHole(a)),THole(TypeHole(b))) => 
		let val newConstraints = getNewConstraints(TypeHole(a),TypeHole(b))
		in case newConstraints of
			[a]   => unifyWrapper(a::rest)
			[a,b] => unifyWrapper(a::b::rest)
		end
		
	(* For two cases below, THole(TypeHole(a)) cannot be in free variables of t
	   This is not a problem at the moment since only primitive types are
	   Int, Bool and Real (no free variables) - check for functions, for example *)
	
	| ((THole(TypeHole(a)),t) =>
		(* Assert t a primitive type: Int, Real, Bool *)
		let val newMap = unifyWrapper(replace(rest,THole(TypeHole(a)),t),theta)
		in Substitution.union(newMap,THole(TypeHole(a)),t) end
	
	| (t,THole(TypeHole(a))) => 
		(* Asset t a primitive type: Int, Real, Bool *)
		let val newMap = unifyWrapper(replace(rest,THole(TypeHole(a)),t),theta)
		in Substitution.union(newMap,THole(TypeHole(a)),t) end
		
	| _ => (theta, false) (* e.g (Int,Real) *)
	
(* Wrapper function. Takes a list of types which must ALL be equal
   generates all the constraints stemming from this list, and calls unify algorithm
   As of now, unify can only be called with 2 or 3 types in the list, hence 2 cases *)
fun unify ([a,b],theta)   = unifyWrapper([(a,b)],theta)
|	unify ([a,b,c],theta) = unifyWrapper([(a,b),(a,c),(b,c)],theta);