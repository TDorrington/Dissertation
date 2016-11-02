(* Set of constraints represented as list of pairs
   Pair of form (a,b) is the constraint that types a and b must be equal *)

(* ----------------------------------------------------------------------------------- *)

(* Auxiliary function to replace all occurrences of a (which will be a type variable)
   with type of b
   E.g. replace( [('a,'b)], 'a, Int ) -> [(Int,'b)] *)
fun replace ([],_,_) = []
|   replace ((x,y)::l,a,b) =
	if a=x then (b,y)::l else 
	if a=y then (x,b)::l else (x,y)::replace(l,a,b);

(* ----------------------------------------------------------------------------------- *)
	
(* Auxiliary function used in unifyWrapper
   It takes two type variables, i.e. either TypeVar, EqualityTypeVar or ArithTypeVar
   which must be equal, and returns a list of pairs which are equivalent constraints
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
	
	  (TypeVar(_),_) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
					
	| (EqualityTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
	
	| (EqualityTypeVar(_), TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))]
			
	| (EqualityTypeVar(_), ArithTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)] 
										
	| (ArithTypeVar(_),ArithTypeVar(_)) => [(THole(TypeHole(a)), THole(TypeHole(b)))]
										
	| (ArithTypeVar(_), EqualityTypeVar(_)) => [(THole(TypeHole(a)), Int),(THole(TypeHole(b)), Int)]
					
	| (ArithTypeVar(_),TypeVar(_)) => [(THole(TypeHole(b)), THole(TypeHole(a)))];

(* ----------------------------------------------------------------------------------- *)

(* Unify algorithm:
   
   T = Int | Bool | Real
   A = T | alpha/beta/gamma/...
   where alpha/... a type var (i.e. either TypeVar, EqualityTypeVar or ArithTypeVar)
   
   unify : ConstraintSet -> Substitution * bool (if successful)
   unify(no constraints) = empty set
   unify({A=A}::C) = unify(C)
   unify({alpha = T}::C) = unify( [alpha -> T]C) union [alpha -> T], alpha not in ftv(A)
   unify({alpha = beta}::C) = unify(newConstraints union C)
   unify({T = alpha}::C) = unify( [alpha -> T]C) union [alpha -> T], alpha not in ftv(A)
   unify(anything else) = FAIL
   CHECK FTV LATER ON
*)

fun unifyWrapper([], theta) = (theta, true)

| 	unifyWrapper(constraint::rest, theta) =  case constraint of

	  (Int,Int)   => unifyWrapper(rest, theta)
	| (Real,Real) => unifyWrapper(rest, theta)
	| (Bool,Bool) => unifyWrapper(rest, theta)
	
	(* Idea is that we replace the constraint 'a = 'b,
	   where 'a and 'b are either TypeVar's, EqualityTypeVar's, or ArithTypeVar's,
	   with new constraint(s) returned from auxiliary function getNewConstraints *) 
	| (THole(TypeHole(a)),THole(TypeHole(b))) => 
	
		if a = b then unifyWrapper(rest,theta) (* last case of unify({A=A}::C) *)
		else
		
		let val newConstraints = getNewConstraints(TypeHole(a),TypeHole(b))
		in case newConstraints of
		
			  [(THole(TypeHole(c)),THole(TypeHole(d)))]   =>  
			  
				(* The latestMapped variable turns cases like unify(['a,'b,Int]) 
				   which would return [a'->'b, 'b->Int]
				   instead into =['a->Int,'b->Int]
				   That is, we check if any constraints further down the line have since 
				   restricted the constraint we are about to add any further before adding
				   An alternative would be to call unifyWrapper with an empty substitution list,
				   rather than current theta, and union them after this method returns
				   in a special way, but this is more efficient *)
				let val (newMap,b) = unifyWrapper(replace(rest,THole(TypeHole(c)),THole(TypeHole(d))),theta)
					val latestMapped = if Substitution.contains(TypeHole(d),newMap) 
									   then Substitution.get(TypeHole(d),newMap)
									   else THole(TypeHole(d))
									   
				in (Substitution.union(newMap,TypeHole(c),latestMapped), b) end
				
			| [a,b] => unifyWrapper(a::b::rest,theta)
		end
		
	(* For two cases below, THole(TypeHole(a)) cannot be in free variables of t
	   This is not a problem at the moment since only primitive types are
	   Int, Bool and Real (no free variables)
	   Will need to check for functions, for example
	   Also first check cases that cannot occur given current types supported 
			EqualityTypeVar -> Real
			ArithTypeVar -> Bool *)
	
	| (THole(TypeHole(a)),t) =>
		(* Assert t a primitive type: Int, Real, Bool *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => (theta,false)
			| (ArithTypeVar(_),Bool) => (theta,false)
			
			| _ =>  let val (newMap,b) = unifyWrapper(replace(rest,THole(TypeHole(a)),t),theta)
					in (Substitution.union(newMap,TypeHole(a),t), b) end)
	
	| (t,THole(TypeHole(a))) => 
		(* Assert t a primitive type: Int, Real, Bool *)
		(case (a,t) of
			  (EqualityTypeVar(_),Real) => (theta,false)
			| (ArithTypeVar(_),Bool) => (theta,false)
			
			| _ =>  let val (newMap,b) = unifyWrapper(replace(rest,THole(TypeHole(a)),t),theta)
					in (Substitution.union(newMap,TypeHole(a),t), b) end)
		
	| _ => (theta,false) (* e.g (Int,Real) *)
	
(* ----------------------------------------------------------------------------------- *)

(* Takes list of types, and current substitution,
   If there is a map a->b in theta, it replaces all occurrences of a in constraints by b *)	
fun normalize ([],_) = []
|   normalize (x::l,theta) = 
	case x of 
	  THole(a) => 
		if Substitution.contains(a,theta) 
		then Substitution.get(a,theta)::normalize(l,theta)
		else x::normalize(l,theta)
	| _ => x::normalize(l,theta);
	
(* ----------------------------------------------------------------------------------- *)
	
(* Wrapper function. Takes a list of types which must ALL be equal,
   generates all the constraints stemming from this list, and calls unify algorithm
   At the mo, unify can only be called with 2 or 3 types in the list from narrow,  
   hence 2 cases *)
fun unify(l,theta) =
	
	let val constraints = normalize(l,theta)
	in case constraints of
		  [a,b]   => unifyWrapper([(a,b)],theta)
		| [a,b,c] => unifyWrapper([(a,b),(a,c),(b,c)],theta)
   end;

(* ----------------------------------------------------------------------------------- *)
(* test cases *)
(* ' is type variables, '' equality type variables, and ''' arithmetic type variables *)
(* 1 appended to end means it is equivalent type, but type hole version *)

val a' = THole(TypeHole(TypeVar("a")));
val a'1 = TypeHole(TypeVar("a"));
val b' = THole(TypeHole(TypeVar("b")));
val b'1 = TypeHole(TypeVar("b"));
val c' = THole(TypeHole(TypeVar("c")));
val d' = THole(TypeHole(TypeVar("d")));

val a'' = THole(TypeHole(EqualityTypeVar("a")));
val b'' = THole(TypeHole(EqualityTypeVar("b")));
val b''1 = TypeHole(EqualityTypeVar("b"));
val c'' = THole(TypeHole(EqualityTypeVar("c")));
val d'' = THole(TypeHole(EqualityTypeVar("d")));

val a''' = THole(TypeHole(ArithTypeVar("a")));
val b''' = THole(TypeHole(ArithTypeVar("b")));
val c''' = THole(TypeHole(ArithTypeVar("c")));
val d''' = THole(TypeHole(ArithTypeVar("d")));

(* test outputs assume true, unless stated otherwise *)
unify( [Int,Int], []);		(* gives mapping [] *)
unify( [a',Int], []);		(* gives mapping [ (a' -> Int) ] *)
unify( [a''',Real], []);	(* gives mapping ------- *)
unify( [a''',Bool], []);	(* gives mapping [] with false *)
unify( [a'',Real], []);		(* gives mapping [] with false *)
unify( [a',b'], []);		(* gives mapping [ (a' -> b') ] *)
unify( [a''',b'''], []);	(* gives mapping [ (a''' -> b''') ] *)
unify( [a'',b'''], []);		(* gives mapping [ (a'' -> Int), (b''' -> Int) ] *)
unify( [a',b',c'], []); 	(* gives mapping [ (a' -> c'), (b' -> c') ] *)
unify( [a',b',Int], []);   	(* gives mapping [ (a' -> Int'), (b' -> Int)] *)
unify( [a',b'',c'''],[]);	(* gives mapping [ (a' -> Int), (b'' -> Int), (c''' -> Int) ] *)
unify( [Int,Int,Int],[]);	(* gives mapping [] *)
unify( [a',Int], [(a'1,Int)]);		(* gives mapping [ (a' -> Int) ] *)
unify( [a',Int], [(a'1,Real)]);  	(* gives mapping with false *)
unify( [a',b',c'], [(a'1,Int)]);	(* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b',c'], [(a'1,Int),(b'1,Real)]); (* gives mapping with false *)
unify( [a',b'',c'''], [(a'1,Int)] );		(* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b'',c'''], [(a'1,Int), (b''1,Int)] );  (* gives mapping [ ('a -> Int), ('b -> Int), ('c -> Int) ] *)
unify( [a',b'',c'''], [(a'1,Real)] ); 		(* gives mapping with false *)


