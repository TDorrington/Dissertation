fun unify(THole(TypeHole(a))::types, theta:typeSub) = case types of

	[t] =>  (case t of THole(TypeHole(b)) => 
			 (* Idea is that 
				- TypeVariable can be unified with any types in TypeVariable, 
				  EqualityTypeVar and ArithTypeVar
				- Unifying EqualityTypeVar and ArithTypeVar can only be int
			    so we need to preserve this order in mapping *)
				(case (a,b) of
					
					  (TypeVar(_),_) => (Substitution.union(theta, TypeHole(a), THole(TypeHole(b))),true)
					
					| (EqualityTypeVar(_), EqualityTypeVar(_)) =>
										(Substitution.union(theta, TypeHole(a), THole(TypeHole(b))),true)
				
					| (EqualityTypeVar(_), ArithTypeVar(_)) =>
										let val theta1 = Substitution.union(theta, TypeHole(a), Int)
										in (Substitution.union(theta1, TypeHole(b), Int), true) end
					 
					| (EqualityTypeVar(_), TypeVar(_)) => 
										(Substitution.union(theta, TypeHole(b), THole(TypeHole(a))),true)
										
					| (ArithTypeVar(_),ArithTypeVar(_)) =>
										(Substitution.union(theta, TypeHole(a), THole(TypeHole(b))),true)
										
					| (ArithTypeVar(_), EqualityTypeVar(_)) =>
										let val theta1 = Substitution.union(theta, TypeHole(a), Int)
										in (Substitution.union(theta1, TypeHole(b), Int), true) end
					
					| (ArithTypeVar(_),TypeVar(_)) =>
										(Substitution.union(theta, TypeHole(b), THole(TypeHole(a))),true))
				
		
			| _ => (Substitution.union(theta, TypeHole(a), t), true))
			
	| [t1,t2] => (* TODO: currently assume t1 = t2 since only case *)
				 (Substitution.union(theta, TypeHole(a), t1), true)
		