fun unify(THole(TypeHole(a))::types, theta:typeSub) = case types of

			  [t]     =>  (case t of 
								THole(TypeHole(b)) => (theta, true) (* TODO *)
								| _ => (Substitution.union(theta, TypeHole(a), t), true))
			
			| [t1,t2] => (* TODO: currently assume t1 = t2 *)
						 (Substitution.union(theta, TypeHole(a), t1), true)
						 
			| _ => (theta, true) (* Just to shutup warnings *)