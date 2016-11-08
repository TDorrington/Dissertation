fun resolveChainTheta(a,theta) = case a of

	  THole(hole) =>
		
		if(Substitution.contains(hole,theta)) 
		then resolveChainTheta(Substitution.get(hole,theta),theta)
		else a
	  
	| Pair(t1,t2) => Pair(resolveChainTheta(t1,theta),resolveChainTheta(t2,theta))
	
	| _ => a; (* Bottom type of Int, Real or Bool *)

(* 'narrow' dynamically performs type-checking 
    narrow : v * t * valSub * typeSub -> <v union stuck, valSub, typeSub>
    takes a value v, type t and current values & type substitutions and
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta) = 

case (v,t) of

	  (N(integer),Int)  => Config(Expression(Value(v)), sigma, theta)
	 
	| (B(boolean),Bool) => Config(Expression(Value(v)), sigma, theta)

	| (R(real),Real)    => Config(Expression(Value(v)), sigma, theta)
	
	| (ValuePair(v1,v2),Pair(t1,t2)) =>
	
		let val (theta1, success) = unify( [typeof(v1),t1], theta) 
		in
			if(success)
			
			then let val (theta2, success2) = unify( [typeof(v2),t2], theta1)
			
				 in
					if(success2)
					then Config(Expression(Value(ValuePair(v1,v2))),sigma,theta2)
					else Config(Stuck,sigma,theta2)
				 end
				 
			else Config(Stuck, sigma, theta1)
		end
	
	| (VHole(ValueHole(a)),t) =>
	   
	   (* First check in given sigma if hole already instantiated *)
		if Substitution.contains(ValueHole(a), sigma) then
		
			(* Hole already instantiated
			   Get existing instantiation
			   Unify all types
			   Get the latest most narrowed down type
				- If it equals the typeof(v) still, return v
				- Otherwise, generate a new value of the more specific type and return that, 
				  as well as updating sigma
				For example, suppose we had 
				narrow(v['a],Int,[v['a]->v['''a]], ['a->'''a]
				First, we get the current mapped value: v['''a]
				Then unify the three types: 'a, Int, '''a
				Giving us the new type substitution: ['a->'''a,'''a->Int]
				Then get the most narrowed down type from 'a, following the chain
				'a -> '''a -> Int
				We see Int does not equal '''a, and it is more narrowed down
				Hence, generate a value of type Int (calling gen), return this value,
				and update sigma to reflect this *)
			let val v = Substitution.get(ValueHole(a), sigma); 
				val (theta1, success) = unify( [THole(TypeHole(a)), t, typeof(v)], theta);
				val latestT = resolveChainTheta(THole(TypeHole(a)),theta1)
			in
				if(success=false) 
				then Config(Stuck, sigma, theta)
				else if(typeof(v) = latestT) 
					 then Config(Expression(Value(v)),sigma,theta1)
					 else let val latestV = gen(t,theta1) 
						  in case v of 
						      VHole(vhole) => Config(Expression(Value(latestV)),Substitution.union(sigma,vhole,latestV),theta1) 
							| _ => Config(Expression(Value(latestV)),sigma,theta1) 
						  end
			end
			
		else
		
			(* Hole not already instantiated *)
			(* Generate value of type t and add to existing instantiations *)
			let val (theta1, success) = unify( [THole(TypeHole(a)), t], theta);
				val v = gen(t, theta1) 
			in
				if(success) 
				then case v of 
				
					  (* prevent adding a map from a value hole to itself as a value *)
					  VHole(ValueHole(vtyVar)) =>
						
						if (a=vtyVar)
						then Config(Expression(Value(v)), sigma, theta1)
						else Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(a),v), theta1)
				
					| _ => Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(a),v), theta1)
				
				else Config(Stuck, sigma, theta)
			end

	| _  => Config(Stuck, sigma, theta);