(* 'narrow' dynamically performs type-checking 
    narrow : v * t * valSub * typeSub -> <v union stuck, valSub, typeSub>
    takes a value v, type t and current values & type substitutions and
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow(v,t,sigma,theta) = 

case (v,t) of

	  (N(integer),Int)  => Config(Expression(Value(N(integer))), sigma, theta)
	 
	| (B(boolean),Bool) => Config(Expression(Value(B(boolean))), sigma, theta)

	| (R(real),Real)    => Config(Expression(Value(R(real))), sigma, theta)
	
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
	(* When v a hole, check in given sigma first if hole already instantiated
	   and if so, return existing instantiation *)
	   
		if Substitution.contains(ValueHole(a), sigma) then
		
			let val v = Substitution.get(ValueHole(a), sigma); 
				val (theta1, success) = unify( [THole(TypeHole(a)), t, typeof(v)], theta) 
			in
				if(success) then Config(Expression(Value(v)),sigma,theta1)
							else Config(Stuck, sigma, theta)	
			end
			
		else
		
			let val (theta1, success) = unify( [THole(TypeHole(a)), t], theta);
				val v = gen(t, theta1) 
			in
				if(success) 
					then Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(a),v), theta1)
					else Config(Stuck, sigma, theta)
			end

	| _  => Config(Stuck, sigma, theta);