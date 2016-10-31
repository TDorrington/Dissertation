(* 'narrow' dynamically performs type-checking 
    narrow : v * t * valSub * typeSub -> <v union stuck, valSub, typeSub>
    takes a value v, type t and current values & type substitutions and
    refines v to have type t by yielding triple of either same value
    and substitutions, or yields stuck state if not possible *)	
	
fun narrow (N(integer), Int, sigma, theta) = 
	Config(Expression(Value(N(integer))), sigma, theta)

|	narrow (B(boolean), Bool, sigma, theta) =
	Config(Expression(Value(B(boolean))), sigma, theta)

| 	narrow (R(real), Real, sigma, theta) =
	Config(Expression(Value(R(real))), sigma, theta)
	
|  narrow(VHole(ValueHole(a)), t, sigma, theta) =
	(* When v a hole, check in given sigma first if hole already instantiated
	   and if so, return existing instantiation *)
	   
	if Substitution.contains(ValueHole(a), sigma) then
	
		let val v = Substitution.get(ValueHole(a), sigma); 
			val (theta1, success) = unify( [THole(TypeHole(a)), t, typeof(v)], theta) in
			if(success) then Config(Expression(Value(v)),sigma,theta1)
				        else Config(Stuck, sigma, theta)	
		end
		
	else
	
		let val (theta1, success) = unify( [THole(TypeHole(a)), t], theta);
			val v = gen(t, theta1) in
			if(success) 
				then Config(Expression(Value(v)), Substitution.union(sigma,ValueHole(a),v), theta1)
				else Config(Stuck, sigma, theta)
		end

| 	narrow (_,_,sigma,theta) = 
	Config(Stuck, sigma, theta);