(* -------------------------------------------------------------------------------- *)
(* Gets the bottom value for a value substitution by following chains in the substitution sigma
   For example, if there is a chain v['a]->v['b], v['b]->v['''b], v['''b]->3
   it will return 3 all when given v['a], v['b] and v['''b]
   Similarly, if there is a chain v['a]->v['b], v['b]->(v['d],v['e]), v['d]->3, v['e]->4
   it will return (3,4) for v['a] and v['b]
   Doesn't detect cycles, but can't be yet?? *)
   
fun resolveChainSigma(a,sigma) = case a of

	  VHole(SimpleHole(ValueHole(typevar))) => 
			if(Substitution.contains(ValueHole(typevar),sigma))
		    then resolveChainSigma(Substitution.get(ValueHole(typevar),sigma),sigma)
			else a
	  
	| ValuePair(v1,v2) => ValuePair(resolveChainSigma(v1,sigma),resolveChainSigma(v2,sigma))
	
	| _ => a; (* Bottom value of int, real or bool *)
	
(* -------------------------------------------------------------------------------- *)	
(* Gets the bottom type for a type substitution sigma, analogously to above *)
   
fun resolveChainTheta(a,theta) = case a of

	  THole(hole) => if(Substitution.contains(hole,theta)) 
					 then resolveChainTheta(Substitution.get(hole,theta),theta)
					 else a
	  
	| Pair(t1,t2) => Pair(resolveChainTheta(t1,theta),resolveChainTheta(t2,theta))
	
	| _ => a; (* Bottom type of Int, Real or Bool *)

	