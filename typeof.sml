(* typeof returns dynamic type of a value *)

fun typeof (v) = case v of
	  N(_) => Int
	| B(_) => Bool
    | R(_) => Real 
	| ValuePair(v1,v2) => Pair(typeof(v1),typeof(v2))
	| VHole(ValueHole(a)) => THole(TypeHole(a));