(* typeof returns dynamic type of a value *)

fun typeof (v) = case v of
	  N(_) => Int
	| B(_) => Bool
	| R(_) => Real
	| VHole(ValueHole(a)) => THole(TypeHole(a));