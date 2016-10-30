(* unify: t list * typeSub -> typeSub * bool *)

fun unify([THole(typeHole(a)), t], theta) =
	
	case t of Bool => Substitution.union(theta, typeHole(a), t)
			| Int  => Substitution.union(theta, typeHole(a), t)
			| Real => Substitution.union(theta, typeHole(a), t)
			| THole(typeHole(b)) =>
	
|	unify(