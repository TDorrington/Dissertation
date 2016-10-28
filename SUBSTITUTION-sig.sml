signature SUBSTITUTION =

	sig
		
	type (''a, 'b) t					(* type of map *)
	exception SubException					(* exception, e.g. for getting mapped value from empty map *)
	val union: (''a, 'b) t * ''a * 'b -> (''a, 'b) t	(* add new mapping to map*)
	val get: ''a * (''a, 'b) t -> 'b			(* get value mapped to under substitution *)
 
	end;
