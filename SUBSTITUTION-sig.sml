signature SUBSTITUTION =

	sig
		
	datatype (''a, 'b) t					(* type of map *)
	exception SubException					(* exception, e.g. for getting mapped value from empty map *)
	val union: (''a, 'b) t * ''a * 'b -> (''a, 'b) t	(* add new mapping to map*)
	val get: ''a * (''a, 'b) t -> 'b			(* get value mapped to under substitution *)
	val contains: ''a * (''a, 'b) t -> bool			(* returns true iff argument in domain of map *)
 
	end;
