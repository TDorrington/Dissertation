signature SUBSTITUTION =

	sig
		
	type (''a,'b) map;									(* type of map *)
	exception SubException								(* exception, e.g. for getting mapped value from empty map *)
	val union: (''a,'b) map * ''a * 'b -> (''a,'b )map	(* add new mapping to map*)
	val get: ''a * (''a,'b) map -> 'b					(* get value mapped to under substitution *)
	val contains: ''a * (''a,'b) map -> bool			(* returns true iff argument in domain of map *)
	val update: (''a,'b) map * ''a * 'b -> (''a,'b)map  (* updates map to have new pair (''a,'b) *)
 
	end;
