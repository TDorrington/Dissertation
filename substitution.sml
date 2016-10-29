structure Substitution : SUBSTITUTION =
	struct

		datatype (''a, 'b) t = Map of (''a * 'b) list;

		exception SubException;

		fun union (Map(map), x, y) =
		    case map of [] => Map([(x,y)])
		    	       | l => Map((x,y)::l);
	
		fun get (x, Map(map)) =
		    case map of [] => raise SubException
		    	| (y,z)::l => if x=y then z else get(x,Map(l));

		
		fun contains(x, Map(map)) = 
		     case map of [] => false
			| (y,z)::l => if x=y then true else contains(x,Map(l));

	end
