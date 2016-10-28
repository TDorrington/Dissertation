structure Subsitution : SUBSTITUTION =
	struct

		datatype (''a, 'b) t = Substitution of (''a * 'b) list;

		exception SubException;

		fun union (Substitution(map), x, y) =
		    case map of [] => Substitution([(x,y)])
		    	       | l => Substitution((x,y)::l);
	
		fun get (x, Substitution(map)) =
		    case map of [] => raise SubException
		    	| (y,z)::l => if x=y then z else get(x,Substitution(l));

	end
