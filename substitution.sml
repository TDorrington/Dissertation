structure Substitution : SUBSTITUTION =
	struct

		type (''a, 'b) map = (''a * 'b) list;

		exception SubException;

		fun union (m, x, y) =
		    case m of [] => [(x,y)]
		    	     | l => (x,y)::l;
	
		fun get (x, m) =
		    case m of [] => raise SubException
		    	| (y,z)::l => if x=y then z else get(x,l);

		
		fun contains(x, m) = 
		     case m of [] => false
				 | (y,z)::l => if x=y then true else contains(x,l);

	end;
