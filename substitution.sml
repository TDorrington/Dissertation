structure Substitution : SUBSTITUTION =

	struct

		type (''a, 'b) map = (''a * 'b) list;

		exception SubException;
		
		fun contains(x, m) = 
		     case   m of [] => false
				 | (y,z)::l => if x=y then true else contains(x,l);
				 
		fun union (m, x, y) =
		    case m of [] => [(x,y)]
					 (* only add map if not already one for x - don't update (?) *)
		    	     | l => if contains(x,m) then l else (x,y)::l;
	
		fun get (x, m) =
		    case   m of [] => raise SubException
		    	| (y,z)::l => if x=y then z else get(x,l);

		fun update(m, x, y) = 
			case   m of [] => raise SubException 
			    | (a,b)::l => if a=x then (a,y)::l else (a,b)::update(l,x,y);
		
	end;
