structure Substitution : SUBSTITUTION =

	struct

		(* type of map *)
		type (''a, 'b) map = (''a * 'b) list;

		(* exception associated with structure, thrown when e.g.
			- calling get for value x, when there is no map of the form x->y
			- calling update for value x, when no map of the form x->y *)
		exception SubException;
		
		(* returns true iff argument in domain of map *)
		fun contains(x, m) = 
		     case   m of [] => false
				 | (y,_)::l => if x=y then true else contains(x,l);
		
		(* add new mapping x->y to map
		   If mapping already exists for x, do not do anything, i.e. no update *)
		fun union (m, x, y) =
		    case m of [] => [(x,y)]
		    	     | l => if contains(x,m) then l else (x,y)::l;
	
		(* get value mapped to under substitution *)
		fun get (x, m) =
		    case   m of [] => raise SubException
		    	| (y,z)::l => if x=y then z else get(x,l);

		(* updates map to replace pair (x,z), for some z, to have (x,y) *)
		fun update(m, x, y) = 
			case   m of [] => raise SubException 
			    | (a,b)::l => if a=x then (a,y)::l else (a,b)::update(l,x,y);
		
		(* returns domain of substitution *)
		fun domain(m) = 
			case 	 m of [] => []
				 |  (a,_)::l => a::domain(l);
				
		(* returns range of substitution *)
		fun range(m) =
			case 	m of [] => []
				 | (_,b)::l => b::range(l);
		
	end;