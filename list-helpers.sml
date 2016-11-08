(* Auxiliary function to replace all occurrences of a (which will be a type variable)
   with type of b in a map
   E.g. replace( [('a,'b)], 'a, Int ) -> [(Int,'b)] *)
   
fun replace ([],_,_) = []
|   replace ((x,y)::l,a,b) =
	if a=x then (b,y)::replace(l,a,b) else 
	if a=y then (x,b)::replace(l,a,b) else (x,y)::replace(l,a,b);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which takes a list and an element, and returns true iff 
   the element is in the list, otherwise returns false *)
   
fun element([],_) = false
|   element(x::l,y) = (x=y) orelse element(l,y);
	
(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function to append two lists, i.e. union with repeat *)

fun append([],ys) = ys
|	append(x::xs,ys) = x::append(xs,ys);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function to union two lists: no repeats *)

fun union([],ys) = ys
|	union(x::xs,ys) = if element(ys,x) then union(xs,ys) else x::union(xs,ys);


(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which takes a list and an element, and removes any occurrences 
   of that element from the list
   e.g. remove([1,2,3,1],1) = [2,3] *)
   
fun remove([],_) = []
|   remove(x::l,y) = if x=y then remove(l,y) else x::remove(l,y);

