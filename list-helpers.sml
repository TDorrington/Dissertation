(* Auxiliary function to replace all occurrences of a with b in a map
   E.g. replace( [('a,'b)], 'b, Int ) -> [('a,Int)] 
   Assumes no map of form x->x in the substitution *)
   
fun replace ([],_,_) = []
|   replace ((x,y)::l,a,b) =
	if a=x then (b,y)::replace(l,a,b) 
		   else if a=y then (x,b)::replace(l,a,b) 
					   else (x,y)::replace(l,a,b);
					   
(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function to append two lists, i.e. union with repeat *)

fun append([],ys) = ys
|	append(x::xs,ys) = x::append(xs,ys);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which takes a list and an element, and returns true iff 
   the element is in the list, otherwise returns false *)
   
fun element([],_) = false
|   element(x::l,y) = (x=y) orelse element(l,y);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function to union two lists: no repeats *)

fun union([],ys) = ys
|	union(x::xs,ys) = if element(ys,x) then union(xs,ys) else x::union(xs,ys);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which takes a list, l, and a list of elements, els, and removes 
   any occurrences of elements in els from the list, l
   e.g. remove([1,2,3,1],[1])   = [2,3]
   and  remove([1,2,3,4],[1,2]) = [3,4] *)
   
fun remove([],_) = []
|   remove(x::l,els) = if element(els,x) then remove(l,els) else x::remove(l,els);

(* ----------------------------------------------------------------------------------- *)
(* Auxiliary function which takes two lists, 
   and returns all the elements of the second list which are an element of the first
   used for capture avoiding substitutions *)
   
fun listElement(l,[]) = []
|   listElement(l,x::rest) = 
	if element(l,x) then union([x],listElement(l,rest))
					else listElement(l,rest);