structure Record : RECORD =

	struct 
	
		(* type of record *)
		type 'a dictionary = (lab * 'a) list;
		
		(* type of exception for record structure. thrown if
		   - we try to get a field corresponding to a label not in the record *)
		exception RecordException;
		
		(* sorts record, based on labels, into ascending order using quick sort *)
		fun sort([]) = []
		|	sort([x]) = [x]
		|	sort((Lab(s),t)::rest) = 
		let fun part (l,r,[]) = (sort l) @ ((Lab(s),t)::(sort r))
			  | part (l,r,(Lab(s1),t1)::l1) = 
					if s1<s then part((Lab(s1),t1)::l,r,l1)
								else part(l,(Lab(s1),t1)::r,l1)
		in part([],[],rest) end;
		
		(* gets labels for a record, as a list *)
		fun getLabels([]) = []
		|	getLabels((lab1,_)::l1) = lab1::getLabels(l1);
		
		(* gets fields for a record, as a list *)
		fun getFields([]) = []
		|	getFields((_,f)::l1) = f::getFields(l1);
		
		(* Gets the field associated with a label *)
		fun access(r,lab) = (case r of 
			  [] => raise RecordException
			| (lab1,f1)::r1 => if lab=lab1 then f1 else access(r1,lab));
		
	    (* Takes two records, and returns a list of pairs of fields from the two 
	       records where the labels are the same
		   Returned in option datatype: NONE if failed (different number or 
		   list of labels *)
		fun merge(r1,r2) = 
		
			let fun localMerge(r1,r2) = (case (r1,r2) of 
	
				  ([],[]) => SOME []
				| ([],_)  => NONE
				| (_,[])  => NONE
				| ((lab1,f1)::rest1,(lab2,f2)::rest2) => 
					if lab1=lab2 
					then (case merge(rest1,rest2) of
							  SOME l => SOME ((f1,f2)::l)
							| NONE 	 => NONE)
					else NONE)
					
			in localMerge(sort(r1),sort(r2)) end;
								
	end;