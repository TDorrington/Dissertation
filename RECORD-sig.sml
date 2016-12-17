(* Record labels datatype *)
datatype lab = Lab of string;

signature RECORD =

	sig
	
	type 'a dictionary;
	exception RecordException;					(* Type of exception for record structure  *)
	val sort: 'a dictionary -> 'a dictionary;	(* Sorts a record based on its labels      *)
	val getLabels: 'a dictionary -> lab list;	(* Gets the labels for a record, as a list *)
	val getFields: 'a dictionary -> 'a list;	(* Gets the fields for a record, as a list *)
	val access: 'a dictionary * lab -> 'a; 		(* Gets the field associated with a label  *)
	
	(* Takes two records, and returns a list of pairs of fields from the two 
	   records where the labels are the same *)
	val merge: 'a dictionary * 'b dictionary -> ('a * 'b) list option;	
	
	end;