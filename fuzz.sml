fuzz( ... ) = list of full expressions paired with their counter with which changed

	generate [variables -> types]

	value => switch to value other type(s)
		int => bool, int list => bool,
		(int -> int) => real
	
	value hole => exception
	
	variable => variable of another type
				if no other variables of other type, put in value of another type
		alpha rename is an extension
		
		
	arith expr => - switch to bool expression, for arbitrary operator
				  - switch *,-,+ <=> /
				  - ...
				  - e1 op e2 <=> e1::e2
				  
	bool expr => same as arith expr
	
	case => change 1 pattern by changing concrete values, add remove record labels
			
	condition => changes types of sub expressions
	
	app => put in function of different arg/return type of e1
	
	record => add & remove labels
	
	let & let rec => change type
		   
	cons => change types of e1 e2
	
	counterexpr => recurse
	
