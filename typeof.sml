(* ----------------------------------------------------------------------------------- *)
(* Generates a fresh type variable of the form depending on enumeration argument 
   (i.e. general type variable, equality type variable, or arithmetic type variable)
   To make it fresh we use a unique integer from the global counter
   We also need to check the generated type variable is neither in the domain or range
   of the current substitution
   If it is (i.e. not fresh), re-call function which will generate a new fresh type 
   variable since the global counter is always unique (and keep recalling until fresh) *)
fun generateFreshTypeVar(typeVarEnum,theta) =

	let val freshTypeVar = case typeVarEnum of
	
		  TYPE_VAR => TypeHole(TypeVar("a" ^ Int.toString(getCounterAndUpdate())))
		  
		| EQUALITY_TYPE_VAR => TypeHole(EqualityTypeVar("a" ^ Int.toString(getCounterAndUpdate())))
		
		| ARITH_TYPE_VAR => TypeHole(ArithTypeVar("a" ^ Int.toString(getCounterAndUpdate())))

	in 
		if element(Substitution.domain(theta),freshTypeVar) orelse element(Substitution.range(theta),THole(freshTypeVar))
		then generateFreshTypeVar(typeVarEnum,theta)
		else THole(freshTypeVar)
	end;

(* ----------------------------------------------------------------------------------- *)
(* Takes two types and an operator (boolean or arithmetic) and determined if the 
   types can be valid argument types to the operator.
   It narrows down types further if value holes and records this information
   in the type substitution passed as an argument
   Split into cases
   - /
   - +,-,*
   - <,<=,>,>=
   - =
*)

fun typeOp(ArithOper(DIVIDE),t1,t2,theta) = (case (t1,t2) of
(* Real/Real	 
   Real/'a		'a/Real		'''a/Real		Real/'''a
   'a/'b 		 '''a/'''b		'a/'''b		'''a/'b 
   In all cases involving a type variable, we can add to the substitution 
   it is now of type real, since / is not overloaded and simply of type real*real->real
*)	

	  (Real,Real) => (SOME Real,theta)
				  
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Real, Substitution.union(theta, TypeHole(ArithTypeVar(a)),Real))
			
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Real, Substitution.union(theta, TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),Real),
									   TypeHole(TypeVar(b)),Real))
												   
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(TypeVar(b)))) => 
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real),
									   TypeHole(TypeVar(b)),Real))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),Real),
									   TypeHole(TypeVar(a)),Real))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(SOME Real, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real),
									   TypeHole(ArithTypeVar(b)),Real))
				
	| _ => (NONE,theta))
	
|	typeOp(ArithOper(_),t1,t2,theta) = (case (t1,t2) of
(* For op = +,-,*
   Int op Int		Real op Real
   Int op alpha		alpha op Int
   Real op 'a		Real op '''a	'a op Real	'''a op Real
   In the above cases involving a type variable we can add to the substitution
   that it is of the type that the other argument is. For example Real op 'a implies
   'a -> Real
   
   alpha op beta, i.e. any combination of type variables
   We can also deduce constraints on the types of the type variables:
     'a op   'b -> both same arithmetic type variable
    ''a op   'b -> both of type int 
   '''a op   'b -> both same arithmetic type variable
     'a op  ''b -> both of type int
	''a op  ''b -> both of type int
   '''a op  ''b -> both of type int
     'a op '''b -> both same arithmetic type variable
	''a op '''b -> both of type int
   '''a op '''b -> both same arithmetic type variable
   All the cases that force the type variables to both be of type int arise because
   the Real type does not occur in the equality type variables
*)
	  (Real,Real) => (SOME Real, theta)
				  
	| (Int,Int)   => (SOME Int, theta)
				
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Real, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Real, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (Int, THole(hole)) => (SOME Int, Substitution.union(theta,hole,Int))
					
	| (THole(hole), Int) => (SOME Int, Substitution.union(theta,hole,Int))
				
	| (THole(TypeHole(hole1)),THole(TypeHole(EqualityTypeVar(b)))) =>
		(SOME Int, Substitution.union(Substitution.union(theta,TypeHole(hole1),Int),
									  TypeHole(EqualityTypeVar(b)),Int))
												  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(hole2))) =>
		(SOME Int, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Int),
									  TypeHole(hole2),Int))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a=b
		then (SOME (THole(TypeHole(ArithTypeVar(a)))), theta)
		else let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			 in (SOME freshVar, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),freshVar),
												   TypeHole(ArithTypeVar(a)),freshVar))
			 end
				
	| (THole(TypeHole(thole1)),THole(TypeHole(thole2))) => 
		let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
		in (SOME freshVar, Substitution.union(Substitution.union(theta,TypeHole(thole1),freshVar),
											  TypeHole(thole2),freshVar))
		end
					
	| _ => (NONE,theta))

|	typeOp(BoolOper(EQ),t1,t2,theta) = 
(* Int=Int		Bool=Bool
   Int=alpha	alpha=Int
   Bool='a		Bool=''a	'a=Bool		''a=Bool
   In the above cases we can add the substitution that the type variable is of the type of
   the other argument
   
   alpha = beta. We can also generate further constraints on the type variable
     'a =   'b -> both same equality type variable
	''a =   'b -> both same equality type variable
   '''a =   'b -> can't deduce anything: may be Int, (Int,Int), ((Int,Int),Int), etc.
     'a =  ''b -> both same equality type variable
	''a =  ''b -> both same equality type variable
   '''a =  ''b -> can't deduce anything
     'a = '''b -> can't deduce anything
	''a = '''b -> can't deduce anything
   '''a = '''b -> can't deduce anything
   The cases where we cannot deduce anything arise because arithmetic & equality type variables
   contain lists and tuples that range over the set as well.
   So, for example, in '''a = ''b, we know one example is Int (since intersection of 
   equality type variables and arithmetic tpye variables is Int), but it could also be
   (Int,Int), so this is also both in the set of arithmetic type variables and the set of 
   equality type variables. We cannot restrict because further down the line another constraint
   may conflict with out choice her.
   
   (t11,t12)=(t21,t22) is both t11=t21 and t12=t22 of form above
*)

	let fun isEqualArgs(t1,t2,theta) =  (case (t1,t2) of 
	
	  (Int,Int) => (SOME Bool,theta)
				  
	| (Int,THole(hole)) => (SOME Bool,Substitution.union(theta,hole,Int))
				
	| (THole(hole),Int) => (SOME Bool,Substitution.union(theta,hole,Int))
				
	| (Bool,Bool) => (SOME Bool,theta)
				
	| (Bool,THole(TypeHole(TypeVar(a)))) => 
		(SOME Bool,Substitution.union(theta,TypeHole(TypeVar(a)),Bool))
					
	| (Bool,THole(TypeHole(EqualityTypeVar(a)))) => 
		(SOME Bool,Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Bool))
					
	| (THole(TypeHole(TypeVar(a))),Bool) => 
		(SOME Bool,Substitution.union(theta,TypeHole(TypeVar(a)),Bool))
					
	| (THole(TypeHole(EqualityTypeVar(a))),Bool) => 
		(SOME Bool,Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Bool))
					
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),freshVar),
				     					  TypeHole(TypeVar(b)),freshVar))
		end
											  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(TypeVar(b)))) =>
		let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),freshVar),
										  TypeHole(TypeVar(b)),freshVar))
		end
												  
	| (THole(TypeHole(TypeVar(a))),THole(TypeHole(EqualityTypeVar(b)))) =>
		let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(TypeVar(a)),freshVar),
										  TypeHole(EqualityTypeVar(b)),freshVar))
		end
						
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(EqualityTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a = b
		then (SOME Bool, theta)
		else let val freshVar = generateFreshTypeVar(EQUALITY_TYPE_VAR,theta)
			 in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),freshVar),
											   TypeHole(EqualityTypeVar(b)),freshVar))
			 end
				
	| (THole(hole1),THole(hole2)) => (SOME Bool, theta)
					
	| (Pair(t11,t12),Pair(t21,t22)) =>
		let val (arg0,theta1) = isEqualArgs(t11,t21,theta);
			val (arg1,theta2) = isEqualArgs(t12,t22,theta1)
		in case (arg0,arg1) of 
			(SOME Bool, SOME Bool) => (SOME Bool,theta2)
							   | _ => (NONE,theta2)
		end
					 
	| _ => (NONE,theta))
	
	in isEqualArgs(t1,t2,theta) end

|	typeOp(BoolOper(_),t1,t2,theta) = (case (t1,t2) of 
(* For op = <,<=,>,>=
   Int op Int		Real op Real
   Int op alpha		alpha op Int
   Real op 'a		Real op '''a	'a op Real	'''a op Real
   In the above cases involving a type variable we can add to the substitution
   that it is of the type that the other argument is. For example Real op 'a implies
   'a -> Real
   
   alpha op beta, i.e. any combination of type variables
   We can also deduce constraints on the types of the type variables: same as op +,-,*
     'a op   'b -> both same arithmetic type variable
    ''a op   'b -> both of type int 
   '''a op   'b -> both same arithmetic type variable
     'a op  ''b -> both of type int
	''a op  ''b -> both of type int
   '''a op  ''b -> both of type int
     'a op '''b -> both same arithmetic type variable
	''a op '''b -> both of type int
   '''a op '''b -> both same arithmetic type variable
   All the cases that force the type variables to both be of type int arise because
   the Real type does not occur in the equality type variables
*)	
	  (Real,Real) => (SOME Bool, theta)
				  
	| (Int,Int)   => (SOME Bool, theta)
				
	| (Real,THole(TypeHole(TypeVar(a)))) => (SOME Bool, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
				
	| (THole(TypeHole(TypeVar(a))),Real) => (SOME Bool, Substitution.union(theta,TypeHole(TypeVar(a)),Real))
					
	| (Real,THole(TypeHole(ArithTypeVar(a)))) => (SOME Bool, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (THole(TypeHole(ArithTypeVar(a))),Real) => (SOME Bool, Substitution.union(theta,TypeHole(ArithTypeVar(a)),Real))
					
	| (Int, THole(hole)) => (SOME Bool, Substitution.union(theta,hole,Int))
					
	| (THole(hole), Int) => (SOME Bool, Substitution.union(theta,hole,Int))
					
	| (THole(TypeHole(hole1)),THole(TypeHole(EqualityTypeVar(b)))) =>
		(SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(hole1),Int),
									   TypeHole(EqualityTypeVar(b)),Int))
												  
	| (THole(TypeHole(EqualityTypeVar(a))),THole(TypeHole(hole2))) =>
		(SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(EqualityTypeVar(a)),Int),
									   TypeHole(hole2),Int))
				
	| (THole(TypeHole(ArithTypeVar(a))),THole(TypeHole(ArithTypeVar(b)))) =>
		(* if already same type, no need to generate a fresh type variable for them to share *)
		if a=b
		then (SOME Bool, theta)
		else let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
			in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(ArithTypeVar(b)),freshVar),
											  TypeHole(ArithTypeVar(a)),freshVar))
			 end
				
	| (THole(TypeHole(thole1)),THole(TypeHole(thole2))) => 
		let val freshVar = generateFreshTypeVar(ARITH_TYPE_VAR,theta)
		in (SOME Bool, Substitution.union(Substitution.union(theta,TypeHole(thole1),freshVar),
										  TypeHole(thole2),freshVar))
		end
					
	| _ => (NONE,theta));
	
(* ----------------------------------------------------------------------------------- *)
(* Returns the type of a simple value hole, which may be simple, e.g. v['a], 
   or compound, e.g. v[if 'a then 3 else 4] *)
   
fun typeofhole(SimpleHole(ValueHole(tyVar)),theta) =  
		if Substitution.contains(TypeHole(tyVar),theta) 
		then (SOME (resolveChainTheta(THole(TypeHole(tyVar)),theta)), theta)
		else (SOME (THole(TypeHole(tyVar))),theta)
	
|  	typeofhole (BinaryOp(EXPR_PAIR,valhole1,valhole2),theta) =

	(case typeofhole(valhole1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case typeofhole(valhole2,theta1) of
		
			  (NONE,theta2) => (NONE,theta2)
			| (SOME(t2),theta2) => (SOME (Pair(t1,t2)),theta2)))
			
|  	typeofhole (BinaryOp(oper,valhole1,valhole2),theta) =

	(case typeofhole(valhole1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case typeofhole(valhole2,theta1) of
		
			  (NONE,theta2) => (NONE,theta2)
			| (SOME(t2),theta2) => typeOp(oper,t1,t2,theta2)))

|	typeofhole (CaseHole(valhole,VariablePair(x1,x2),expr),theta) =
	(* For 'case valhole of (x,y) -> e'
	   Firstly, call typeofhole recursively on valhole
	   The type returned can only be 
		(i)  simple value hole, v['a]. Here, we can then further say v['a] can be replaced 
			 by compound value hole v[('b,'c)] for fresh 'b and 'c, with the mapping 
			 'a -> 'b * 'c added to the substitution
			 We can even more specific and say 
				'a -> (  'b *   'c)
			   ''a -> ( ''b *  ''c)
			  '''a -> ('''b * '''c)
		(ii) compound value hole, of form v[('a,'b)]
	  We then call substitute on e' with x and y mapping to their respective value holes
	  Finally, we calculate the type of that resulting expression
	*)
	(case typeofhole(valhole,theta) of 
	
		  (SOME (THole(TypeHole(tyvar))),theta1) => 
			let val typevar_type = case tyvar of TypeVar(_)    => TYPE_VAR
										  | ArithTypeVar(_)    => ARITH_TYPE_VAR
									      | EqualityTypeVar(_) => EQUALITY_TYPE_VAR;
				val t1 as THole(TypeHole(fresh1)) = generateFreshTypeVar(typevar_type,theta1);
				val t2 as THole(TypeHole(fresh2)) = generateFreshTypeVar(typevar_type,theta1);
				val subExpr = substitute(expr,
						[(x1,Value(VHole(SimpleHole(ValueHole(fresh1))))),
						 (x2,Value(VHole(SimpleHole(ValueHole(fresh2)))))]);
				val theta2 = Substitution.union(theta1,TypeHole(tyvar),Pair(t1,t2))
			in typeofexpr(subExpr,theta2) end
		
		| (SOME (Pair(THole(TypeHole(tyvar1)),THole(TypeHole(tyvar2)))),theta1) => 
			let val subExpr = substitute(expr,[(x1,Value(VHole(SimpleHole(ValueHole(tyvar1))))),
											   (x2,Value(VHole(SimpleHole(ValueHole(tyvar2)))))])
			in typeofexpr(subExpr,theta1) end
		 
		| (_,theta1) => (NONE,theta1))

| 	typeofhole (ConditionHole(valhole1,_,_),theta) =
	(* Only check guard is typeable to Bool. Don't restrict branches *)
	
	(case typeofhole(valhole1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case t1 of
		
			  Bool => 
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, theta1) end
			  
			| THole(TypeHole(TypeVar(a))) =>
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, Substitution.union(theta1,TypeHole(TypeVar(a)),Bool)) end
				
			| THole(TypeHole(EqualityTypeVar(a))) =>
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, Substitution.union(theta1,TypeHole(EqualityTypeVar(a)),Bool)) end
				
			| _ => (NONE, theta1)))
	
(* no semi-colon: mutually recursive with typeof and typeofexpr *)
(* ----------------------------------------------------------------------------------- *)	
(* typeof returns dynamic type of a value
   Returns pair of type (wrapped in option datatype) and a type substitution
   We use option datatype in case it cannot be typed. This can only be the case in value
   holes, such as v[ ('a/'b) = ('c/'d)]
   The type substitution is again only used in value holes, and represents any constraints
   placed on the type variables which are contained in the value holes.
   For example, for v[ ('a/'b) + ('c/'d)], it would return type Int, and the substitution
   { 'a->Real, 'b->Real, 'c->Real, 'd->Real} *)
   
and typeof (v,theta) = case v of
	  N(_) => (SOME Int,theta)
	| B(_) => (SOME Bool,theta)
    | R(_) => (SOME Real,theta)
	| ValuePair(v1,v2) => 
		(case typeof(v1,theta) of
		
			  (NONE,theta1) => (NONE,theta1)
			| (SOME(a),theta1) => (case typeof(v2,theta1) of
			  
					  (NONE,theta2) => (NONE,theta2)
					| (SOME(b),theta2) => (SOME (Pair(a,b)),theta2)))
					
	| VHole(h) => typeofhole(h,theta)
	
(* no semi-colon: mutually recursive with typeofhole and typeofexpr *)
(* ----------------------------------------------------------------------------------- *)
(* typeofexpr returns dynamic type of an expression wrapped in an option datatype
   with NONE being returned if it cannot be typed *)
   
and typeofexpr(Value(v),theta) = typeof(v,theta)

(* Assume variable always in variable map gamma of variables -> expressions 
   Don't  care about static errors due to unbound variables: cannot lead to witnesses
   Can this even occur? Do we not substitute all variables with an expression anyway
| 	typeofexpr(Variable(x),theta,gamma) = typeofexpr(Substitution.get(gamma,x)) *)

| 	typeofexpr(ArithExpr(oper,e1,e2),theta) =

	(case typeofexpr(e1,theta) of
	  
			(NONE,theta1) => (NONE,theta1)
		  | (SOME t1,theta1) => (case typeofexpr(e2,theta1) of
		  
			    (NONE,theta2) => (NONE,theta2)
			  | (SOME t2,theta2) => typeOp(ArithOper(oper),t1,t2,theta2)))
				
|	typeofexpr(BoolExpr(oper,e1,e2),theta) =

	(case typeofexpr(e1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case typeofexpr(e2,theta1) of
		
			  (NONE,theta2) => (NONE,theta2)
			| (SOME(t2),theta2) => typeOp(BoolOper(oper),t1,t2,theta2)))

| 	typeofexpr(ExpressionPair(e1,e2),theta) =

	(case typeofexpr(e1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case typeofexpr(e2,theta1) of
		
			  (NONE,theta2) => (NONE,theta2)
			| (SOME(t2),theta2) => (SOME (Pair(t1,t2)),theta2)))

|	typeofexpr(Case(e1,VariablePair(x1,x2),e2),theta) =
(* Calculate the type of e1 recursively, and check it is of the form that can
   match the pattern p: Pair or THole
   We only need to calculate type, we are not concerned with the actual value of the expression
   Supposed e1 types to Pair(a,b)
   And if it types to 'a, add mapping 'a->Pair(a,b), for fresh a,b type variables
   Then substitute e2[gen(a)/x][gen(b)/y] and recursively call typeofexpr on e2   
*)
	(case typeofexpr(e1,theta) of 
	
		  (SOME (THole(TypeHole(tyvar))),theta1) => 
			let val typevar_type = case tyvar of TypeVar(_)    => TYPE_VAR
										  | ArithTypeVar(_)    => ARITH_TYPE_VAR
									      | EqualityTypeVar(_) => EQUALITY_TYPE_VAR;
				val t1 as THole(TypeHole(fresh1)) = generateFreshTypeVar(typevar_type,theta1);
				val t2 as THole(TypeHole(fresh2)) = generateFreshTypeVar(typevar_type,theta1);
				val subExpr = substitute(e2,[(x1,Value(gen(t1,theta))),(x2,Value(gen(t2,theta)))]);
				val theta2 = Substitution.union(theta1,TypeHole(tyvar),Pair(t1,t2))
			in typeofexpr(subExpr,theta2) end
		
		| (SOME (Pair(t1,t2)),theta1) => 
			let val subExpr = substitute(e2,[(x1,Value(gen(t1,theta))),(x2,Value(gen(t2,theta)))])
			in typeofexpr(subExpr,theta1) end
		 
		| (_,theta1) => (NONE,theta1))

|	typeofexpr(Condition(e1,e2,e3),theta) =
	(* Only check guard is typeable to Bool. Don't restrict branches *)
	
	(case typeofexpr(e1,theta) of 
	
		  (NONE,theta1) => (NONE,theta1)
		| (SOME(t1),theta1) => (case t1 of
		
			  Bool => 
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, theta1) end
			  
			| THole(TypeHole(TypeVar(a))) =>
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, Substitution.union(theta1,TypeHole(TypeVar(a)),Bool)) end
				
			| THole(TypeHole(EqualityTypeVar(a))) =>
				let val freshVar = generateFreshTypeVar(TYPE_VAR,theta1)
				in (SOME freshVar, Substitution.union(theta1,TypeHole(EqualityTypeVar(a)),Bool)) end
				
			| _ => (NONE, theta1)));

