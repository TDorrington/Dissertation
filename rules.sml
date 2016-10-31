(* -------------------------------------------------------------------------------- *)
(* Curried functions: Take operator 'a * 'a -> 'b,
   and returns function Value('a) * Value('a) => Value('b)
   i.e. wrap input & output into value datatype *)	

(* Function that wraps I/O of n+n, n-n, or n*n (not n/n) in value datatype *)
val arithInt = fn oper => fn (N(n1), N(n2)) => N(oper(n1,n2));
	
(* Function that wraps I/O of r+r, r-r, r/r or r*r in value datatype *)
val arithReal = fn oper => fn (R(r1), R(r2)) => R(oper(r1,r2));
	
(* Function that wraps I/O of n<n, n<=n, etc. in value datatype *)
val logicalInt = fn oper => fn (N(n1), N(n2)) => B(oper(n1,n2));
	
(* Function that wraps I/O of r<r, r<=r, etc. (not r=r) in value datatype *)
val logicalReal = fn oper => fn (R(r1), R(r2)) => B(oper(r1,r2));

(* -------------------------------------------------------------------------------- *)
(* rules E-OP-ARITH-BOOL-GOOD, -BAD1, -BAD2 *)
fun elabPhraseArithBool (v1, v2, sigma, theta, oper, t1, t2) : config = 
	
	case narrow(v1,t1,sigma,theta) of
			
		  (* rule E-OP-ARITH-BOOL-BAD1 *)
		  c1 as Config(Stuck,sigma1,theta1) => c1

		| Config(Expression(Value(n1)),sigma1,theta1) =>
				
			case narrow(v2,t2,sigma,theta) of
					
				  (* rule E-OP-ARITH-BOOL-BAD2 *)
				  c2 as Config(Stuck,sigma2,theta2) => c2
						
				  (* rule E-OP-ARITH-BOOL-GOOD *)
				| _ =>
							
					(* get value using type & value substitutions from n1 *)
					let val Config(Expression(Value(n3)),sigma3,theta3) = 
							narrow(v2,t2,sigma1,theta1);
							
					in 
					case (t1,t2) of
						  (Int,Int) => Config(Expression(Value(oper(n1,n3))), sigma3, theta3) 
						| (Real,Real) => Config(Expression(Value(oper(n1,n3))), sigma3, theta3) 
						| _ => Config(Expression(Plus(Value(n1),Value(n3))), sigma3, theta3) 
					
					end;
		
(* -------------------------------------------------------------------------------- *)
(* Arithmetic operations: +,-,* *)

datatype operType = PLUS | SUBTRACT | DIVIDE | TIMES;

fun elabPhraseArithGeneral (v1,v2,sigma,theta,oper) =
	
	let val (intArith, realArith) = 
		case oper of PLUS     => (arithInt(op+),arithReal(op+))
				   | SUBTRACT => (arithInt(op-),arithReal(op-))
				   | TIMES    => (arithInt(op*),arithReal(op*))
				   | DIVIDE   => (arithInt(op*),arithReal(op/)) (* intArith arbitrary, never used *)
	in
	
	case (v1,v2) of 
	
		  (* op : int * int -> int *)
		  (N(_),N(_)) 	  	  => elabPhraseArithBool(v1,v2,sigma,theta, intArith, Int,Int)
		| (N(_),VHole(_)) 	  => elabPhraseArithBool(v1,v2,sigma,theta, intArith, Int,Int)
		| (VHole(_),N(_)) 	  => elabPhraseArithBool(v1,v2,sigma,theta, intArith, Int,Int)
					
		  (* op : real * real -> real *)
		| (R(_),R(_)) 	  	  => elabPhraseArithBool(v1,v2,sigma,theta, realArith, Real,Real)
		| (R(_),VHole(_)) 	  => elabPhraseArithBool(v1,v2,sigma,theta, realArith, Real,Real)
		| (VHole(_),R(_)) 	  => elabPhraseArithBool(v1,v2,sigma,theta, realArith, Real,Real)
		
		| (VHole(ValueHole(a)),VHole(ValueHole(b))) =>
			
			let val s1 = case a of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s;
				val s2 = case b of TypeVar(s) => s | EqualityTypeVar(s) => s | ArithTypeVar(s) => s
			in
				(* ignores arithReal(op+) argument in this case *)
				elabPhraseArithBool(v1,v2,sigma,theta,intArith,
					THole(TypeHole(ArithTypeVar(s1))), THole(TypeHole(ArithTypeVar(s2))))
			end
								 
		| _			 	 	  => Config(Stuck,sigma,theta)
		
	end;
		

fun elabPhraseArith (Config(Expression(Plus(Value(v1),Value(v2))),sigma,theta)) = 
		elabPhraseArithGeneral(v1,v2,sigma,theta,PLUS)

|	elabPhraseArith (Config(Expression(Subtract(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseArithGeneral(v1,v2,sigma,theta,SUBTRACT)

|  	elabPhraseArith (Config(Expression(Times(Value(v1),Value(v2))),sigma,theta)) =
		elabPhraseArithGeneral(v1,v2,sigma,theta,TIMES)

|	elabPhraseArith (Config(Expression(Divide(Value(R(v1)),Value(R(v2)))),sigma,theta)) =
		(* only arithmetic operator with real arguments only *)
		elabPhraseArithGeneral(R(v1),R(v2),sigma,theta,DIVIDE);
		
(* real *)
val c2' = Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))), 
								  Value(R(3.0)))), [], []);
		
(* -------------------------------------------------------------------------------- *)
(* Boolean operation: < *)

fun elabPhraseLessThan (Config(Expression(LessThan(Value(v1),Value(v2))),sigma,theta)) =

	case (v1,v2) of 
	
		  (N(_),N(_)) => (* < : int * int -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalInt(op<))
					
		| (R(_),R(_)) => (* < : real * real -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalReal(op<))
		
		| _			  => Config(Stuck,sigma,theta);
		
(* -------------------------------------------------------------------------------- *)
(* Boolean operation: > *)

fun elabPhraseMoreThan (Config(Expression(MoreThan(Value(v1),Value(v2))),sigma,theta)) =

	case (v1,v2) of 
	
		  (N(_),N(_)) => (* > : int * int -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalInt(op>))
					
		| (R(_),R(_)) => (* > : real * real -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalReal(op>))
		
		| _			  => Config(Stuck,sigma,theta);
(* -------------------------------------------------------------------------------- *)
(* Boolean operation: >= *)

fun elabPhraseMoreThanEqual (Config(Expression(MoreThanEqual(Value(v1),Value(v2))),sigma,theta)) =

	case (v1,v2) of 
	
		  (N(_),N(_)) => (* >= : int * int -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalInt(op>=))
					
		| (R(_),R(_)) => (* >= : real * real -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalReal(op>=))
		
		| _			  => Config(Stuck,sigma,theta);

(* -------------------------------------------------------------------------------- *)
(* Boolean operation: <= *)

fun elabPhraseLessThanEqual (Config(Expression(LessThanEqual(Value(v1),Value(v2))),sigma,theta)) =

	case (v1,v2) of 
	
		  (N(_),N(_)) => (* <= : int * int -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalInt(op<=))
					
		| (R(_),R(_)) => (* <= : real * real -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalReal(op<=))
		
		| _			  => Config(Stuck,sigma,theta);

(* -------------------------------------------------------------------------------- *)
(* Boolean operation: = *)

fun elabPhraseEqual (Config(Expression(Equal(Value(v1),Value(v2))),sigma,theta)) =

	case (v1,v2) of 
	
		  (N(_),N(_)) => (* = : int * int -> bool, no clause for real * real -> bool *)
						  elabPhraseArithBool(v1,v2,sigma,theta, logicalInt(op=))
					
		| _			  => Config(Stuck,sigma,theta);

(* -------------------------------------------------------------------------------- *)
	
fun elabPhraseCond (Config(Expression(Condition(Value(v),e1,e2)),sigma,theta)) : config =

	case narrow(v,Bool,sigma,theta) of
	
		  (* rule E-IF-BAD *)
		  c as Config(Stuck,sigma1,theta1) => c
		 
		| Config(Expression(Value(B(b))),sigma2,theta2) =>
		
			if b (* rule E-IF-GOOD1 *)
				 then Config(Expression(e1),sigma2,theta2)
				
				 (* rule E-IF-GOOD2 *)
				 else Config(Expression(e2),sigma2,theta2);