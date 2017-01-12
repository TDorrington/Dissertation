(* ----------------------------------------------------------------------------------- *)
(* TETS CASES FOR SUBSTITUTION STRUCTURE *)

val l = [ (1,"a"), (2,"b"), (3,"c"), (4,"d") ];
Substitution.contains(1,l);  (* true  *)
Substitution.contains(3,l);  (* true  *)
Substitution.contains(5,l);  (* false *)
val l = Substitution.union(l,5,"e"); (* [(5,"e"),(1,"a"),(2,"b"),(3,"c"),(4,"d")] *)
Substitution.get(1,l);		 (* "a" *)
Substitution.get(5,l);       (* "e" *)
Substitution.get(5,l);		 (* "f" *)
Substitution.domain(l);		 (* [5,1,2,3,4] *)
Substitution.range(l);		 (* ["f","a","b","c","d"] *)

(* ----------------------------------------------------------------------------------- *)
(* TETS CASES FOR RECORD STRUCTURE *)

val r = [(Lab("a"),1),(Lab("b"),2),(Lab("c"),3)];
Record.getLabels(r); 		(* [a,b,c] *)
Record.getFields(r);		(* [1,2,3] *)
Record.access(r,Lab("a"));	(* 1 *)
Record.access(r,Lab("c"));	(* 3 *)

val r = [(Lab("d"),4),(Lab("b"),2),(Lab("e"),5),(Lab("c"),3),(Lab("a"),1)];
val s = [(Lab("b"),20),(Lab("d"),40),(Lab("c"),30),(Lab("a"),10),(Lab("e"),50)];
Record.sort(r); 	(* [(Lab "a", 1), (Lab "b", 2), (Lab "c", 3), (Lab "d", 4), (Lab "e", 5)] *)
Record.sort(s);		(* [(Lab "a", 10), (Lab "b", 20), (Lab "c", 30), (Lab "d", 40),(Lab "e", 50)] *)
Record.merge(r,s);	(* [(1, 10), (2, 20), (3, 30), (4, 40), (5, 50)] *)
Record.merge(s,r);	(* [(10, 1), (20, 2), (30, 3), (40, 4), (50, 5)] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR RESOLVE CHAIN *)

val a = (ValueHole(TypeVar("a")));
val b = (ValueHole(TypeVar("b")));
val c = (ValueHole(TypeVar("c")));
val d = (ValueHole(TypeVar("d")));
val e = (ValueHole(TypeVar("e")));
val f = (ValueHole(TypeVar("f")));
val g = (ValueHole(TypeVar("g")));

val sigma = [ (a,VHole(SimpleHole(b))), (b,VHole(SimpleHole(c))), (c,Concrete(N((3)))) ];

resolveChainSigma(VHole(SimpleHole(a)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(b)),sigma); (* 3 *)
resolveChainSigma(VHole(SimpleHole(c)),sigma); (* 3 *)

val sigma = [(a,VHole(BinaryOpHole(BoolOper(EQ),VHole(SimpleHole(b)),VHole(SimpleHole(c))))),
			 (b,VRecord([(Lab("1"),VHole(SimpleHole(d))),(Lab("2"),VHole(SimpleHole(e)))])),
			 (c,Concrete(N(3))),
			 (e,VRecord([]))];
			 
prettyPrintValue(resolveChainSigma(VHole(SimpleHole(a)),sigma)); 
(* v[ {1=v['d], 2=v{}} = 3 ] *)

val sigma = [ (a,VHole(SimpleHole(b))), 
			  (b,VRecord([(Lab("a"),VHole(SimpleHole(c))),(Lab("b"),VHole(SimpleHole(d)))])), 
			  (c,Concrete(N(3))), (d,Concrete(B(true))) ];

prettyPrintValue(resolveChainSigma(VHole(SimpleHole(a)),sigma)); 
(* {a=3, b=true} *)

val sigma = [(a,VHole(ConditionHole(VHole(SimpleHole(b)),
			    BoolExpr(LESS,Value(Concrete(N(3))),Value(VHole(SimpleHole(c)))),
				BoolExpr(MORE,Value(Concrete(N(10))),Value(VHole(SimpleHole(d))))))),
			 (b,VHole(CaseHole(VHole(SimpleHole(e)),
							   [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
							     BoolExpr(LESS,Variable(Var("x")),ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("y"))))),
							     (PVal(N(2)),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))]))),
			 (e,VRecord([(Lab("a"),Fun(Var("x"),Int,Value(VHole(SimpleHole(g))))),(Lab("b"),Concrete(N(2)))])),
			 (d,VHole(BinaryOpHole(ArithOper(SUBTRACT),VHole(SimpleHole(f)),Concrete(N(5))))),
			 (g,VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(B(true)))]))];
			 
prettyPrintValue(resolveChainSigma(VHole(SimpleHole(a)),sigma));
(* v[ if v[ case {a=fx x:int => {1=3, 2=true}, b=2} of {a=x, b=y} -> x < 2 * y | 2 -> x + x] 
      then 3 < v['c] 
	  else 10 > v[ v['f] - 5 ] ] *)

val a' = (TypeHole(TypeVar("a")));
val b' = (TypeHole(TypeVar("b")));
val c' = (TypeHole(TypeVar("c")));
val d' = (TypeHole(TypeVar("d")));
val e' = (TypeHole(TypeVar("e")));

val theta = [(a',TRecord([(Lab("yes"),Int),(Lab("no"),Real)])), (b',Int)];
prettyPrintTheta(theta);

prettyPrintType(resolveChainTheta(THole(b'),theta));

val theta = [(a',TRecord([(Lab("yes"),TRecord([(Lab("1"),THole(b')),(Lab("2"),Int)])),
						  (Lab("no"),TRecord([(Lab("a"),Real),
											   (Lab("b"),TRecord([(Lab("x"),Bool),(Lab("y"),THole(c'))]))]))])),
			 (b',TRecord([(Lab("one"),Real),(Lab("two"),THole(d'))])),
			 (c',TFun(Bool,THole(e'))),
			 (e',TFun(Int,TFun(TRecord([]),Int)))];
			 
prettyPrintType(resolveChainTheta(THole(a'),theta));
(* {yes:{1:{one:real, two:'d}, 2:int}, 
    no:{a:real, b:{x:bool, y:(bool -> (int -> ({}->int)))}}} *)
	
(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR LIST HELPERS *)
 
val l = [ (1,2), (3,4), (5,6)];
replace(l,1,5);	(* [(5, 2), (3, 4), (5, 6)] *)
replace(l,2,6); (* [(1, 6), (3, 4), (5, 6)] *)
replace(l,3,7); (* [(1, 2), (7, 4), (5, 6)] *)
replace(replace(replace(l,2,1),4,3),6,5); (* [(1, 1), (3, 3), (5, 5)] *)

append([],[1,2,3]);	     (* [1,2,3] *)
append([1,2,3],[]);		 (* [1,2,3] *)
append([1,2,3],[4,5,6]); (* [1,2,3,4,5,6] *)

union([],[1,2,3]);	     (* [1,2,3] *)
union([1,2,3],[1]);		 (* [2,3,1] *)
union([1,2,3],[1,2,3]);  (* [1,2,3] *)
union([1,2,3],[4,5,6]);  (* [1,2,3,4,5,6] *)
union([1,2,3],[4,5,6,1,2,3]);  (* [4,5,6,1,2,3] *)

remove([1,2,3],[1]);		 	 (* [2,3]	  *)
remove([1,1,1],[1]);		 	 (* []        *)
remove([1,2,3],[4]);		 	 (* [1,2,3]	  *)
remove([1,2,3,1,2,3],[1]);   	 (* [2,3,2,3] *)
remove([1,2,3,4],[1,5]);	 	 (* [2,3,4]   *)
remove([1,2,3,4],[1,2,3]);	 	 (* [4] 	  *)
remove([1,2],[5]);			 	 (* [1,2]  	  *)
remove([1,2,3,1,2,3],[1,2]); 	 (* [3,3] 	  *)
remove([1,2,3,1,2,3],[1,2,3,4]); (* [] 		  *)

element([1,2,3],1);		 (* true *)
element([],1);			 (* false *)
element([1],1);			 (* true *)
element([1],2);			 (* false *)
element([1,2,3,4,5],6);	 (* false *)

listElement([1,2,3],[0,1]);					(* [1]   *)
listElement([1,2,3],[0,4]);					(* []    *)
listElement([1,2,3],[1,2]); 				(* [1,2] *)
listElement([0,1,2,3,4,5,6,7,8,9,10],[10]);	(* [10]  *)
listElement([0,1,2,3,4,5,6,7,8,9,10],[11,12,13,14,15,16,17,18,19,20]); (* [] *)

allElementsEqual([1,1,1,1,1]);	(* true *)
allElementsEqual([1,1,1,1,2]);	(* false *)
allElementsEqual([1,1]);		(* true *)
allElementsEqual([true,true,true,true]);				(* true *)
allElementsEqual([false,true,true,true]);				(* false *)
allElementsEqual([true,true,false,true,true]);			(* false *)
allElementsEqual([9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]);	(* true *)
allElementsEqual([9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,3]);	(* false *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR FREE VARIABLES *)

fv[Value(Concrete(N(3)))]; 	(* [] *)
fv[Value(VRecord([]))];		(* [] *)

fv[Value(VRecord([(Lab("1"),Concrete(N(2))),
				  (Lab("2"),Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))),
				  (Lab("3"),Concrete(R(2.0)))]))];
(* [] *)
				  
fv[Variable(Var("x"))]; (* [ x ] *)
fv[ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(3))))]; 	(* [ ]   *)
fv[BoolExpr(EQ,Variable(Var("y")),Value(Concrete(N(3))))]; 			(* [ y ] *)

fv[ArithExpr(TIMES,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
					   ArithExpr(PLUS,Variable(Var("a")),Variable(Var("b"))))]; 
(* [ x y a b ] *)
					   
fv[ArithExpr(TIMES,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),
					   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))]; 
(* [ x ] *)

fv[Case(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(3)))])),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),Value(Concrete(B(true)))),
			   (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(N(2)))]),Value(Concrete(B(false))))])]; 
(* [ ] *)

fv[Case(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(3)))])),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),Variable(Var("x"))),
			   (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])];
(* [ ] *)

fv[Case(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(3)))])),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),Variable(Var("z"))),
			   (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),Variable(Var("a")))])]; 
(* [ z, a ] *)

fv[Case(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(3)))])),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("e")))]),ArithExpr(PLUS,Variable(Var("z")),Variable(Var("a")))),
			   (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),ArithExpr(PLUS,Variable(Var("b")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("x")))))])]; 
(* [ a, z, b ] *)

fv[Case(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(3)))])),
			  [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("e")))]),
			    ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])]; 
(* [ ] *)
			
fv[Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x")))))]; (* [ ]   *)
fv[Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))))];	(* [ y ] *)

fv[Value(Fun(Var("x"),Int,Value(Fun(Var("y"),Int,
	     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))))))]; (* [ ] *)
		
fv[Value(Fun(Var("x"),Int,Value(Fun(Var("y"),Int,
	     ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("z")),Variable(Var("y"))))))))]; (* [ z ] *)
	
val va' = VHole(SimpleHole(ValueHole(TypeVar("a"))));
val vb' = VHole(SimpleHole(ValueHole(TypeVar("b"))));
val vc' = VHole(SimpleHole(ValueHole(TypeVar("c"))));
val vd' = VHole(SimpleHole(ValueHole(TypeVar("d"))));
val ve' = VHole(SimpleHole(ValueHole(TypeVar("e"))));
val vf' = VHole(SimpleHole(ValueHole(TypeVar("f"))));
	
fv[Value(va')]; (* [ ] *)
fv[Value(VRecord([(Lab("1"),va'),(Lab("2"),vb')]))]; (* [ ] *)
fv[Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4)))]))]; (* [ ] *)
fv[Value(VHole(BinaryOpHole(ArithOper(PLUS),va',vb')))]; (* [] *)
fv[Value(VHole(RecordHole([(Lab("a"),Concrete(N(3))),(Lab("b"),va'),(Lab("c"),vc')])))]; (* [] *)

fv[Value(VHole(CaseHole(
	VHole(CaseHole(va',
				   [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z")))),
					(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("z")))]),ArithExpr(TIMES,Variable(Var("z")),Variable(Var("a"))))])),
	[(PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b")))]),
	  Record([(Lab("i"),Record([(Lab("one"),Variable(Var("a"))),(Lab("two"),Variable(Var("c")))])),
		    (Lab("j"),Record([(Lab("10"),Variable(Var("d"))),(Lab("20"),Variable(Var("d")))]))])),
	 (PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b")))]),
	  ArithExpr(TIMES,ArithExpr(PLUS,Variable(Var("a")),Variable(Var("b"))),Variable(Var("f"))))])))];
(* [ z, c, d, a, f] *)

fv[
	Value(VHole(ConditionHole(
		VHole(CaseHole(va',[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),Value(va'))])),
		Value(Concrete(N(3))),
		Value(Concrete(N(4)))))),

	
	App(Value(Fun(Var("x"),Int,
			  Condition(Value(Concrete(B(true))),
					    Record([(Lab("i"),Value(Concrete(N(2)))),
							    (Lab("j"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))))]),
					    Record([(Lab("i"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("y")),Value(Concrete(N(2))))))),
						    	(Lab("j"),Record([(Lab("1"),Variable(Var("a"))),(Lab("2"),Variable(Var("b")))]))])))),
		Variable(Var("z")))];
(* [ z, a, y, b] *)

fv[Let(Var("x"),Int,ArithExpr(PLUS,Variable(Var("a")),Value(Concrete(N(10)))),
	   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))];
(* [a] *)

fv[Let(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("z"))),
	   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))];
(* [x,z] *)

fv[Let(Var("x"),Int,Let(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("a")),Variable(Var("b")))),Variable(Var("x"))),
   Let(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("c"))),
	   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("d")))))];
(* [a,b,c,d,x] *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR SUBSTITUTE AND ALPHAVARIANT *)

val sub = [ (Var("x"),Value(Concrete(N(3)))) ];

fun prettyPrintE(e) = prettyPrintExpression(Expression(e));

prettyPrintE(substitute(Value(Concrete(N(4))),sub)); 	(* 4  *)
prettyPrintE(substitute(Value(VRecord([])),sub));		(* {} *)
prettyPrintE(substitute(Variable(Var("x")),sub)); 	 	(* 3  *)
prettyPrintE(substitute(Variable(Var("y")),sub)); 	 	(* y  *)
prettyPrintE(substitute(ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(4)))),sub)); 	(* 3 + 4 *)
prettyPrintE(substitute(ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))),sub)); 	   	(* 3 + 3 *)
prettyPrintE(substitute(Condition(Value(Concrete(B(true))),Variable(Var("x")),
								 ArithExpr(PLUS,Variable(Var("x")),Value(Concrete(N(1))))),sub)); 
(* if true then 3 else 3 + 1 *)

prettyPrintE(substitute(Case(
			Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4)))])),
			[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
			  ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]),
sub)); 
(* case {1=3,2=4} of {1=x,2=y} -> x+y+z 
   =>
   case {1=3,2=4} of {1=x0,2=y} -> x0+y+z *)
   
prettyPrintE(substitute(Case(
			Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4)))])),
			[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
			  ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z"))))),
			 (PRecord([(Lab("1"),PVar(Var("y"))),(Lab("2"),PVar(Var("z")))]),
			  ArithExpr(PLUS,Variable(Var("y")),ArithExpr(PLUS,Variable(Var("z")),Variable(Var("x")))))]),
sub)); 
(* (case {1=3,2=4} of {1=x,2=y} -> x+y+z 
				    | {1=y,2=z} -> y+z+x ) [3/x]
   =>
   case {1=3,2=4} of {1=x1,2=y} -> x1+y+z
				   | {1=y,2=z}  -> y+z+3 *)
	
prettyPrintE(substitute(Case(
			Record([(Lab("1"),Value(Concrete(N(3)))),(Lab("2"),Variable(Var("x")))]),
			[(PRecord([(Lab("1"),PVar(Var("y"))),(Lab("2"),PVar(Var("z")))]),
			 ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]),
			
sub)); 
(* case {1=3,2=3} of {1=y,2=z} -> 3+y+z *)

prettyPrintE(substitute(Case(
	Variable(Var("x")),
	[(PRecord([(Lab("1"),PVar(Var("x")))]),
	  Case(Variable(Var("x")),
		   [(PRecord([(Lab("i"),PVar(Var("y")))]),Variable(Var("x"))),
		    (PRecord([(Lab("j"),PVar(Var("x")))]),Variable(Var("x")))])),
	 (PRecord([(Lab("2"),PVar(Var("y")))]),Variable(Var("x")))]),sub));
(* case 3 of {1=x2}->case x2 of {i=y} -> x2 | {j=x2}->x2
		   | {2=y} -> 3 *)
	
val sub = [ (Var("x"),Value(Concrete(N(3)))), (Var("y"),Value(Concrete(N(4)))) ];

prettyPrintE(substitute(Case(
			Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(4)))])),
			[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("z")))]),
			  ArithExpr(SUBTRACT,Variable(Var("y")),ArithExpr(DIVIDE,Variable(Var("x")),Variable(Var("z"))))),
			 (PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b")))]),
			  ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("a")),Variable(Var("x"))),Variable(Var("y")))),
			 (PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
			  ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),Variable(Var("z"))))
			]),
sub));
(* (case {1=3,2=4} of {1=x,2=z} -> y+x+z
				    | {1=a,2=b} -> a+x+y
				    | {1=x,2=y} -> x+y+z)[3/x,4/y]
=>
   case {1=3,2=4} of {1=x3,2=z}  -> 4-(x3/z)
				   | {1=a,2=b}   -> a+3+4
				   | {1=x4,2=y4} -> x4+y4+z *)

prettyPrintE(substitute(ArithExpr(PLUS,
	ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
	Case(Value(VRecord([(Lab("1"),Concrete(N(1))),(Lab("2"),Concrete(N(2)))])),
		 [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
		   ArithExpr(SUBTRACT,
			ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))),
			Case(Value(VRecord([(Lab("1"),Concrete(N(1))),(Lab("2"),Concrete(N(2))),(Lab("3"),Concrete(N(3)))])),
				 [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y"))),(Lab("3"),PVar(Var("z")))]),
				   ArithExpr(SUBTRACT,
						ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))),
						Case(Value(VRecord([(Lab("1"),Concrete(N(1))),(Lab("2"),Concrete(N(2)))])),
							 [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
							   BoolExpr(EQ,Variable(Var("x")),Variable(Var("y"))))])))]))),
			(PRecord([(Lab("3"),PVar(Var("a"))),(Lab("4"),PVar(Var("y")))]),
			 ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("a")),Variable(Var("y")))))])),sub));
(* 3*4 + case {1=1,2=2} of {1=x5,2=y5} -> x5*y5 - case {1=1,2=2,3=3} of {1=x5,2=y5,3=z} -> x5*x5 - case {1=1,2=2} of {1=x5,2=y5} -> x5=y5
						 | {3=a,4=y6} -> 3+a+y6	 *)
						 
val sub2 = [ (Var("x"),Value(Concrete(N(3)))), (Var("y"),Value(Concrete(N(4)))), (Var("z"),Value(Concrete(N(5)))) ];

prettyPrintE(substitute(Case(
	Case(Record([(Lab("a"),Variable(Var("x"))),(Lab("b"),Variable(Var("y")))]),
		 [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
		   Record([(Lab("1"),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),
				   (Lab("2"),ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))),
				   (Lab("3"),ArithExpr(TIMES,Variable(Var("z")),Variable(Var("z"))))]))]),
	[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y"))),(Lab("3"),PVar(Var("z")))]),
	  ArithExpr(SUBTRACT,Variable(Var("x")),ArithExpr(SUBTRACT,Variable(Var("y")),Variable(Var("z")))))]),sub2));
						
(* (case (case {a=x,b=y} of {a=x,b=y} -> {1=x+y,2=x*y,3=z*z}) of {1=x,2=y,3=z} -> x-z-y)[3/x,4/y,5/z]
   => 
   case (case {a=3,b=4} of {a=x7,b=y7}-> {1=x7+y7,2=x7*y7,3=5*5}) of {1=x8,2=y8,3=z8} -> x8-y8-z8
*)

prettyPrintE(substitute(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Value(Concrete(N(2))),Variable(Var("x"))))),sub));
(* fn x5:int => 2*x5 *)

prettyPrintE(substitute(Value(Fun(Var("x"),Int,
	Value(Fun(Var("y"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))))),sub));
(* fn x6:int => fn y7:int => x6+y7 *)

prettyPrintE(substitute(Value(Fun(Var("a"),Int,
	ArithExpr(PLUS,
		Variable(Var("x")),
		Value(Fun(Var("b"),Int,
			ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("y")),Variable(Var("a"))),Variable(Var("b")))))))),sub));
(* fn a:int => 3 + fn b:int => 4 + a +b *)

prettyPrintE(substitute(App(
	Value(Fun(Var("a"),Int,
		ArithExpr(PLUS,
			Variable(Var("x")),
			Value(Fun(Var("b"),Int,
				ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("y")),Variable(Var("a"))),Variable(Var("b")))))))),
	ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y")))),sub));
(* (fn a:int => 3 + fn b:int => 4 + a +b)(3+4) *)

prettyPrintE(substitute(Record([
	(Lab("a"),Variable(Var("x"))),
	(Lab("c"),App(Value(Fun(Var("x"),Int,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))),Variable(Var("x")))),
	(Lab("d"),Case(Record([(Lab("1"),Variable(Var("x"))),(Lab("2"),Variable(Var("z"))),(Lab("3"),Value(Concrete(R(2.0))))]),
				   [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y"))),(Lab("3"),PVar(Var("z")))]),
				     BoolExpr(LESS,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),Variable(Var("z")))),
					(PRecord([(Lab("4"),PVar(Var("x"))),(Lab("5"),PVar(Var("a"))),(Lab("6"),PVar(Var("b")))]),
					 BoolExpr(LESS,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("a"))),Variable(Var("y"))))]))]),sub));
(* ( {a = x,
	  c = (fn x:int => x+y) x,
	  d = case {1=x,2=z,3=2.0} of {1=x,2=y,3=z} -> x+y < z
								| {4=x,5=a,y=b} -> x+a < y) [3/x, 4/y]
	=>
	 {a = 3,
	  c = (fn x8:int => x8+4) 3,
	  d = case {1=3,2=z,3=2.0} of {1=x9,2=y9,3=z} -> x9+y9 < z
								| {4=x9,5=a,y=b}  -> x9+a  < 4} *)
	  
prettyPrintE(substitute(Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(R(3.0)))])),[])); 
(* {a=3,b=3.0}) *)				   

val va' = VHole(SimpleHole(ValueHole(TypeVar("a"))));
val vb' = VHole(SimpleHole(ValueHole(TypeVar("b"))));
val vc' = VHole(SimpleHole(ValueHole(TypeVar("c"))));
val vd' = VHole(SimpleHole(ValueHole(TypeVar("d"))));
val ve' = VHole(SimpleHole(ValueHole(TypeVar("e"))));
val vf' = VHole(SimpleHole(ValueHole(TypeVar("f"))));

val va'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("a"))));
val vb'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("b"))));

val va''' = VHole(SimpleHole(ValueHole(ArithTypeVar("a"))));
val vb''' = VHole(SimpleHole(ValueHole(ArithTypeVar("b"))));

prettyPrintE(substitute(Value(va'),[])); (* v['a] *)

prettyPrintE(substitute(Value(VRecord([(Lab("a"),va'),(Lab("b"),vb')])),[])); (* {a=v['a],b=v['b]} *)

prettyPrintE(substitute(Value(VHole(BinaryOpHole(ArithOper(PLUS),va',vb'))),[])); (* v['a+'b] *)

prettyPrintE(substitute(ArithExpr(PLUS,Value(va'),Value(vb')),[])); (* v['a]+v['b] *)

prettyPrintE(substitute(Value(VHole(ConditionHole(va',
		   ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),
		   BoolExpr(EQ,
					BoolExpr(LESS,Variable(Var("x")),Variable(Var("y"))),
					BoolExpr(MORE,Variable(Var("x")),Variable(Var("y"))))))),sub));
(* v[if 'a then 3+4 else (3<4)=(3>4)] *)

prettyPrintE(substitute(Value(VHole(CaseHole(va',
		   [(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("z")))]),
		     ArithExpr(PLUS,Variable(Var("x")),
					   ArithExpr(PLUS,Variable(Var("y")),Variable(Var("z")))))]))),sub));
(* v[case 'a of {1=x10,2=z} -> x10+4+z] *)

prettyPrintE(substitute(Value(VHole(CaseHole(
	VHole(CaseHole(va',[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))])),
	[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
	  ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))))]))),sub));
(* v[case (case 'a of {1=x,2=y}->x+y) of {1=x,2=y} -> x+y]
   =>
   v[case (case 'a of {1=x12,2=y12} -> x12+y12) of {1=x11,b=11} -> z11+y11]
*)   

prettyPrintE(substitute(Value(VHole(RecordHole([
	(Lab("a"),va'),
	(Lab("b"),VHole(BinaryOpHole(ArithOper(DIVIDE),va',vb')))]))),sub));
(* v[ {a='a, b='a/'b} ] *)

val sub = [ (Var("x"),Value(Concrete(N(3)))), (Var("y"),Value(Concrete(N(4)))),
			(Var("z"),Value(Concrete(N(5)))), (Var("t"),Value(Concrete(N(6)))) ];
			
prettyPrintE(substitute(
	Case(Record([(Lab("a"),BoolExpr(EQ,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("t"))),
									   ArithExpr(TIMES,Variable(Var("y")),Variable(Var("f"))))),
				 (Lab("b"),BoolExpr(LESS,Variable(Var("z")),Variable(Var("e"))))]),
		 [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVal(B(true)))]),ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("y")),Variable(Var("f"))))),
		  (PRecord([(Lab("a"),PVal(B(true))),(Lab("b"),PVar(Var("z")))]),ArithExpr(PLUS,Variable(Var("z")),ArithExpr(PLUS,Variable(Var("x")),Variable(Var("t"))))),
		  (PRecord([(Lab("a"),PVal(B(false))),(Lab("b"),PVar(Var("f")))]),ArithExpr(TIMES,Variable(Var("f")),Variable(Var("f")))),
		  (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),ArithExpr(TIMES,Variable(Var("x")),ArithExpr(TIMES,Variable(Var("y")),Variable(Var("t")))))]),sub));
	
		 
(* case { a = (x+t)=(y*f), b = z<e } of {a=x,b=true}  -> x+y+f
									  | {a=true,b=z}  -> z+x+t
									  | {a=false,b=f} -> f*f
									  | {a=x,b=y}     -> x*y*t
   =>
   case { a = (3+6)=(4*f), b = 5<e } of {a=x0,b=true} -> x0+4+f
									  | {a=true,b=z0} -> z0+3+6
									  | {a=false,b=f} -> f*f
									  | {a=x0,b=y0}   -> x0*y0*6 *)
									  
prettyPrintE(substitute(Value(Fun(Var("x"),Int,Case(
	Variable(Var("x")),
	[(PRecord([(Lab("1"),PVar(Var("x")))]),
	  Case(ArithExpr(TIMES,Variable(Var("x")),Variable(Var("t"))),
		   [(PRecord([(Lab("i"),PVar(Var("y")))]),Variable(Var("x"))),
		    (PRecord([(Lab("j"),PVar(Var("x")))]),Variable(Var("x"))),
			(PRecord([(Lab("k"),PVar(Var("t")))]),ArithExpr(TIMES,Variable(Var("z")),Variable(Var("t"))))])),
	 (PRecord([(Lab("2"),PVar(Var("y")))]),Variable(Var("x"))),
	 (PRecord([(Lab("3"),PVar(Var("a")))]),ArithExpr(TIMES,Variable(Var("t")),Variable(Var("z"))))]))),sub));
(* fn x:int => case x of {1=x} -> case x*t of {i=y} -> x | {j=x} -> x | {k=t} -> z*t
					   | {2=y} -> x
					   | {3=a} -> t * z
   =>
   fn x21:int => case x21 of {1=x21} -> case x21*6 of {i=y22}->x21 | {j=x21}->x21 | {k=t23}->5*t23
						   | {2=y24} -> x21
						   | {3=a} -> 6*5
*)

prettyPrintE(substitute(Let(Var("x"),Int,
	ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),Variable(Var("z"))),
	ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("t")),Variable(Var("a"))))),sub));
(* let x25:int = 3+4+5 in x25+6+a *)

prettyPrintE(substitute(Let(Var("x"),Int,
	Let(Var("y"),Int,ArithExpr(PLUS,ArithExpr(PLUS,Variable(Var("x")),Variable(Var("y"))),Variable(Var("z"))),
					 ArithExpr(PLUS,Variable(Var("y")),Variable(Var("x")))),
	Let(Var("z"),Int,ArithExpr(PLUS,Variable(Var("x")),ArithExpr(PLUS,Variable(Var("t")),Variable(Var("a")))),
					 ArithExpr(PLUS,Variable(Var("z")),Variable(Var("t"))))),sub));
(* let x26:int = (let y27:int=3+4+5 in y27+3) in (let z28:int = x26+6+a in z28+6) *)

(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR GEN *)

prettyPrintValue(gen(Bool,[])); (* true *)
prettyPrintValue(gen(Int,[]));  (* 1    *)
prettyPrintValue(gen(Real,[])); (* 1.0  *)
prettyPrintValue(gen(TRecord([(Lab("a"),Int),(Lab("b"),Int)]),[])); (* {a=1,b=1} *)
prettyPrintValue(gen(TRecord([]),[])); (* {} *)

prettyPrintValue(gen(TRecord([
	(Lab("one"),TRecord([(Lab("a"),Int),(Lab("b"),Real)])),
	(Lab("two"),TRecord([(Lab("a"),Bool),(Lab("b"),Int)]))]),[])); 
(* {one={a=1,b=1.0},two={a=true,b=1}} *)

prettyPrintValue(gen(THole(TypeHole(TypeVar("a"))),[])); (* v['a] *)
prettyPrintValue(gen(THole(TypeHole(TypeVar("a"))),[ (TypeHole(TypeVar("a")),Int) ])); (* 1 *)
prettyPrintValue(gen(THole(TypeHole(TypeVar("a"))),
	[(TypeHole(TypeVar("a")),TRecord([(Lab("a"),THole(TypeHole(TypeVar("b")))),
									  (Lab("b"),THole(TypeHole(ArithTypeVar("a"))))])),
	 (TypeHole(TypeVar("b")),Int),
	 (TypeHole(ArithTypeVar("a")),TRecord([(Lab("one"),Int),(Lab("two"),Int)]))]));
(* {a=1, b={one=1,two=1}} *)

prettyPrintValue(gen(TFun(Int,Int),[]));   (* fn x:int => 1 *)
prettyPrintValue(gen(TFun(Bool,Real),[])); (* fn x:bool => 1.0 *)

prettyPrintValue(gen(TFun(
	TRecord([(Lab("a"),Int),(Lab("b"),Real),(Lab("c"),TFun(Int,Int))]),
	TRecord([(Lab("i"),TRecord([(Lab("a"),Bool),(Lab("b"),Bool)])),
			 (Lab("j"),TRecord([(Lab("a"),TFun(Int,Bool)),(Lab("b"),TFun(THole(TypeHole(TypeVar("a"))),Int))]))])),[]));
(* fn x:{a:int,b:real,c:(int->int)} =>
        {i={a=true,b=true}, j={a=fn x:int=>true,b=fn x:'a=>1}} *)

prettyPrintValue(gen(TFun(TFun(Int,Int),TFun(Bool,Bool)),[]));
(* fn x:(int->int) => fn x:bool => true *)

prettyPrintValue(gen(TFun(TFun(
	TFun(Int,Int),TFun(Real,Real)),
	TFun(TRecord([(Lab("i"),Int),
				  (Lab("j"),TRecord([(Lab("1"),Real),(Lab("2"),Bool),(Lab("3"),Real)]))]),
		 TRecord([(Lab("x"),Bool),(Lab("y"),Bool)]))),[]));
(* fn x: (int->int -> real->real) => 
	      fn x:{i:int,j:{1:real,2:bool,3:real}} => {x=true,y=true} *)

prettyPrintValue(gen(TFun(TFun(THole(TypeHole(TypeVar("a"))),THole(TypeHole(TypeVar("b")))),
						  TFun(TFun(Bool,Bool),THole(TypeHole(EqualityTypeVar("a"))))),[]));
(* fn x: ('a -> 'b) => fn x : (bool -> bool) => v[''a] *)

(* ----------------------------------------------------------------------------------- *)
(* TETS CASES FOR UNIFY *)

val a' = THole(TypeHole(TypeVar("a")));
val a'1 = TypeHole(TypeVar("a"));
val b' = THole(TypeHole(TypeVar("b")));
val b'1 = TypeHole(TypeVar("b"));
val c' = THole(TypeHole(TypeVar("c")));
val c'1 = TypeHole(TypeVar("c"));
val d' = THole(TypeHole(TypeVar("d")));
val e' = THole(TypeHole(TypeVar("e")));

val a'' = THole(TypeHole(EqualityTypeVar("a")));
val b'' = THole(TypeHole(EqualityTypeVar("b")));
val b''1 = TypeHole(EqualityTypeVar("b"));
val c'' = THole(TypeHole(EqualityTypeVar("c")));
val d'' = THole(TypeHole(EqualityTypeVar("d")));

val a''' = THole(TypeHole(ArithTypeVar("a")));
val b''' = THole(TypeHole(ArithTypeVar("b")));
val c''' = THole(TypeHole(ArithTypeVar("c")));
val d''' = THole(TypeHole(ArithTypeVar("d")));
val e''' = THole(TypeHole(ArithTypeVar("e")));

fun unifyTest(a,b) = (case unify(a,b) of

	  NONE => "FAIL"
	| SOME(theta) => prettyPrintTheta(theta));

unifyTest( [Int,Int], []);		(* [] *)
unifyTest( [a',a'], []);		(* [] *)	
unifyTest( [a',Int], []);		(* [ ('a -> Int) ] *)
unifyTest( [a''',Real], []);	(* [ ('''a -> Real) ] *)
unifyTest( [a''',Bool], []);	(* FAIL *)
unifyTest( [a'',Real], []);		(* FAIL *)
unifyTest( [a',b'], []);		(* [ ('a -> 'b) ] *)
unifyTest( [a''',b'''], []);	(* [ ('''a -> '''b) ] *)
unifyTest( [a'',b'''], []);		(* [ (''a -> Int), ('''b -> Int) ] *)
unifyTest( [a',b',c'], []); 	(* [ ('a -> 'c), ('b -> 'c) ] *)
unifyTest( [a',b',Int], []); 	(* [ ('a -> Int), ('b -> Int)] *)
unifyTest( [a',b'',c'''],[]);	(* [ ('a -> Int), (''b -> Int), ('''c -> Int) ] *)
unifyTest( [Int,Int,Int],[]);	(* [] *)
unifyTest( [Int,Int,Real],[]);	(* FAIL *)
unifyTest( [Real,Real,Real],[]);(* [] *)
unifyTest( [a',Int], [(a'1,Int)]);		(* [ ('a -> Int) ] *)
unifyTest( [a',Int], [(a'1,Real)]);  	(* FAIL *)
unifyTest( [a',b',c'], [(a'1,Int)]);	(* [ ('b -> Int), ('c -> Int), ('a -> Int) ] *)
unifyTest( [a',b'], [(a'1,c')]); 		(* [ 'a -> 'c, 'c -> 'b ] *)
unifyTest( [a',b',c'], [(a'1,Int),(b'1,Real)]);  	(* FAIL *)
unifyTest( [a',b'',c'''], [(a'1,Int)]);				(* [ (''b -> Int), ('''c -> Int), ('a -> Int) ] *)
unifyTest( [a',b'',c'''], [(a'1,Int), (b''1,Int)]); (* [ ('a -> Int), (''b -> Int), ('''c -> Int) ] *)
unifyTest( [a',b'',c'''], [(a'1,Real)]); 			(* FAIL *)

unifyTest( [TFun(Int,Int),TFun(Int,Int)], [] ); (* [ ] *)
unifyTest( [TFun(Real,Real),TFun(Real,Int)], []); (* FAIL *)
unifyTest( [TFun(TFun(Int,Int),TFun(Int,Int)),TFun(TFun(Int,Int),TFun(Int,Int))],[]); (* [ ] *)
unifyTest( [TFun(TFun(Int,Int),TFun(Int,Int)),TFun(TFun(Int,Int),TFun(Bool,Int))],[]); (* FAIL *)

unifyTest( [TRecord([]),TRecord([])], []); 				(* [] *)
unifyTest( [TRecord([]),TRecord([]), TRecord([])], []);	(* [] *)
unifyTest( [Bool,TRecord([])],[]);						(* FAIL *)
unifyTest( [TFun(TRecord([]),TRecord([])),TFun(TRecord([]),TRecord([]))], [] ); (* [] *)
unifyTest( [a',TRecord([])], []);		(* ['a->{}] *)
unifyTest( [a',b',TRecord([])], []);	(* ['a->{},'b->{}] *)

unifyTest( [a', TRecord([(Lab("1"),TRecord([])),(Lab("2"),TRecord([]))])],[]);
(* ['a14 -> {}, 'a13 -> {}, 'a -> {1:'a13, 2:'a14}] *)

unifyTest( [TFun(Int,a'), TFun(b''',Real) ], [] ); (* [ 'a->Real, '''b->Int ] *)

unifyTest( [TFun(TFun(a''',b''),TFun(Int,Bool)),TFun(TFun(a'',b'),TFun(c''',Bool))],[]);
(* [ '''a->Int, ''a->Int, 'b->''b, '''c->Int ] *)

unifyTest( [TFun(Int,Int), TFun(a''',a''), a'], []);
(* [ '''a->int, ''a->int, 'a->('a13->'a14), 'a13->int, 'a14->int ] *)

unifyTest( [TFun(Int,Int), TFun(a''',a''), a''], []); (* FAIL *)

unifyTest( [TFun(Int,Int), TFun(a''',a''), a'''], []); (* FAIL *)

unifyTest( [TRecord([(Lab("a"),Int),(Lab("b"),Int)]),
			TRecord([(Lab("a"),Int),(Lab("b"),Int)])], []);  	
(* [] *)

unifyTest( [TRecord([(Lab("a"),Int)]),
			TRecord([(Lab("b"),Int)])], []);  	
(* FAIL *)

unifyTest( [TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),Real),(Lab("d"),TFun(Int,Real))]),
			TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),Real),(Lab("d"),TFun(Int,Real))])], []);  	
(* [] *)

unifyTest( [TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),Real),(Lab("d"),TFun(Int,Real))]),
			TRecord([(Lab("d"),TFun(Int,Real)),(Lab("c"),Real),(Lab("b"),Bool),(Lab("a"),Int)])], []);
(* [] *)

unifyTest( [TRecord([(Lab("a"),Int),(Lab("b"),b''),(Lab("c"),Real),(Lab("d"),TFun(d''',Real)),
					 (Lab("e"),a'),(Lab("f"),c'''),(Lab("g"),TFun(Int,d')),
					 (Lab("h"),TRecord([(Lab("i"),a'''),(Lab("j"),b'')]))]),
			TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),e'''),(Lab("d"),TFun(Int,Real)),
					 (Lab("e"),TFun(Bool,Bool)),(Lab("f"),c''),(Lab("g"),TFun(Int,TFun(Int,Int))),
					 (Lab("h"),TRecord([(Lab("i"),Real),(Lab("j"),Bool)]))]),
			e'], []);
(* ['''e -> real, '''d -> int, 'a15 -> bool, 'a16 -> bool, '''c -> int, ''c -> int, 'a17 -> int, 'a18 -> int, '''a -> real, ''b -> bool, 'a27 -> '''d, 'a28 -> real, 'a29 -> int, 'a31 -> '''a, 'a32 -> ''b, 'a19 -> int, 'a20 -> bool, 'a21 -> '''e, 'a33 -> int, 'a34 -> real, 'a35 -> bool, 'a36 -> bool, 
    'a24 -> ''c, 'a37 -> int, 'a39 -> int, 'a40 -> int, 'a41 -> real, 'a42 -> bool, 'a38 -> ('a39 -> 'a40), 'a23 -> ('a35 -> 'a36), 'a26 -> {i:'a31, j:'a32}, 'a25 -> ('a29 -> 'a30), 'a22 -> ('a27 -> 'a28), 'e -> {a:'a19, b:'a20, c:'a21, d:'a22, e:'a23, f:'a24, g:'a25, h:'a26}, 'd -> ('a17 -> 'a18), 'a -> ('a15 -> 'a16)] *)
			
unifyTest( [a',TRecord([(Lab("i"),Real),(Lab("j"),Real)])], []);
(* [ 'a43 -> real, 'a44 -> real, 'a -> {i:'a43, j:'a44} ] *)

unifyTest( [a',
			TRecord([(Lab("i"),a'),(Lab("j"),Int)]),
			TRecord([(Lab("i"),Real),(Lab("j"),b')])], []);				
(* FAIL *)

unifyTest( [a',
			TRecord([(Lab("i"),c'),(Lab("j"),Int)]),
			TRecord([(Lab("i"),Real),(Lab("j"),b')])], []);	
(* [ 'a45 -> real, 'a46 -> int, 'c -> real, 'b -> int, 'a -> {i:'a45, j:'a46} ] *)

unifyTest( [a',
			TRecord([(Lab("i"),c'),(Lab("j"),Int)]),
			TRecord([(Lab("i"),Real),(Lab("j"),b'),(Lab("k"),Int)])], []);	
(* FAIL *)

unifyTest( [a',TRecord([
				(Lab("1"),TRecord([(Lab("i"),c''')])),
				(Lab("2"),TRecord([(Lab("i"),Bool),(Lab("j"),b')])),
				(Lab("3"),TRecord([(Lab("i"),TFun(Int,Int))]))])], []); 
(* 'a52 -> '''c, 'a53 -> bool, 'a54 -> 'b, 'a56 -> int, 'a57 -> int, 'a55 -> ('a56 -> 'a57),
   'a51 -> {i:'a55}, 'a50 -> {i:'a53, j:'a54}, 'a49 -> {i:'a52}, 'a -> {1:'a49, 2:'a50, 3:'a51} *)

unifyTest( [a',
			TRecord([(Lab("1"),TRecord([(Lab("i"),c''')])),
					 (Lab("2"),TRecord([(Lab("i"),Bool),(Lab("j"),b')]))]),
			TRecord([(Lab("1"),TRecord([(Lab("i"),a'')])),
					 (Lab("2"),TRecord([(Lab("i"),Bool),(Lab("j"),d')]))])],[]);
(* [ 'a60 -> int, 'a61 -> bool, 'a62 -> 'd, 'a63 -> int, 'a64 -> bool, 'a65 -> 'd, '''c -> int, 
    ''a -> int, 'b -> 'd, 'a59 -> {i:'a61, j:'a62}, 'a58 -> {i:'a60}, 'a -> {1:'a58, 2:'a59} ] *)
	
unifyTest( [a',TRecord([(Lab("i"),Int),(Lab("j"),Real)])], [(a'1,Int)]); 	 	 
(* FAIL *)

unifyTest( [a',TRecord([(Lab("i"),b'),(Lab("j"),c')])], [(b'1,Int),(c'1,Int)]);
(* 'a66 -> int, 'a67 -> int, 'a -> {i:'a66, j:'a67}, 'b -> int, 'c -> int *)

unifyTest( [a'',TRecord([(Lab("i"),Int),(Lab("j"),Bool)])], []);
(* [''a68 -> int, ''a69 -> bool, ''a -> {i:''a68, j:''a69}] *)

unifyTest( [a'',TRecord([(Lab("i"),Int),(Lab("j"),Bool),(Lab("k"),Bool)])], []);
(* [''a70 -> int, ''a71 -> bool, ''a72 -> bool, ''a -> {i:''a70, j:''a71, k:''a72}] *)

unifyTest( [a'',TRecord([(Lab("i"),Int),(Lab("j"),Bool),(Lab("k"),Bool),
						 (Lab("l"),TRecord([(Lab("1"),Bool),(Lab("2"),Int)]))])], []);
(* ''a73 -> int, ''a74 -> bool, ''a75 -> bool, ''a77 -> bool, ''a78 -> int, 
   ''a76 -> {1:''a77, 2:''a78}, ''a -> {i:''a73, j:''a74, k:''a75, l:''a76} *)
						 
unifyTest( [a'',TRecord([(Lab("i"),Int),(Lab("j"),Bool),(Lab("k"),Bool),
						 (Lab("l"),TRecord([(Lab("1"),Bool),(Lab("2"),Real)]))])], []); 
(* FAIL *)

unifyTest( [a'',TRecord([(Lab("i"),Int),(Lab("j"),b'')]),TRecord([(Lab("i"),b'''),(Lab("j"),Bool)])], []);
(* [ ''a85 -> int, ''a86 -> bool, '''b -> int, ''b -> bool, ''a -> {i:''a85, j:''a86} ]*)

unifyTest( [a''',TRecord([(Lab("i"),Int)])], []);
(* FAIL *)

unifyTest( [a'',TRecord([(Lab("i"),a'''),(Lab("j"),b'''),(Lab("k"),c'''),(Lab("l"),d''')])], []);
(* [ ''a87 -> int, '''a -> int, ''a88 -> int, '''b -> int, ''a89 -> int, '''c -> int, 
     ''a90 -> int, '''d -> int, ''a -> {i:''a87, j:''a88, k:''a89, l:''a90} ] *)