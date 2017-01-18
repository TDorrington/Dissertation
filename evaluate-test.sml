(* ----------------------------------------------------------------------------------- *)
(* TEST CASES FOR EVALUATE *)

val va' = VHole(SimpleHole(ValueHole(TypeVar("a"))));
val vb' = VHole(SimpleHole(ValueHole(TypeVar("b"))));
val vc' = VHole(SimpleHole(ValueHole(TypeVar("c"))));
val vd' = VHole(SimpleHole(ValueHole(TypeVar("d"))));
val ve' = VHole(SimpleHole(ValueHole(TypeVar("e"))));
val vf' = VHole(SimpleHole(ValueHole(TypeVar("f"))));
val vg' = VHole(SimpleHole(ValueHole(TypeVar("g"))));

val va'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("a"))));
val vb'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("b"))));
val vc'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("c"))));
val vd'' = VHole(SimpleHole(ValueHole(EqualityTypeVar("d"))));

val va''' = VHole(SimpleHole(ValueHole(ArithTypeVar("a"))));
val vb''' = VHole(SimpleHole(ValueHole(ArithTypeVar("b"))));
val vc''' = VHole(SimpleHole(ValueHole(ArithTypeVar("c"))));
val vd''' = VHole(SimpleHole(ValueHole(ArithTypeVar("d"))));
val ve''' = VHole(SimpleHole(ValueHole(ArithTypeVar("e"))));
val vf''' = VHole(SimpleHole(ValueHole(ArithTypeVar("f"))));
val vg''' = VHole(SimpleHole(ValueHole(ArithTypeVar("g"))));
val vh''' = VHole(SimpleHole(ValueHole(ArithTypeVar("h"))));

(* --- ARITHMETIC & BOOLEAN EXPRESSIONS --- *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* 7, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* 12, [], [] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* ~1, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* 7.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* 12.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* ~1.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* 0.75, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(N(4))))),[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(B(true))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(B(false))),Value(Concrete(B(false))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(N(3))),
					   Value(VRecord([(Lab("a"),Concrete(N(4)))])))),[],[])));
(* Stuck, [], [] *)	

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(Concrete(N(3))),Value(Concrete(R(4.0))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(4)))),
		ArithExpr(TIMES,Value(Concrete(N(5))),Value(Concrete(N(2)))))),[],[])));
(* 9, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(Concrete(N(10))),Value(Concrete(N(4)))),
			ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(~1))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(N(3))),Value(Concrete(N(4)))),
			ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(2))))))),[],[])));
(* [(10+4)-(3+-1)] + [(3*4)*(3-2)] => 24, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(Concrete(N(10))),Value(Concrete(N(4)))),
			ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(~1))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(N(3))),Value(Concrete(N(4)))),
			ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(R(2.0))))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(Concrete(R(10.0))),Value(Concrete(R(4.0)))),
			ArithExpr(PLUS,Value(Concrete(R(3.0))),Value(Concrete(R(~1.0))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0)))),
			ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(2.0))))))),[],[])));
(* 24.0, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(Concrete(R(10.0))),Value(Concrete(R(5.0)))),
			ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(1.0))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0)))),
			ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(2.0))))))),[],[])));
(* 11.0, [], [] *)


prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,Value(Concrete(N(2))),Value(Concrete(N(4))))),[],[])));
(* true, [], [] *) 

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* false, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(Concrete(N(4))),Value(Concrete(N(4))))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(Concrete(N(3))),Value(Concrete(N(4))))),[],[])));
(* false, [], [] *)	

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* false, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(Concrete(R(3.0))),Value(Concrete(R(4.0))))),[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(N(4))))),[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(B(true))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(Concrete(B(false))),Value(Concrete(R(3.0))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(Concrete(R(10.0))),Value(Concrete(R(5.0)))),
			ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(1.0))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0)))),
			ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(2.0))))))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(Concrete(R(10.0))),Value(Concrete(R(5.0)))),
			ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(1.0))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0)))),
			ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(2.0))))))),[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(N(4)))),
			ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(3))))),
		ArithExpr(TIMES,
			ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(1)))),
			ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(1))))))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(N(4)))),
			ArithExpr(SUBTRACT,Value(Concrete(N(3))),Value(Concrete(N(3))))),
		ArithExpr(TIMES,
			ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(1)))),
			ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(1))))))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(Concrete(R(10.0))),Value(Concrete(R(5.0)))),
			ArithExpr(DIVIDE,Value(Concrete(R(3.0))),Value(Concrete(R(1.0))))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(Concrete(R(3.0))),Value(Concrete(R(4.0)))),
			ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(Concrete(R(2.0))))))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([])),
		Value(VRecord([])))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([])),
		Value(VRecord([(Lab("a"),Concrete(N(2)))])))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([])),
		Record([]))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true)))])),
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true)))])))),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true)))])),
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true)))])))),[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true)))])),
		Value(VRecord([(Lab("a"),Concrete(N(4))),(Lab("b"),Concrete(B(true)))])))),[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true))),
					   (Lab("c"),Concrete(B(false))),(Lab("d"),Concrete(N(4)))])),
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true))),
					   (Lab("d"),Concrete(N(4))),(Lab("c"),Concrete(B(false)))])))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(B(true))),
					   (Lab("c"),Concrete(B(false))),(Lab("d"),Concrete(N(4)))])),
		Record([(Lab("a"),Value(Concrete(N(3)))),(Lab("b"),Value(Concrete(B(true)))),
				(Lab("d"),Value(Concrete(N(4)))),(Lab("c"),Value(Concrete(B(false))))]))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(Concrete(B(true))),Value(Concrete(B(true))))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(Concrete(N(4))))),[],[])));
(* 5, [v['a]->1], ['a->Int] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(Concrete(N(4))))),[],[])));
(* 4, [v[''a]->1], [''a->Int] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'''),Value(Concrete(N(4))))),[],[])));
(* ~3, [v['''a]->1], ['''a->Int] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'),Value(Concrete(N(4))))),[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(Concrete(R(3.0))),Value(va'))),[],[])));
(* 4.0, [v['a]->1.0], ['a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(Concrete(R(4.0))))),[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(Concrete(R(3.0))),Value(va'''))),[],[])));
(* 2.0, [v['''a]->1.0], ['''a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'''),Value(Concrete(R(4.0))))),[],[])));
(* 0.25, [v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(vb'))),[],[])));
(* v['''a0 + '''a0], 
  [v['a]->v['''a0], v['b]->v['''a0]],
  ['a->'''a0, 'b->'''a0] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va'),Value(va'))),[],[])));
(* v['''a0 * '''a0], 
  [v['a]->v['''a0]], 
  ['a->'''a0] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'),Value(va''))),[],[])));
(* v['''a0 - 1],
  [v['a]->v['''a0], v[''a]->1],
  ['a->'''a0,'''a0->Int,''a->Int] *)  
	
prettyPrintConfig(evaluate(Config(Expression(
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va''),Value(vb'')),
			ArithExpr(PLUS,Value(vc''),Value(vd'')))),[],[])));
(* 4,
   [v[''d]->1,v[''c]->1,v[''b]->1,v[''a]->1]
   [''b->int,''a64->int,''c->int,''d->int,''a->int,''a63->int] *)
   
 prettyPrintConfig(evaluate(Config(Expression(
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(vc'''),Value(vd''')))),[],[])));
(* v['''a7+'''a7+'''a7+'''a7],
   [v['''a6] -> v['''a7], v['''a5] -> v['''a7], v['''d] -> v['''a6], v['''c] -> v['''a6], v['''b] -> v['''a5], v['''a] -> v['''a5]]
   ['''a6 -> '''a7, '''a5 -> '''a7, '''d -> '''a6, '''c -> '''a6, '''b -> '''a5, '''a -> '''a5] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'),Value(va'))),[],[])));
(* 1.0, 
  [v['a]->1.0], 
  ['a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(va'''))),[],[])));
(* v['''a0 + '''a0],
  [v['a]->v['''a0],v['''a]->v['''a0]],
  ['a->'''a0,'''a->'''a0] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(va'''))),[],[])));
(* 1, 
  [v[''a]->1,v['''a]->1], 
  [''a->Int, '''a->Int, '''a0->Int] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'''),Value(va'''))),[],[])));
(* v['''a - '''a],[],[] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va''),Value(va'''))),[],[])));
(* Stuck *)


prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(Concrete(N(4)))),
		ArithExpr(TIMES,Value(Concrete(N(5))),Value(vb')))),[],[])));
(* 2,
  [v['a]->1,v['b]->1]
  ['a->Int,'b->Int] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(Concrete(N(4)))),
		ArithExpr(TIMES,Value(Concrete(N(5))),Value(va')))),[],[])));
(* 2, 
  [v['a]->1], 
  ['a->Int] *)
 
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(vc'),Value(vd')))),[],[])));
(* v['''a12-'''a12 + '''a12*'''a12], 
  [v['''a11] -> v['''a12], v['''a10] -> v['''a12], v['d] -> v['''a11], v['c] -> v['''a11], v['b] -> v['''a10], v['a] -> v['''a10]]
  ['''a11 -> '''a12, '''a10 -> '''a12, 'd -> '''a11, 'c -> '''a11, 'b -> '''a10, 'a -> '''a10] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(va'),Value(vb')),  
			ArithExpr(PLUS,Value(vc'),Value(vd'))),
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(ve'),Value(vf')),
			Value(Concrete(N(3)))))),[],[])));
(* ((v['a]*v['b]) + (v['c]+v['d])) - ((v['e]*v['f])+3)
   =>
   ~1,
   [v['''a15] -> 1, v['''a16] -> 1, v['f] -> v['''a16], v['e] -> v['''a16], v['''a14] -> v['''a15], v['''a13] -> v['''a15], v['d] -> v['''a14], v['c] -> v['''a14], v['b] -> v['''a13], v['a] -> v['''a13]]
   ['''a15 -> int, '''a16 -> int, 'f -> '''a16, 'e -> '''a16, '''a14 -> '''a15, '''a13 -> '''a15, 'd -> '''a14, 'c -> '''a14, 'b -> '''a13, 'a -> '''a13]
*) 
 
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(va'),Value(vb')))),[],[])));
(* v[ '''a17-'''a17 + '''a17*'''a17]
  [v['b] -> v['''a17], v['a] -> v['''a17]]
  ['b -> '''a17, 'a -> '''a17] *)
  
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(va'),Value(vb')))),[],[])));
(* false
  [v['''a13] -> 1, v['b] -> v['''a13], v['a] -> v['''a13]]
  ['''a13 -> int, ''a14 -> int, 'b -> '''a13, 'a -> '''a13] *)
  
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(vc'),Value(vd')))),[],[])));
(* false
  [v['''a16] -> 1, v['''a15] -> 1, v['d] -> v['''a16], v['c] -> v['''a16], v['b] -> v['''a15], v['a] -> v['''a15]]
  ['''a16 -> int, '''a15 -> int, ''a17 -> int, 'd -> '''a16, 'c -> '''a16, 'b -> '''a15, 'a -> '''a15] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(va'),Value(vb')),  
			ArithExpr(PLUS,Value(vc'),Value(vd'))),
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(ve'),Value(vf')),
			Value(Concrete(N(2)))))),[],[])));
(* true
  [v['''a20] -> 1, v['''a21] -> 1, v['f] -> v['''a21], v['e] -> v['''a21], v['''a19] -> v['''a20], v['''a18] -> v['''a20], v['d] -> v['''a19], v['c] -> v['''a19], v['b] -> v['''a18], v['a] -> v['''a18]]
  ['''a20 -> int, '''a21 -> int, 'f -> '''a21, 'e -> '''a21, '''a19 -> '''a20, '''a18 -> '''a20, 'd -> '''a19, 'c -> '''a19, 'b -> '''a18, 'a -> '''a18] *)
  
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,Value(va'''),Value(vb''))),[],[])));
(* true, [v['''a]->1, v[''b]->1], ['''a->Int, ''b->Int, ''a0->Int] *)
  
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),va'),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(5)))])),
	Value(vb''))),[],[])));
(* v[ {a=v[''a28], b=true, c=5} = {a=v[''a28], b=true, c=1} ],
  [v['a] -> v[''a28], v[''b] -> {a=v[''a28], b=true, c=1}],
  ['a -> ''a28, ''a29 -> bool, ''a30 -> int, ''b -> {a:''a28, b:''a29, c:''a30}] *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),va'),(Lab("b"),Concrete(B(true))),(Lab("c"),Concrete(N(5))),(Lab("d"),Concrete(R(1.0)))])),
	Value(vb''))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),va'),(Lab("2"),vb')])),
				   (Lab("b"),VRecord([(Lab("1"),vc'),(Lab("3"),vd''')]))])),
	Value(ve'))),[],[])));
(* v[ {a={1=v[''a41], 2=v[''a42]}, b={1=v[''a43], 3=1}} = {a={1=v[''a41], 2=v[''a42]}, b={1=v[''a43], 3=1}} ],
  [v['''d] -> 1, v['c] -> v[''a43], v['b] -> v[''a42], v['a] -> v[''a41], v['e] -> {a={1=v['a], 2=v['b]}, b={1=v['c], 3=v['''d]}}],
  ['''d -> int, ''a44 -> int, 'c -> ''a43, 'b -> ''a42, 'a -> ''a41, 'a37 -> 'a, 'a38 -> 'b, 'a39 -> 'c,
    'a40 -> '''d, 'a36 -> {1:'a39, 3:'a40}, 'a35 -> {1:'a37, 2:'a38}, 'e -> {a:'a35, b:'a36}] *)
	
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),va'),(Lab("2"),vb')])),
				   (Lab("b"),VRecord([(Lab("1"),vc'),(Lab("3"),vd''')]))])),
	Value(vb'))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),va'''),(Lab("2"),vb''')])),
				   (Lab("b"),VRecord([(Lab("1"),vc'''),(Lab("3"),vd''')]))])),
	Value(ve'))),[],[])));
(* true,
  [v['''d] -> 1, v['''c] -> 1, v['''b] -> 1, v['''a] -> 1, v['e] -> ((v['''a],v['''b]),(v['''c],v['''d]))]
  ['''d -> int, ''a228 -> int, '''c -> int, ''a227 -> int, '''b -> int, ''a226 -> int, '''a -> int, ''a225 -> int, 'a221 -> '''a, 'a222 -> '''b, 'a223 -> '''c, 'a224 -> '''d, 'a220 -> ('a223 * 'a224), 'a219 -> ('a221 * 'a222), 'e -> ('a219 * 'a220)] *)


prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),va'''),(Lab("2"),vb''')]))])),
	Value(vb'''))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(5)))])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(B(true))),(Lab("3"),Concrete(B(false)))]))])),
	Value(ve'))),[],[])));
(* false,
  [v['e] -> {a={1=1, 2=1}, b={1=true, 3=true}}],
  ['a57 -> int, 'a58 -> int, 'a59 -> bool, 'a60 -> bool, 'a56 -> {1:'a59, 3:'a60},
   'a55 -> {1:'a57, 2:'a58}, 'e -> {a:'a55, b:'a56}]
*)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(5))),
									  (Lab("3"),Concrete(B(true))),(Lab("4"),Concrete(R(1.0)))]))])),
	Value(ve'))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(VRecord([(Lab("a"),VRecord([(Lab("1"),va''')])),
				   (Lab("b"),VRecord([(Lab("1"),Concrete(N(5))),
									  (Lab("2"),VRecord([(Lab("x"),Concrete(B(false))),
														 (Lab("y"),vb''')]))]))])),
	Value(VRecord([(Lab("a"),va''),(Lab("b"),vb'')])))),[],[])));
(* false,
  [v['''b] -> 1, v[''b] -> {1=1, 2={x=true, y=1}}, v['''a] -> 1, v[''a] -> {1=1}],
  [''a67 -> int, ''a69 -> bool, ''a70 -> int, '''b -> int, ''a68 -> {x:''a69, y:''a70}, 
   ''b -> {1:''a67, 2:''a68}, ''a66 -> int, '''a -> int, ''a -> {1:''a66}] *)
  
 (* --- EXPRESSION PAIRS --- *)
  
prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),Value(Concrete(N(3)))),(Lab("b"),Value(Concrete(R(5.0))))])),[],[])));	
(* {a=3,b=5.0}, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),Value(Concrete(B(true)))),(Lab("b"),Value(Concrete(R(5.0))))])),[],[])));	
(* {a=true,b=5.0}, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),Value(Concrete(B(true)))),
	(Lab("b"),Record([(Lab("1"),Value(Concrete(R(5.0)))),
					  (Lab("2"),Value(VRecord([(Lab("x"),Concrete(B(true))),(Lab("y"),Concrete(B(false)))])))]))])),[],[])));	
(* {a=true,b={1=5.0,2={x=true,y=false}}}, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),ArithExpr(PLUS,Value(va'),Value(vb'))),
	(Lab("b"),Value(Concrete(N(2)))),
	(Lab("c"),Case(ArithExpr(PLUS,Value(va'),Value(vb')),
				   [(PVal(N(3)),
				     Value(Concrete(N(5))))]))])),[],[])));
(* {a=v[ v['''a106] + v['''a106] ], b=2, c=v[ case v[ v['''a106] + v['''a106] ] of 3 -> 5 ]},
  [v['''a104] -> v['''a106], v['b] -> v['''a104], v['a] -> v['''a104]],
  ['''a104 -> '''a106, 'a105 -> '''a106, 'b -> '''a104, 'a -> '''a104] *)
	
prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),ArithExpr(PLUS,
				ArithExpr(TIMES,Value(Concrete(N(5))),Value(Concrete(N(2)))),
				ArithExpr(SUBTRACT,Value(Concrete(N(2))),Value(Concrete(N(2)))))),
	(Lab("b"),BoolExpr(LESS,
				ArithExpr(PLUS,Value(va'),Value(vb')),
				ArithExpr(TIMES,Value(va'),Value(vb')))),
	(Lab("c"),Case(Value(Concrete(N(2))),
				   [(PVar(Var("x")),
				     ArithExpr(PLUS,Variable(Var("x")),Variable(Var("x"))))])),
	(Lab("d"),Record([(Lab("1"),App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))),
									Condition(Value(Concrete(B(true))),
											  Value(Concrete(N(10))),
											  Value(Concrete(N(20)))))),
					  (Lab("2"),Value(Fun(Var("x"),TFun(Int,Int),App(Variable(Var("x")),Value(Concrete(N(5)))))))]))])),[],[])));
(* {a=10, b=v[ '''a0+'''a0 < '''a0*'''a0], c=4,
    d={1=100,2=fn x:(int->int)=> (x)(5)}}
  [v['b] -> v['''a0], v['a] -> v['''a0]]
  ['b -> '''a0, 'a -> '''a0] *)

prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("a"),BoolExpr(EQ,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(TIMES,Value(va'),Value(vb')))),
	(Lab("b"),BoolExpr(LESS,
		ArithExpr(TIMES,Value(va'),Value(vb')),
		ArithExpr(PLUS,Value(va'),Value(vb'))))])),[],[])));
(* {a=false,b=true},
  [v['''a48] -> 1, v['b] -> v['''a48], v['a] -> v['''a48]]
  ['''a51 -> int, '''a50 -> int, '''a48 -> int, ''a49 -> int, 'b -> '''a48, 'a -> '''a48] *)
		
(* --- CONDITIONS --- *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(Concrete(B(true))),Value(Concrete(N(5))),Value(Concrete(N(2))))),[],[])));
(* 5, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(Concrete(B(false))),Value(Concrete(N(5))),Value(Concrete(N(2))))),[],[])));
(* 2, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(Concrete(B(true))),
	BoolExpr(LESS,Value(Concrete(N(2))),Value(Concrete(N(3)))),
	BoolExpr(MORE,Value(Concrete(N(2))),Value(Concrete(N(3)))))),[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(Concrete(B(false))),
	BoolExpr(EQ,Value(Concrete(N(2))),Value(Concrete(N(3)))),
	BoolExpr(EQ,Value(VRecord([(Lab("a"),Concrete(N(2)))])),Record([(Lab("a"),Value(Concrete(N(2))))])))),
	[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(Concrete(B(false))),
	BoolExpr(EQ,Value(Concrete(N(2))),Value(Concrete(N(3)))),
	BoolExpr(EQ,Value(VRecord([(Lab("a"),Concrete(N(2)))])),Record([(Lab("b"),Value(Concrete(N(2))))])))),
	[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	Record([
		(Lab("1"),BoolExpr(EQ,Value(va'''),Value(vb'''))),
		(Lab("2"),ArithExpr(DIVIDE,Value(vb'),Value(vc')))]),
	Value(Concrete(N(3))))),[],[])));
(* {1=true,2=1.0},
    [v['c] -> 1.0, v['b] -> 1.0, v['''b] -> 1, v['''a] -> 1, v['a] -> true],
    ['c -> real, 'b -> real, '''b -> int, '''a -> int, ''a76 -> int, 'a -> bool] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	BoolExpr(EQ,Value(va'),Value(va')),
	Value(Concrete(B(false))))),[],[])));
(* true, [v['a]->true], ['a->bool,''a0->bool] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	BoolExpr(EQ,Value(va'),Value(vb'')),
	Value(Concrete(B(false))))),[],[])));
(* true, [v[''b]->true,v['a]->true], [''b->bool,''a0->bool,'a->bool] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va''),
	ArithExpr(PLUS,
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(va'''),Value(vb'''))),
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(va'''),Value(vb''')))),
	Value(Concrete(N(2))))),[],[])));
(* v[ ('''a60+'''a60 + '''a60+'''a60) + ('''a60+'''a60 + '''a60+'''a60)]
  [v['''b] -> v['''a60], v['''a] -> v['''a60], v[''a] -> true]
  ['''b -> '''a60, '''a -> '''a60, ''a -> bool] *)
 
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'''),Value(Concrete(B(true))),Value(Concrete(B(false))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(LESS,Value(Concrete(N(2))),Value(Concrete(N(5)))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(vc'''),Value(vd'''))),
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(ve'''),Value(vf''')),
			ArithExpr(PLUS,Value(vg'''),Value(vh''')))),
	Value(Concrete(N(2))))),[],[])));
(* v['''a67+'''a67+'''a67+'''a67  + '''a67+'''a67+'''a67+'''a67],
  [v['''a66] -> v['''a67], v['''a63] -> v['''a67], v['''a65] -> v['''a66], v['''a64] -> v['''a66], v['''h] -> v['''a65], v['''g] -> v['''a65], v['''f] -> v['''a64], v['''e] -> v['''a64], v['''a62] -> v['''a63], v['''a61] -> v['''a63], v['''d] -> v['''a62], v['''c] -> v['''a62], v['''b] -> v['''a61], v['''a] -> v['''a61]]
  ['''a66 -> '''a67, '''a63 -> '''a67, '''a65 -> '''a66, '''a64 -> '''a66, '''h -> '''a65, '''g -> '''a65, '''f -> '''a64, '''e -> '''a64, '''a62 -> '''a63, '''a61 -> '''a63, '''d -> '''a62, '''c -> '''a62, '''b -> '''a61, '''a -> '''a61] *)
  
prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(EQ,Value(Concrete(R(3.0))),Value(Concrete(R(2.0)))),
	Value(Concrete(N(2))),Value(Concrete(N(3))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(LESS_EQ,Value(va''),Value(vb'')),
	ArithExpr(DIVIDE,Value(va'),ArithExpr(PLUS,Value(vb'),Value(vc'''))),
	Value(Concrete(R(2.0))))),
	[],[])));
(* 0.5
  [v['''a69] -> 1.0, v['a] -> 1.0, v['''c] -> v['''a69], v['b] -> v['''a69], v[''b] -> 1, v[''a] -> 1]
  ['''a69 -> real, 'a -> real, '''c -> '''a69, 'b -> '''a69, ''b -> int, ''a -> int, '''a68 -> int]
*)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,Value(va''),Value(vb'')),
	Value(Concrete(R(2.0))),
	ArithExpr(TIMES,Value(Concrete(N(2))),ArithExpr(PLUS,Value(va''),Value(vb''))))),
	[],[])));
(* 4
  [v[''b] -> 1, v[''a] -> 1]
  ['''a71 -> int, ''b -> int, ''a -> int, '''a70 -> int] *)
  
prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,
		ArithExpr(PLUS,Value(va''),Value(vb'')),
		ArithExpr(SUBTRACT,Value(vc''),Value(vd''))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,Value(va''),Value(vb'')),
		ArithExpr(PLUS,Value(vc''),Value(vd''))),
	Value(Concrete(N(2))))),
	[],[])));
(* 4
  [v[''d] -> 1, v[''c] -> 1, v[''b] -> 1, v[''a] -> 1]
  ['''a75 -> int, '''a74 -> int, ''d -> int, ''c -> int, '''a73 -> int, ''b -> int, ''a -> int, '''a72 -> int] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(SUBTRACT,Value(vc'),Value(vd'))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(PLUS,Value(vc'),Value(vd'))),
	Value(Concrete(N(2))))),
	[],[])));
(* v[if ('''a79+'''a79 > '''a79-'''a79) then ('''a79+''''a79 + '''a79+'''a79) else 2]
  [v['''a78] -> v['''a79], v['''a77] -> v['''a78], v['''a76] -> v['''a78], v['d] -> v['''a77], v['c] -> v['''a77], v['b] -> v['''a76], v['a] -> v['''a76]]
  ['''a78 -> '''a79, '''a77 -> '''a78, '''a76 -> '''a78, 'd -> '''a77, 'c -> '''a77, 'b -> '''a76, 'a -> '''a76] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(SUBTRACT,Value(vc'),Value(Concrete(N(2))))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(PLUS,Value(vc'),Value(vd'))),
	Value(Concrete(N(2))))),
	[],[])));
(* 4,
  [v['d] -> 1, v['''a80] -> 1, v['c] -> 1, v['b] -> v['''a80], v['a] -> v['''a80]]
  ['d -> int, '''a82 -> int, '''a81 -> int, '''a80 -> int, 'c -> int, 'b -> '''a80, 'a -> '''a80] *)
 
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Record([(Lab("a"),Value(Concrete(N(2))))]),
	Value(Concrete(N(2))),Value(Concrete(N(3))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(LESS,Value(Concrete(N(2))),Value(Concrete(N(3)))),
			  BoolExpr(MORE,Value(Concrete(R(5.0))),Value(Concrete(R(2.0)))),
			  BoolExpr(EQ,Value(Concrete(B(true))),Value(Concrete(B(true))))),
	Value(Concrete(N(3))),Value(Concrete(N(2))))),[],[])));
(* 3, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(LESS,Value(Concrete(N(2))),Value(Concrete(N(3)))),
			  Value(va'),
			  BoolExpr(EQ,Value(Concrete(B(true))),Value(Concrete(B(true))))),
	Value(va'),Value(Concrete(B(false))))),[],[])));
(* true, [v['a]->true], ['a->bool] *)
 
 prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(EQ,Record([(Lab("a"),Value(va'')),(Lab("b"),ArithExpr(PLUS,Value(Concrete(N(2))),Value(vb'')))]),
						  Value(VRecord([(Lab("a"),Concrete(B(true))),(Lab("b"),Concrete(N(3)))]))),
			  BoolExpr(LESS,Value(Concrete(N(1))),ArithExpr(PLUS,Value(va'''),Value(vb'''))),
			  Value(Concrete(B(false)))),
	Condition(Value(va''),
			  ArithExpr(PLUS,ArithExpr(PLUS,Value(va'''),Value(vb''')),Value(vc''')),
			  ArithExpr(SUBTRACT,ArithExpr(SUBTRACT,Value(va'''),Value(vb''')),Value(vc'''))),
	Value(Concrete(N(100))))),[],[])));
(* if (if (v[''a],2+''b)=(true,3) then (1<v['''a]+v['''b]) else false
   then (if v[''a] then (v['''a]+v['''b]+v['''c]) else (v['''a]-v['''b]-v['''c])
   else 100 
   =>
   3,
   [v['''c] -> 1, v['''a107] -> 1, v['''b] -> v['''a107], v['''a] -> v['''a107], v[''a] -> true, v[''b] -> 1],
   ['''c -> int, '''a108 -> int, '''a107 -> int, '''b -> '''a107, '''a -> '''a107, ''a -> bool, ''b -> int] *)
 
(* --- CASE --- *)

val x = Variable(Var("x"));
val y = Variable(Var("y"));
val z = Variable(Var("z"));
val a = Variable(Var("a"));
val b = Variable(Var("b"));

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(2))),
	[(PWildcard,
	  Value(Concrete(N(5))))])),[],[])));
(* 5, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("a"),ArithExpr(PLUS,Value(Concrete(N(5))),Value(Concrete(N(6)))))]),
	[(PWildcard,
	Value(Concrete(N(7))))])),[],[])));
(* 7, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(2))),
	[(PVal(N(2)),
	Value(Concrete(N(5))))])),[],[])));
(* 5, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(2))),
	[(PVal(N(3)),
	Value(Concrete(N(5))))])),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,x))),Value(Concrete(N(5)))),
	[(PVal(N(25)),
	Value(Concrete(N(2))))])),[],[])));
(* 2, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	BoolExpr(LESS,Value(Concrete(N(3))),Value(Concrete(N(5)))),
	[(PVal(B(true)),
	Value(Concrete(B(false))))])),[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	App(Value(Fun(Var("x"),Real,ArithExpr(TIMES,x,x))),Value(Concrete(R(5.0)))),
	[(PVal(R(25.0)),
	Value(Concrete(N(2))))])),[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,x))),Value(Concrete(N(5)))),
	[(PVar(Var("x")),
	ArithExpr(PLUS,x,Value(Concrete(N(2)))))])),[],[])));
(* 27, [], [] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),Int,BoolExpr(LESS,x,Value(Concrete(N(10)))))),
	[(PVar(Var("x")),
	App(x,Value(Concrete(N(5)))))])),[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),Int,BoolExpr(MORE,x,Value(va')))),
	[(PVar(Var("x")),
	App(x,Value(Concrete(N(5)))))])),[],[])));
(* true, [v['a]->1], ['a->int] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),TRecord([(Lab("a"),TFun(Int,Int)),(Lab("b"),TFun(Int,Int)),(Lab("c"),Int)]),
		      Case(x,
			       [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
				     App(x,App(y,z)))]))),
	[(PVar(Var("x")),
	App(x,Record([(Lab("a"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,x)))),
				  (Lab("b"),Value(Fun(Var("x"),Int,ArithExpr(PLUS,x,x)))),
				  (Lab("c"),Value(Concrete(N(5))))])))])),[],[])));
(* 100, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
				        Case(Record([(Lab("a"),x),(Lab("b"),y)]),
					         [(PRecord([(Lab("a"),PRecord([(Lab("1"),PVar(Var("x"))),
												           (Lab("2"),PVar(Var("y")))])),
								        (Lab("b"),PRecord([(Lab("1"),PVar(Var("z"))),
												           (Lab("2"),PVar(Var("a"))),
												           (Lab("3"),PVar(Var("b")))]))]),
					           App(App(App(x,App(a,y)),z),b))]))))),
	[(PVar(Var("x")),
	  App(x,Record([(Lab("1"),Value(Fun(Var("x"),Int,
								   Value(Fun(Var("y"),TFun(Int,Int),
									     Value(Fun(Var("z"),Int,
											   ArithExpr(PLUS,x,App(y,z))))))))),
				   (Lab("2"),Value(Concrete(N(3))))])))])),[],[])));
(* fn y:{1:(int -> int), 2:(int -> int), 3:int} => case {a={1=fn x215:int => fn y:(int -> int) => fn z:int => x215 + (y) (z), 2=3}, b=y} of {a={1=x193, 2=y193}, b={1=z, 2=a, 3=b}} -> (((x193) ((a) (y193))) (z)) (b),
    [v['a198] -> v['a213], v['a197] -> fn x:'a195 => v['a204], v['a194] -> fn x:'a204 => fn x:'a196 => fn x:'a213 => v['a214],
   	 v['b] -> {1=v['a196], 2=v['a197], 3=v['a198]}, v['a] -> {1=v['a194], 2=v['a195]}], [...] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
				        Case(Record([(Lab("a"),x),(Lab("b"),y)]),
					       [(PRecord([(Lab("a"),PRecord([(Lab("1"),PVar(Var("x"))),
												         (Lab("2"),PVar(Var("y")))])),
								      (Lab("b"),PRecord([(Lab("1"),PVar(Var("z"))),
												         (Lab("2"),PVar(Var("a"))),
												         (Lab("3"),PVar(Var("b")))]))]),
					         App(App(App(x,App(a,y)),z),b))]))))),
	[(PVar(Var("x")),
	  App(App(x,Record([(Lab("1"),Value(Fun(Var("x"),Int,
								   Value(Fun(Var("y"),TFun(Int,Int),
									     Value(Fun(Var("z"),Int,
											   ArithExpr(PLUS,x,App(y,z))))))))),
				   (Lab("2"),Value(Concrete(N(3))))])),
		Record([(Lab("1"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,x)))),
				(Lab("2"),Value(Fun(Var("x"),Int,ArithExpr(PLUS,x,x)))),
				(Lab("3"),Value(Concrete(N(5))))])))])),[],[])));
(* 31,
  [v['a268] -> v['a283], v['a267] -> fn x:'a265 => v['a274], v['a264] -> fn x:'a274 => fn x:'a266 => fn x:'a283 => v['a284],
   v['b] -> {1=v['a266], 2=v['a267], 3=v['a268]}, v['a] -> {1=v['a264], 2=v['a265]}], [...] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
				        Case(Record([(Lab("a"),x),(Lab("b"),y)]),
					       [(PRecord([(Lab("a"),PRecord([(Lab("1"),PVar(Var("x"))),
												         (Lab("2"),PVar(Var("y")))])),
								      (Lab("b"),PRecord([(Lab("1"),PVar(Var("z"))),
												         (Lab("2"),PVar(Var("a"))),
												         (Lab("3"),PVar(Var("b")))]))]),
					         App(App(App(x,App(a,y)),z),b))]))))),
	[(PVar(Var("x")),
	  App(x,Value(vc')))])),[],[])));
(* fn y:{1:'a448, 2:('a447 -> 'a463), 3:'a450} => case {a=v['c], b=y} of {a={1=x407, 2=y407}, b={1=z, 2=a, 3=b}} -> (((x407) ((a) (y407))) (z)) (b) 
  [v['a436] -> v['a450], v['a432] -> v['a463], v['a434] -> v['a448], v['a431] -> v['a447], v['a437] -> v['a459], v['a412] -> v['a427],
   v['a411] -> fn x:'a409 => v['a418], v['a408] -> fn x:'a418 => fn x:'a410 => fn x:'a427 => v['a428],
   v['b] -> {1=v['a410], 2=v['a411], 3=v['a412]}, v['a] -> {1=v['a408], 2=v['a409]}], [...] *)
   
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
			  Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),
				        Case(Record([(Lab("a"),x),(Lab("b"),y)]),
					       [(PRecord([(Lab("a"),PRecord([(Lab("1"),PVar(Var("x"))),
												         (Lab("2"),PVar(Var("y")))])),
								      (Lab("b"),PRecord([(Lab("1"),PVar(Var("z"))),
												         (Lab("2"),PVar(Var("a"))),
												         (Lab("3"),PVar(Var("b")))]))]),
					         App(App(App(x,App(a,y)),z),b))]))))),
	[(PVar(Var("x")),
	  App(App(x,Value(vc')),Value(vd')))])),[],[])));
(* v['a590],
  [v['a587] -> v['a590], v['a583] -> v['a587], v['a573] -> v['a583], v['a577] -> v['a578], v['d] -> {1=v['a564], 2=fn x:'a576 => v['a577], 3=v['a566]},
  v['a541] -> v['a563], v['a550] -> v['a573], v['a537] -> v['a544], v['a539] -> v['a551], v['a535] -> v['a542],
  v['c] -> {1=fn x:'a545 => fn x:'a547 => fn x:'a549 => v['a550], 2=v['a541]}, v['a503] -> v['a517], v['a499] -> v['a530],
  v['a501] -> v['a515], v['a498] -> v['a514], v['a504] -> v['a526], v['a479] -> v['a494], v['a478] -> fn x:'a476 => v['a485],
  v['a475] -> fn x:'a485 => fn x:'a477 => fn x:'a494 => v['a495], v['b] -> {1=v['a477], 2=v['a478], 3=v['a479]}, v['a] -> {1=v['a475], 2=v['a476]}], [...] *)
   
val x1y2 = PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]);
val a1b2 = PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b")))]);
   
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),Concrete(N(2))),(Lab("2"),Concrete(N(3)))])),
	[(x1y2,
	  ArithExpr(PLUS,x,y))])),[],[])));
(* 5, [], ['a86->int], [...]] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),Concrete(R(2.0))),(Lab("2"),Concrete(R(4.0)))])),
	[(x1y2,
	  ArithExpr(DIVIDE,x,y))])),[],[])));
(* 0.5, [], [...] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(B(false)))])),
	               (Lab("2"),VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(B(false)))]))])),
	[(x1y2,
	  BoolExpr(EQ,x,y))])),[],[])));
(* true, [], [...] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),Concrete(N(2))),(Lab("2"),Concrete(N(3))),(Lab("3"),Concrete(N(4))),
				   (Lab("4"),Concrete(N(5))),(Lab("5"),Concrete(N(6)))])),
	[(PRecord([(Lab("5"),PVar(Var("a"))),(Lab("4"),PVar(Var("b"))),(Lab("3"),PVar(Var("x"))),
		   	   (Lab("2"),PVar(Var("z"))),(Lab("1"),PVar(Var("y")))]),
	  ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))])),[],[])));
(* 20, [], [...] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("1"),ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(5))))),
		    (Lab("2"),BoolExpr(LESS,Value(Concrete(R(1.0))),Value(Concrete(R(2.0)))))]),
	[(x1y2,
	  BoolExpr(EQ,BoolExpr(EQ,x,x),y))])),[],[])));
(* true, [], ['a98->bool, '97->int], [x->8,y->true] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("1"),Record([(Lab("a"),ArithExpr(PLUS,Value(Concrete(N(3))),Value(Concrete(N(5))))),
							  (Lab("b"),BoolExpr(LESS,Value(Concrete(R(1.0))),Value(Concrete(R(2.0)))))])),
			(Lab("2"),Value(Concrete(B(true))))]),
  [(x1y2,
	Condition(y,
			  Record([(Lab("a"),x),
					  (Lab("b"),Record([(Lab("x"),y),(Lab("y"),y)]))]),
			  Record([(Lab("a"),Value(Concrete(N(1)))),
					  (Lab("b"),Value(Concrete(N(1))))])))])),
	[],[])));
(* {a={a=8,b=true},b={x=true,y=true}}, [], 
  ['a100->bool,'a102->bool,'a101->int,'a99->('a101*'a102)],
  [x->(8,true),y->true] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(va'),
	[(x1y2,
	  Record([(Lab("a"),x),(Lab("b"),y)]))])),[],[])));
(* {a=v['a612], b=v['a613]},
  [v['a] -> {1=v['a612], 2=v['a613]}],
  ['a614 -> 'a612, 'a615 -> 'a613, 'a -> {1:'a614, 2:'a615}] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),va'),(Lab("2"),vb')])),
  [(x1y2,
	Record([(Lab("x"),ArithExpr(PLUS,x,y)),(Lab("y"),ArithExpr(SUBTRACT,x,y))]))])),[],[])));
(* {x=v[ v['''a618] + v['''a618] ], y=v[ v['''a618] - v['''a618] ]},
  [v['a617] -> v['''a618], v['a616] -> v['''a618], v['b] -> v['a617], v['a] -> v['a616]],
  ['a617 -> '''a618, 'a616 -> '''a618, 'b -> 'a617, 'a -> 'a616] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("1"),VRecord([(Lab("a"),va'),(Lab("b"),vb')])),
				   (Lab("2"),VRecord([(Lab("a"),vc'),
								      (Lab("b"),VRecord([(Lab("x"),vd''),(Lab("y"),vg')]))]))])),
	[(x1y2,
	Record([(Lab("x"),BoolExpr(EQ,x,Value(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(B(true)))])))),
			(Lab("y"),BoolExpr(EQ,y,Value(VRecord([(Lab("a"),VRecord([(Lab("f"),Concrete(B(false))),
																	  (Lab("h"),Concrete(N(5)))])),
												   (Lab("b"),VRecord([(Lab("x"),ve'''),
																	  (Lab("y"),vf''')]))]))))]))])),[],[])));
(* {x=true, y=v[ {a={f=true, h=1}, b={x=v[''a629], y=v[''a630]}} = {a={f=false, h=5}, b={x=1, y=1}} ]},
  [v['''f] -> 1, v['a626] -> v[''a630], v['''e] -> 1, v[''d] -> v[''a629], v['a623] -> {f=true, h=1},
   v['a622] -> true, v['a621] -> 1, v['g] -> v['a626], v['c] -> v['a623], v['b] -> v['a622], v['a] -> v['a621]] *)
   
prettyPrintConfig(evaluate(Config(Expression(Case(
	Condition(Value(va'),
			  Value(VRecord([(Lab("1"),VRecord([(Lab("1"),Concrete(N(1))),(Lab("2"),Concrete(N(2)))])),
							 (Lab("2"),VRecord([(Lab("a"),Concrete(N(1)))])),
							 (Lab("3"),VRecord([]))])),
			  Value(vb')),
	[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y"))),(Lab("3"),PVar(Var("z")))]),
	ArithExpr(PLUS,
			  Case(x,[(a1b2,ArithExpr(PLUS,a,b))]),
			  ArithExpr(PLUS,
						Case(y,[(PRecord([(Lab("a"),PVal(N(1)))]),Value(Concrete(N(3))))]),
						Case(z,[(PRecord([]),Value(Concrete(N(4))))]))))])),[],[])));
(* 10, [v['a] -> true], [...] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("a"),ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(1))))),
			(Lab("b"),ArithExpr(SUBTRACT,Value(Concrete(N(4))),Value(Concrete(N(1))))),
			(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(N(2)))))]),
	[(PRecord([(Lab("c"),PVar(Var("z"))),(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	ArithExpr(TIMES,
			  Case(Record([(Lab("1"),x),(Lab("2"),Value(Concrete(N(2)))),(Lab("3"),Value(Concrete(N(4))))]),
				 [(PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b"))),(Lab("3"),PWildcard)]),
				   ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(Concrete(N(2))))))]),
			  Case(Record([(Lab("1"),Value(Concrete(N(3)))),(Lab("2"),y),(Lab("3"),z)]),
			     [(PRecord([(Lab("3"),PVar(Var("c"))),(Lab("1"),PVal(N(3))),(Lab("2"),PVar(Var("b")))]),
				   ArithExpr(TIMES,Variable(Var("c")),ArithExpr(TIMES,b,Value(Concrete(N(3))))))])))])),[],[])));
(* case (2,3,4) of (x,y,z) -> (case (2,2,4) of (a,b,_) -> a*b*2) * (case (3,3,4) of (3,b,c) -> a*b*c)
   =>
   288, [], 
   ['a137 -> int, 'a136 -> int, 'a135 -> int, 'a134 -> int, 'a133 -> int, 'a132 -> int],
   [a -> 3, b -> 3, a -> 2, b -> 2, x -> 2, y -> 3]
*)  

prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("a"),ArithExpr(PLUS,Value(Concrete(N(1))),Value(Concrete(N(1))))),
			(Lab("b"),ArithExpr(SUBTRACT,Value(Concrete(N(4))),Value(Concrete(N(1))))),
			(Lab("c"),ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(N(2)))))]),
	[(PRecord([(Lab("c"),PVar(Var("z"))),(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	ArithExpr(TIMES,
			  Case(Record([(Lab("1"),x),(Lab("2"),Value(Concrete(N(2)))),(Lab("3"),Value(Concrete(N(4))))]),
				   [(PRecord([(Lab("1"),PVar(Var("a"))),(Lab("2"),PVar(Var("b"))),(Lab("3"),PWildcard)]),
				   ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(Concrete(N(2))))))]),
			  Case(Record([(Lab("1"),Value(Concrete(N(3)))),(Lab("2"),y),(Lab("3"),z)]),
			       [(PRecord([(Lab("3"),PVar(Var("c"))),(Lab("1"),PVal(N(1))),(Lab("2"),PVar(Var("b")))]),
				   ArithExpr(TIMES,Variable(Var("c")),ArithExpr(TIMES,b,Value(Concrete(N(3))))))])))])),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("1"),ArithExpr(PLUS,Value(va'),Value(vb'))),(Lab("2"),ArithExpr(SUBTRACT,Value(vc'),Value(vd')))]),
	[(x1y2, ArithExpr(PLUS,x,y))])),[],[])));
(* v[ '''a144+'''a144 + '''a144-'''a144 ],
  [v['''a143] -> v['''a144], v['''a142] -> v['''a144], v['''a139] -> v['''a143], v['''a138] -> v['''a142], v['d] -> v['''a139], v['c] -> v['''a139], v['b] -> v['''a138], v['a] -> v['''a138]]
  [...]
*)  
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("1"),ArithExpr(PLUS,Value(va'),Value(vb'))),(Lab("2"),ArithExpr(SUBTRACT,Value(vc'),Value(vd')))]),
	[(x1y2, 
	ArithExpr(PLUS,
			  Case(Record([(Lab("1"),x),(Lab("2"),Value(va'''))]),
				   [(a1b2,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vb'''))))]),
			  Case(Record([(Lab("1"),Value(vc''')),(Lab("2"),y)]),
			       [(a1b2,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vd'''))))])))])),[],[])));
(* v[ (('''a173+'''a173) * '''a173 * '''a173) + ('''a173 * ('''a173-'''a173) * '''a173)) ],
  [v['''a172] -> v['''a173], v['''a167] -> v['''a173], v['''a171] -> v['''a172], v['''c] -> v['''a172], v['''d] -> v['''a171],
  v['''a170] -> v['''a171], v['''a162] -> v['''a170], v['''a166] -> v['''a167], v['''a165] -> v['''a167],
  v['''b] -> v['''a166], v['''a] -> v['''a166], v['''a161] -> v['''a165], v['''a158] -> v['''a162],
  v['''a157] -> v['''a161], v['d] -> v['''a158], v['c] -> v['''a158], v['b] -> v['''a157], v['a] -> v['''a157]],
  [...] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4))),(Lab("c"),Concrete(N(5)))])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
	Case(Record([(Lab("a"),ArithExpr(PLUS,x,y)),
				 (Lab("b"),ArithExpr(TIMES,x,y)),
				 (Lab("c"),ArithExpr(PLUS,x,z)),
				 (Lab("d"),ArithExpr(TIMES,x,z))]),
		 [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),
				  (Lab("c"),PVar(Var("z"))),(Lab("d"),PVar(Var("a")))]),
		 Record([(Lab("x"),ArithExpr(TIMES,x,a)),(Lab("y"),ArithExpr(TIMES,z,y))]))]))])),[],[])));
(* {x=105,y=96}, ... *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(3))),(Lab("b"),Concrete(N(4))),(Lab("c"),Concrete(N(5)))])),
	[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
	Case(Record([(Lab("a"),ArithExpr(PLUS,x,y)),
				 (Lab("b"),ArithExpr(TIMES,x,y)),
				 (Lab("c"),ArithExpr(PLUS,x,z)),
				 (Lab("d"),ArithExpr(TIMES,x,z))]),
		 [(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),
				  (Lab("c"),PVar(Var("z"))),(Lab("e"),PVar(Var("a")))]),
		 Record([(Lab("x"),ArithExpr(TIMES,x,a)),(Lab("y"),ArithExpr(TIMES,z,y))]))]))])),[],[])));
(* Stuck *)
  
(* FUNCTIONS *)

prettyPrintConfig(evaluate(Config(Expression(Value(
	Fun(Var("x"),Int,ArithExpr(PLUS,x,x)))),[],[])));
(* fn x:int => x+x, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Value(
	Fun(Var("x"),Int,
		Value(Fun(Var("y"),Int,ArithExpr(PLUS,y,x)))))),[],[])));
(* fn x:int => fn y:int => y+x, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),
	Value(Concrete(N(2))))),[],[])));
(* 4, [], []  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),
	Value(Concrete(R(2.0))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Real,ArithExpr(PLUS,x,x))),
	Value(Concrete(R(2.0))))),[],[])));
(* 4.0 *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,ArithExpr(DIVIDE,x,x))),
	Value(Concrete(N(2))))),[],[])));
(* Stuck  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,ArithExpr(PLUS,x,y))))),
	Value(Concrete(N(2))))),[],[])));
(* fn y:int => 2+y, [], []  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,ArithExpr(DIVIDE,x,y))))),
	Value(Concrete(N(2))))),[],[])));
(* Stuck  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Real,
		 Value(Fun(Var("y"),Int,ArithExpr(DIVIDE,x,y))))),
	Value(Concrete(R(2.0))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,ArithExpr(PLUS,x,y))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3))))),[],[])));
(* 5, [], []  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,
			   Value(Fun(Var("z"),Int,
					 Value(Fun(Var("a"),Int,
						   Value(Fun(Var("b"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))))))))))),
	Value(Concrete(N(2))))),[],[])));
 (* fn y:int => fn z:int  => fn a:int => fn b:int => 2+y+z+a+b, [], [] *)
 
 prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,
			   Value(Fun(Var("z"),Int,
					 Value(Fun(Var("a"),Int,
						   Value(Fun(Var("b"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))))))))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3))))),[],[])));
 (* fn z:int  => fn a:int => fn b:int => 2+3+z+a+b *)
 
  prettyPrintConfig(evaluate(Config(Expression(App(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,
			   Value(Fun(Var("z"),Int,
					 Value(Fun(Var("a"),Int,
						   Value(Fun(Var("b"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))))))))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3)))),Value(Concrete(N(4))))),[],[])));
 (* fn a:int => fn b:int => 2+3+4+a+b *)
 
   prettyPrintConfig(evaluate(Config(Expression(App(App(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,
			   Value(Fun(Var("z"),Int,
					 Value(Fun(Var("a"),Int,
						   Value(Fun(Var("b"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))))))))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3)))),Value(Concrete(N(4)))),Value(Concrete(N(5))))),[],[])));
 (* fn b:int => 2+3+4+5+b *)
 
    prettyPrintConfig(evaluate(Config(Expression(App(App(App(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("y"),Int,
			   Value(Fun(Var("z"),Int,
					 Value(Fun(Var("a"),Int,
						   Value(Fun(Var("b"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,y,ArithExpr(PLUS,z,ArithExpr(PLUS,a,b)))))))))))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3)))),Value(Concrete(N(4)))),Value(Concrete(N(5)))),Value(Concrete(N(6))))),[],[])));
 (* 20 *)
 
     prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,
			   Value(Fun(Var("x"),Int,
					 Value(Fun(Var("x"),Int,
						   Value(Fun(Var("x"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,x,ArithExpr(PLUS,x,ArithExpr(PLUS,x,x)))))))))))))),
	Value(Concrete(N(2))))),[],[])));
 (* (fn x:int => fn x:int => fn x:int => fn x:int => fn x:int => x+x+x+x+x) 2
     =>
    (fn x:int => fn x:int => fn x:int => fn x:int => x+x+x+x+x)	 *) 

 prettyPrintConfig(evaluate(Config(Expression(App(App(App(App(App(
	Value(Fun(Var("x"),Int,
		 Value(Fun(Var("x"),Int,
			   Value(Fun(Var("x"),Int,
					 Value(Fun(Var("x"),Int,
						   Value(Fun(Var("x"),Int,
	ArithExpr(PLUS,x,ArithExpr(PLUS,x,ArithExpr(PLUS,x,ArithExpr(PLUS,x,x)))))))))))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3)))),Value(Concrete(N(4)))),Value(Concrete(N(5)))),Value(Concrete(N(6))))),[],[])));
 (* 30 *)
 
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),
		 Value(Fun(Var("y"),THole(TypeHole(TypeVar("b"))),ArithExpr(PLUS,y,x))))),
	Value(Concrete(N(2)))),Value(Concrete(N(3))))),[],[])));
(* 5, [], ['a->int,'b->int]  *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
		  Case(x,[(x1y2,ArithExpr(PLUS,x,y))]))),
	Value(VRecord([(Lab("1"),Concrete(N(3))),(Lab("2"),Concrete(N(7)))])))),[],[])));
(* 10 *)
	
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),TRecord([(Lab("1"),Bool),(Lab("2"),Bool)]),
		  Value(Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
				Case(x,[(PRecord([(Lab("1"),PVar(Var("b1"))),(Lab("2"),PVar(Var("b2")))]),
					 Case(y,[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
						  Condition(BoolExpr(EQ,Variable(Var("b1")),Variable(Var("b2"))),
									x,y))]))]))))),
	Value(VRecord([(Lab("1"),Concrete(B(true))),(Lab("2"),Concrete(B(true)))]))),
	Value(VRecord([(Lab("1"),Concrete(N(0))),(Lab("2"),Concrete(N(10)))])))),[],[])));
(* 0, [...] *)
  
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),TRecord([(Lab("1"),Bool),(Lab("2"),Bool)]),
		  Value(Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
				Case(x,[(PRecord([(Lab("1"),PVar(Var("b1"))),(Lab("2"),PVar(Var("b2")))]),
					 Case(y,[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
						  Condition(BoolExpr(EQ,Variable(Var("b1")),Variable(Var("b2"))),
									x,y))]))]))))),
	Value(VRecord([(Lab("1"),Concrete(B(true))),(Lab("2"),Concrete(B(false)))]))),
	Value(VRecord([(Lab("1"),Concrete(N(0))),(Lab("2"),Concrete(N(10)))])))),[],[])));
(* 10, [...] *)
  						 
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("x"),TRecord([(Lab("1"),Bool),(Lab("2"),Bool)]),
		  Value(Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
				Case(x,[(PRecord([(Lab("1"),PVar(Var("b1"))),(Lab("2"),PVar(Var("b2")))]),
					 Case(y,[(PRecord([(Lab("1"),PVar(Var("x"))),(Lab("2"),PVar(Var("y")))]),
						  Condition(BoolExpr(EQ,Variable(Var("b1")),Variable(Var("b2"))),
									x,y))]))]))))),
	Value(VRecord([(Lab("1"),ve'),(Lab("2"),vf')]))),
	Condition(Value(vg'),
			  Record([(Lab("1"),Value(va')),(Lab("2"),ArithExpr(PLUS,Value(vb'),Value(Concrete(N(2)))))]),
			  Value(VRecord([(Lab("1"),vc'),(Lab("2"),vc')]))))),[],[])));
	
(* 1, [v['a] -> 1, v['f] -> true, v['e] -> true, v['b] -> 1, v['g] -> true], [...] *)
	

 prettyPrintConfig(evaluate(Config(Expression(Case(
	App(Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Bool)]),Record([(Lab("1"),x),(Lab("2"),x)]))),
		Value(VRecord([(Lab("1"),Concrete(N(2))),(Lab("2"),Concrete(B(true)))]))),
	[(x1y2,
	BoolExpr(EQ,x,y))])),[],[])));
(* true *)

prettyPrintConfig(evaluate(Config(Expression(Record([
	(Lab("x"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2))))))),
	(Lab("y"),App(Value(Fun(Var("x"),Real,Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,y,x))))),Value(Concrete(R(2.0)))))])),
	[],[])));
(* ( fn x;int=>x*2, fn y:real=>y/2.0 ) *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),TRecord([(Lab("a"),TFun(Int,Int)),(Lab("b"),TFun(Real,Real)),(Lab("c"),TFun(Bool,Bool))]),
		  Case(x,[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
			   Record([(Lab("i"),App(x,Value(Concrete(N(2))))),
					   (Lab("j"),App(y,Value(Concrete(R(2.0))))),
					   (Lab("k"),App(z,Value(Concrete(B(true)))))]))]))),
	Record([
		(Lab("a"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2))))))),
		(Lab("b"),App(Value(Fun(Var("x"),Real,Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,y,x))))),Value(Concrete(R(2.0))))),
		(Lab("c"),Value(Fun(Var("y"),Bool,BoolExpr(EQ,y,Value(Concrete(B(true)))))))]))),
	[],[])));
(* {i=4, j=1.0, k=true} ... *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),TRecord([(Lab("a"),TFun(Int,Int)),(Lab("b"),TFun(Real,Real)),(Lab("c"),TFun(Bool,Bool))]),
		  Case(x,[(PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y"))),(Lab("c"),PVar(Var("z")))]),
			   Record([(Lab("i"),App(x,Value(va'))),
					   (Lab("j"),App(y,Value(vb'))),
					   (Lab("k"),App(z,Value(vc')))]))]))),
	Record([
		(Lab("a"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(vd'))))),
		(Lab("b"),App(Value(Fun(Var("x"),Real,Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,y,x))))),Value(ve'))),
		(Lab("c"),Value(Fun(Var("y"),Bool,BoolExpr(EQ,y,Value(va'')))))]))),
	[],[])));
(* {i=1, j=1.0, k=true},
  [v[''a] -> true, v['e] -> 1.0, v['d] -> 1, v['c] -> true, v['b] -> 1.0, v['a] -> 1] *)
	
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("z"),Int,
	Value(Fun(Var("x"),
	TRecord([(Lab("i"),TFun(Int,Int)),
			 (Lab("j"),TRecord([(Lab("x"),TFun(Int,Int)),
							    (Lab("y"),TRecord([(Lab("1"),TFun(Int,Int)),
												   (Lab("2"),TRecord([(Lab("a"),TFun(Int,Int)),
																      (Lab("b"),TFun(Int,Int))]))]))]))]),
	   Case(x,[(PRecord([(Lab("i"),PVar(Var("a"))),(Lab("j"),PVar(Var("b")))]),
			Case(Variable(Var("b")),[(PRecord([(Lab("x"),PVar(Var("c"))),(Lab("y"),PVar(Var("d")))]),
				 Case(Variable(Var("d")),[(PRecord([(Lab("1"),PVar(Var("e"))),(Lab("2"),PVar(Var("f")))]),
					  Case(Variable(Var("f")),[(PRecord([(Lab("a"),PVar(Var("g"))),(Lab("b"),PVar(Var("h")))]),
		Record([(Lab("1"),App(Variable(Var("a")),z)),
				(Lab("2"),App(Variable(Var("c")),z)),
				(Lab("3"),App(Variable(Var("e")),z)),
				(Lab("4"),App(Variable(Var("g")),z)),
				(Lab("5"),App(Variable(Var("h")),z))]))]))]))]))]))))),
	Value(Concrete(N(10)))),
	Record([(Lab("i"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2))))))),
			(Lab("j"),Record([(Lab("x"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(3))))))),
							  (Lab("y"),Record([(Lab("1"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(4))))))),
												(Lab("2"),Record([(Lab("a"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(5))))))),
																  (Lab("b"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(6)))))))]))]))]))]))),
	[],[])));		
(* {1=20,2=30,3=40,4=50,5=60} ... *)

prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(Fun(Var("z"),Int,
	Value(Fun(Var("x"),
	TRecord([(Lab("i"),TFun(Int,Int)),
			 (Lab("j"),TRecord([(Lab("x"),TFun(Int,Int)),
							    (Lab("y"),TRecord([(Lab("1"),TFun(Int,Int)),
												   (Lab("2"),TRecord([(Lab("a"),TFun(Int,Int)),
																      (Lab("b"),TFun(Int,Int))]))]))]))]),
	   Case(x,[(PRecord([(Lab("i"),PVar(Var("a"))),(Lab("j"),PVar(Var("b")))]),
			Case(Variable(Var("b")),[(PRecord([(Lab("x"),PVar(Var("c"))),(Lab("y"),PVar(Var("d")))]),
				 Case(Variable(Var("d")),[(PRecord([(Lab("1"),PVar(Var("e"))),(Lab("2"),PVar(Var("f")))]),
					  Case(Variable(Var("f")),[(PRecord([(Lab("a"),PVar(Var("g"))),(Lab("b"),PVar(Var("h")))]),
		Record([(Lab("1"),App(Variable(Var("a")),z)),
				(Lab("2"),App(Variable(Var("c")),z)),
				(Lab("3"),App(Variable(Var("e")),z)),
				(Lab("4"),App(Variable(Var("g")),z)),
				(Lab("5"),App(Variable(Var("h")),z))]))]))]))]))]))))),
	Value(Concrete(N(10)))),
	Record([(Lab("i"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(va'))))),
			(Lab("j"),Record([(Lab("x"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(vb'))))),
							  (Lab("y"),Record([(Lab("1"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(vc'))))),
												(Lab("2"),Record([(Lab("a"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(vd'))))),
																  (Lab("b"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(ve')))))]))]))]))]))),
	[],[])));	
 (* {1=10,2=10,3=10,4=10,5=10},
	[v['e] -> 1, v['d] -> 1, v['c] -> 1, v['b] -> 1, v['a] -> 1], ... 
*)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),Int,
		  App(Value(Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),x))),
	Value(Concrete(N(2))))),[],[])));
(* 4 *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Real,ArithExpr(TIMES,x,Value(Concrete(R(2.0)))))),
			  Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,y,Value(Concrete(R(2.0))))))),
	App(Value(Fun(Var("x"),Real,ArithExpr(PLUS,x,x))),
		Condition(Value(va'),
				  Value(Concrete(R(2.0))),Value(Concrete(R(4.0))))))),[],[])));
(* 8.0 *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Condition(Value(va'),
			  Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2)))))),
			  Value(Fun(Var("y"),Real,ArithExpr(DIVIDE,y,Value(Concrete(R(2.0))))))),
	App(Value(Fun(Var("x"),Real,ArithExpr(PLUS,x,x))),
		Condition(Value(Concrete(B(false))),
				  Value(Concrete(N(2))),Value(Concrete(R(4.0))))))),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'),Value(Concrete(N(2))))),[],[])));
(* v['a1236],
   [v['a] -> fn x:int => v['a1236]] *)
   
 prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'),
	Value(VRecord([(Lab("i"),Concrete(N(2))),
				   (Lab("j"),Concrete(R(2.0))),
				   (Lab("k"),Concrete(B(false)))])))),[],[])));
(* v['1240],
   [v['a] -> fn x:{i:int,j:real,k:bool} => v['a1240]] *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'),Value(vb'))),[],[])));
(* v['a1246],
  [v['a]-> fn x:'b => v['a1246]] *)
   
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'),Value(va'))),[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'),
	Value(VRecord([(Lab("i"),Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),
				   (Lab("j"),Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Bool)]),
								 Case(y,[(x1y2,Condition(y,x,Value(Concrete(N(2)))))])))])))),
	[],[])));
(* v['a1641],
 [v['a]->fn x:{i:(int->int),j:({1:int,2:bool}->int)} => v['a1641] *)
  
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va''),
	Value(VRecord([(Lab("i"),Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),
				   (Lab("j"),Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Bool)]),
								 Case(y,[(x1y2,Condition(y,x,Value(Concrete(N(2)))))])))])))),
	[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(va'''),
	Value(VRecord([(Lab("i"),Fun(Var("x"),Int,ArithExpr(PLUS,x,x))),
				   (Lab("j"),Fun(Var("y"),TRecord([(Lab("1"),Int),(Lab("2"),Bool)]),
								 Case(y,[(x1y2,Condition(y,x,Value(Concrete(N(2)))))])))])))),
	[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(App(App(
	Value(va'),Value(Concrete(N(2)))),Value(Concrete(N(2))))),[],[])));
(* v['a1272],
  [v['a]-> fn x:int => v['a1268], v['a1268]-> fn x:int => v['a1272]] *)
  
prettyPrintConfig(evaluate(Config(Expression(App(App(App(
	Value(va'),
		Value(Fun(Var("x"),THole(TypeHole(ArithTypeVar("a"))),ArithExpr(PLUS,x,x)))),
			Value(VRecord([(Lab("i"),Concrete(N(2))),
						   (Lab("j"),Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
										 BoolExpr(EQ,y,Value(Concrete(B(true))))))]))),
				Value(vc'))),[],[])));
(* v['a1674]
  [v['a1667] -> fn x:'c => v['a1674],
   v['a1662] -> fn x:{i:int, j:(bool -> bool)} => v['a1667],
   v['a] -> fn x:('''a -> '''a) => v['a1662]] *)
    
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vb'),x))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),ArithExpr(TIMES,y,y))))),
	[],[])));
(* v['a1686],
  [v['a1677] -> v['a1686], 
   v['b] -> fn x:(int -> int) => v['a1677]] *)
  
prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vc'),App(Value(vb'),x)))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),ArithExpr(TIMES,y,y))))),
	[],[])));
(* v['a1701],
  [v['a1687] -> v['a1701], v['a1690] -> v['a1700], v['b] -> fn x:(int -> int) => v['a1690],
   v['c] -> fn x:'a1690 => v['a1687]] *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vc'),App(x,Value(vb'))))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),ArithExpr(TIMES,y,y))))),
	[],[])));
(* v['a1711],
  [v['a1702] -> v['a1711], v['b] -> 1, v['c] -> fn x:int => v['a1702]] *)
  
 prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vc'),App(x,Value(vb'))))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
			   Condition(Value(va''),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(TIMES,x,y))]))),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(SUBTRACT,x,y))])))))))),[],[])));
(* v['a1747],
  [v['a1716] -> v['a1747], v[''a] -> true, v['b] -> true, v['c] -> fn x:({1:int, 2:int} -> int) => v['a1716]] *)  
  
 prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vc'),App(x,Value(vb'))))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
			   Condition(Value(va''),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(TIMES,x,y))]))),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Real),(Lab("2"),Real)]),
									Case(x,[(x1y2,ArithExpr(DIVIDE,x,y))])))))))),[],[])));
(* Stuck *)

 prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(App(Value(vc'),Value(vd')),App(x,Value(vb'))))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
			   Condition(Value(va''),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(TIMES,x,y))]))),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(SUBTRACT,x,y))])))))))),[],[])));
(* v['a1817],
  [v['a1796] -> v['a1817], v[''a] -> true, v['a1786] -> v['a1796], v['b] -> true,
   v['c] -> fn x:'d => fn x:({1:int, 2:int} -> int) => v['a1786]] *)
   
   prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(App(Value(vc'),App(x,Value(vb'))),Value(vd')))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
			   Condition(Value(va''),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(TIMES,x,y))]))),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(SUBTRACT,x,y))])))))))),[],[])));
(* v['a1862],
  [v['a1858] -> v['a1862], v['a1839] -> v['a1858], v[''a] -> true, v['d] -> v['a1838], v['b] -> true,
   v['c] -> fn x:({1:int, 2:int} -> int) => fn x:'a1838 => v['a1839]] *)
 
   prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),THole(TypeHole(TypeVar("a"))),App(Value(vc'),App(App(x,Value(vb')),Value(vd''))))),
	Value(Fun(Var("y"),THole(TypeHole(EqualityTypeVar("a"))),
			   Condition(Value(va''),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(TIMES,x,y))]))),
						 Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),Int)]),
									Case(x,[(x1y2,ArithExpr(SUBTRACT,x,y))])))))))),[],[])));
(* v['a1903],
  [v['a1867] -> v['a1903], v[''a] -> true, v[''d] -> {1=1, 2=1}, v['b] -> true, v['c] -> fn x:int => v['a1867]] *)

 prettyPrintConfig(evaluate(Config(Expression(Record([
 
	(Lab("x"),App(Value(Fun(Var("x"),TRecord([(Lab("1"),Int),(Lab("2"),TFun(Int,TFun(Bool,Bool)))]),Case(x,[(x1y2,App(y,x))]))),
		App(Value(va'),Value(Concrete(N(2)))))),
		
	(Lab("y"),Condition(
		App(Value(vb'),Value(Fun(Var("x"),TRecord([(Lab("1"),TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),Int)])),
											 	   (Lab("2"),TRecord([(Lab("a"),Int),(Lab("b"),Bool),(Lab("c"),Int)]))]),
							 Case(x,[(x1y2,BoolExpr(EQ,x,y))])))),
		App(Value(vc'),Value(VRecord([(Lab("a"),Concrete(N(2))),(Lab("b"),Concrete(B(true)))]))),
		App(Value(vd'),Value(VRecord([(Lab("a"),Concrete(B(false))),(Lab("b"),Concrete(R(2.0)))])))))])),
	
	[],[])));
(* {x=fn x1937:bool => true, y=v['a1952]},
  [v['c] -> fn x:{a:int, b:bool} => v['a1952], v['a1939] -> true,
   v['b] -> fn x:({1:{a:int, b:bool, c:int}, 2:{a:int, b:bool, c:int}} -> bool) => v['a1939],
   v['a1904] -> {1=1, 2=fn x:int => fn x:bool => true}, 
   v['a] -> fn x:int => v['a1904]] *)

prettyPrintConfig(evaluate(Config(Expression(App(
	Value(Fun(Var("x"),TRecord([(Lab("a"),TFun(Int,Bool)),
								(Lab("b"),TFun(Int,Int)),
								(Lab("c"),Int),
								(Lab("1"),Bool),
								(Lab("d"),TFun(Int,Real)),
								(Lab("e"),TFun(Int,Real)),
								(Lab("x"),Int),
								(Lab("2"),Int),
								(Lab("y"),TRecord([(Lab("1"),Int)]))]),
		  Case(x,[(PRecord([(Lab("a"),PVar(Var("x"))),
						  (Lab("b"),PVar(Var("y"))),
						  (Lab("c"),PVar(Var("z"))),
						  (Lab("d"),PVar(Var("a"))),
						  (Lab("e"),PVar(Var("b"))),
						  (Lab("y"),PWildcard),
						  (Lab("1"),PVal(B(false))),
						  (Lab("2"),PVal(N(6))),
						  (Lab("x"),PVal(N(2)))]),
			   App(Value(Fun(Var("x"),Real,ArithExpr(TIMES,x,Value(Concrete(R(2.0)))))),
				   Condition(App(x,App(y,z)),
							 App(a,z),
							 App(b,z))))]))),
	Record([(Lab("a"),Value(Fun(Var("x"),Int,
							Condition(BoolExpr(LESS_EQ,x,Value(Concrete(N(10)))),
									  BoolExpr(MORE_EQ,x,Value(Concrete(N(0)))),
									  Value(Concrete(B(false))))))),
			(Lab("b"),Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2))))))),
			(Lab("c"),Value(Concrete(N(5)))),
			(Lab("d"),Value(Fun(Var("x"),Int,Condition(BoolExpr(MORE_EQ,x,Value(Concrete(N(0)))),
													   Value(Concrete(R(1.0))),
													   Value(Concrete(R(~1.0))))))),
			(Lab("e"),Value(Fun(Var("x"),Int,Condition(BoolExpr(MORE_EQ,x,Value(Concrete(N(100)))),
													   Value(Concrete(R(5.0))),
													   Value(Concrete(R(~5.0))))))),
			(Lab("y"),Value(VRecord([(Lab("1"),Concrete(N(5)))]))),
			(Lab("1"),App(Value(Fun(Var("x"),Int,BoolExpr(EQ,x,Value(Concrete(N(3)))))),Value(Concrete(N(2))))),
			(Lab("2"),App(Case(App(Value(Fun(Var("x"),Int,BoolExpr(LESS,x,Value(Concrete(N(100)))))),Value(Concrete(N(10)))),
							   [(PVar(Var("x")),
							   Condition(x,Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(2)))))),
										   Value(Fun(Var("x"),Int,ArithExpr(TIMES,x,Value(Concrete(N(3))))))))]),
						 Value(Concrete(N(3))))),
			(Lab("x"),Value(Concrete(N(2))))]))),
	[],[])));				  
(* 2.0 *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(1))),
	[(PVal(N(0)),Value(Concrete(B(true)))),
	 (PVal(N(2)),Value(Concrete(B(true)))),
	 (PVal(N(1)),Value(Concrete(B(false))))])),[],[])));
(* false *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(1))),
	[(PVal(N(0)),Value(Concrete(B(true)))),
	 (PVal(N(2)),Value(Concrete(B(true)))),
	 (PWildcard,Value(Concrete(B(false))))])),[],[])));	 
(* false *)
	 
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(1))),
	[(PVal(N(0)),Value(Concrete(B(true)))),
	 (PVal(N(2)),Value(Concrete(B(true)))),
	 (PVar(Var("x")),Value(Concrete(N(1))))])),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(N(1))),
	[(PVal(N(0)),Value(Concrete(B(true)))),
	 (PVal(N(2)),Value(Concrete(B(true)))),
	 (PVar(Var("x")),Value(Concrete(B(false))))])),[],[])));
(* false *)
	 
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(N(2)))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PVal(N(0)))]),Value(Concrete(N(0)))),
	 (PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVal(N(1)))]),Value(Concrete(N(1)))),
	 (PRecord([(Lab("b"),PVar(Var("x"))),(Lab("a"),PVar(Var("y")))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))])),[],[])));
(* 2 *)
 
 prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(N(2)))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PVal(N(0)))]),Value(Concrete(N(0)))),
	 (PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVal(N(1)))]),Value(Concrete(N(1)))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("c"),PVar(Var("y")))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y"))))])),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(1))),(Lab("b"),Concrete(N(2)))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PVal(N(0)))]),Value(Concrete(N(0)))),
	 (PRecord([(Lab("a"),PVal(N(1))),(Lab("b"),PVal(N(1)))]),Value(Concrete(N(1)))),
	 (PRecord([(Lab("a"),PVar(Var("x"))),(Lab("b"),PVar(Var("y")))]),
	  ArithExpr(TIMES,Variable(Var("x")),Variable(Var("y")))),
	 (PVal(N(2)),Value(Concrete(N(2))))])),[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(2))),
				   (Lab("b"),Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),VRecord([(Lab("1"),Concrete(N(0)))])),
				   (Lab("d"),Concrete(B(false))),
				   (Lab("e"),Fun(Var("y"),Bool,Condition(Variable(Var("y")),Value(Concrete(N(1))),Value(Concrete(N(~1))))))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PWildcard),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVar(Var("y"))),(Lab("e"),PVar(Var("f")))]),
	  App(Variable(Var("f")),Variable(Var("y")))),
	  
	 (PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVar(Var("f1"))),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVal(B(false))),(Lab("e"),PVar(Var("f2")))]),
	  ArithExpr(TIMES,App(Variable(Var("f2")),Value(Concrete(B(false)))),
					  App(Variable(Var("f1")),Value(Concrete(N(3))))))])),[],[])));
(* -9 *)
 
 prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VRecord([(Lab("a"),Concrete(N(2))),
				   (Lab("b"),Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),VRecord([(Lab("1"),Concrete(N(0)))])),
				   (Lab("d"),Concrete(B(false))),
				   (Lab("e"),Fun(Var("y"),Bool,Condition(Variable(Var("y")),Value(Concrete(N(1))),Value(Concrete(N(~1))))))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PWildcard),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVar(Var("y"))),(Lab("e"),PVar(Var("f")))]),
	  Value(Fun(Var("y"),Bool,App(Variable(Var("f")),Variable(Var("y")))))),
	  
	 (PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVar(Var("f1"))),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVal(B(false))),(Lab("e"),PVar(Var("f2")))]),
	  Value(Fun(Var("x"),Bool,ArithExpr(TIMES,App(Variable(Var("f2")),Variable(Var("x"))),
										      App(Variable(Var("f1")),Value(Concrete(N(3))))))))])),[],[])));
(* fn x:bool => (fn y:bool => if y then 1 else ~1) x * (fn x:int => x*x) (3) *)

 prettyPrintConfig(evaluate(Config(Expression(App(Case(
	Value(VRecord([(Lab("a"),Concrete(N(2))),
				   (Lab("b"),Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),VRecord([(Lab("1"),Concrete(N(0)))])),
				   (Lab("d"),Concrete(B(false))),
				   (Lab("e"),Fun(Var("y"),Bool,Condition(Variable(Var("y")),Value(Concrete(N(1))),Value(Concrete(N(~1))))))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PWildcard),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVar(Var("y"))),(Lab("e"),PVar(Var("f")))]),
	  Value(Fun(Var("y"),Bool,App(Variable(Var("f")),Variable(Var("y")))))),
	  
	 (PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVar(Var("f1"))),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVal(B(false))),(Lab("e"),PVar(Var("f2")))]),
	  Value(Fun(Var("x"),Bool,ArithExpr(TIMES,App(Variable(Var("f2")),Variable(Var("x"))),
										      App(Variable(Var("f1")),Value(Concrete(N(3))))))))]),
	App(App(
	Value(Fun(Var("xx"),Int,
		Value(Fun(Var("yy"),Real,
			Case(Record([(Lab("i"),Value(Fun(Var("x"),Int,Variable(Var("x"))))),
				 		 (Lab("j"),Value(Fun(Var("z"),Real,Value(Concrete(N(1))))))]),
				 [(PRecord([(Lab("i"),PVar(Var("a"))),(Lab("j"),PVar(Var("b")))]),
				   BoolExpr(EQ,App(Variable(Var("a")),Variable(Var("xx"))),
							   App(Variable(Var("b")),Variable(Var("yy")))))]))))),
	Value(Concrete(N(2)))),Value(Concrete(R(1.0)))))),
	[],[])));
(* ~9 *) 

 prettyPrintConfig(evaluate(Config(Expression(App(Case(
	Value(VRecord([(Lab("a"),vc'),
				   (Lab("b"),Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
				   (Lab("c"),VRecord([(Lab("1"),vd''')])),
				   (Lab("d"),Concrete(B(false))),
				   (Lab("e"),Fun(Var("y"),Bool,Condition(Variable(Var("y")),Value(Concrete(N(1))),Value(Concrete(N(~1))))))])),
	[(PRecord([(Lab("a"),PVal(N(0))),(Lab("b"),PWildcard),(Lab("c"),PRecord([(Lab("1"),PVal(N(10)))])),
			   (Lab("d"),PVal(B(true))),(Lab("e"),PVar(Var("f")))]),
	  Value(Fun(Var("y"),Bool,App(Variable(Var("f")),Variable(Var("y")))))),
	  
	 (PRecord([(Lab("a"),PVal(N(2))),(Lab("b"),PVar(Var("f1"))),(Lab("c"),PRecord([(Lab("1"),PVal(N(0)))])),
			   (Lab("d"),PVal(B(false))),(Lab("e"),PVar(Var("f2")))]),
	  Value(Fun(Var("x"),Bool,ArithExpr(TIMES,App(Variable(Var("f2")),Variable(Var("x"))),
										      App(Variable(Var("f1")),Value(Concrete(N(3))))))))]),
	App(App(
	Value(Fun(Var("xx"),Int,
		Value(Fun(Var("yy"),Real,
			Case(Record([(Lab("i"),Value(Fun(Var("x"),Int,Variable(Var("x"))))),
				 		 (Lab("j"),Value(Fun(Var("z"),Real,Value(Concrete(N(1))))))]),
				 [(PRecord([(Lab("i"),PVar(Var("a"))),(Lab("j"),PVar(Var("b")))]),
				   BoolExpr(EQ,App(Variable(Var("a")),Variable(Var("xx"))),
							   App(Variable(Var("b")),Variable(Var("yy")))))]))))),
	Value(va')),Value(vb')))),
	[],[])));
(* 9, 
  [v['a]->1, v['b]->1.0,v['c]->v['a112],v['a112]->2, v['''d]->0] *)
 
 prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("a"),App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
						  App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
							  Value(Concrete(N(2)))))),
			 (Lab("b"),Value(Fun(Var("x"),Bool,
							 Condition(Variable(Var("x")),
									   Value(Fun(Var("x"),Int,Condition(BoolExpr(LESS,Variable(Var("x")),Value(Concrete(N(0)))),	
																		Value(Concrete(R(1.0))),Value(Concrete(R(~1.0)))))),
									   Value(Fun(Var("x"),Int,Condition(BoolExpr(MORE,Variable(Var("x")),Value(Concrete(N(10)))),
																	    Value(Concrete(R(2.0))),Value(Concrete(R(~2.0)))))))))),
			 (Lab("c"),Record([(Lab("1"),Record([]))])),
			 (Lab("d"),Case(Value(Concrete(N(2))),
						    [(PVal(N(0)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2))))))),
							 (PVal(N(1)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(4))))))),
							 (PVal(N(2)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))))])),
			 (Lab("e"),Value(VRecord([(Lab("i"),Concrete(N(10))),(Lab("j"),Concrete(R(10.0))),(Lab("k"),Concrete(B(true))),
								      (Lab("l"),Fun(Var("y"),Real,BoolExpr(MORE,Variable(Var("y")),Value(Concrete(R(0.0)))))),
								      (Lab("m"),VRecord([(Lab("1"),VRecord([(Lab("2"),Concrete(N(0)))]))]))])))]),
	[(PRecord([(Lab("a"),PVal(N(16))),(Lab("b"),PWildcard),(Lab("c"),PRecord([(Lab("1"),PRecord([]))])),
			   (Lab("d"),PVar(Var("x"))),(Lab("e"),PVar(Var("y")))]),
	  Case(Variable(Var("y")),
		  [(PRecord([(Lab("i"),PVar(Var("val"))),(Lab("k"),PWildcard),(Lab("j"),PWildcard),
					 (Lab("l"),PWildcard),(Lab("m"),PRecord([(Lab("1"),PRecord([(Lab("2"),PVal(N(0)))]))]))]),
			App(Variable(Var("x")),Variable(Var("val"))))]))])),[],[])));
(* case { a = (fn x:int => x*x) ((fn x:int => x*x) (2)),
		  b = (fn x:bool => if x then (fn x:int => if x<0 then 1.0 else ~1.0) else (fn x:int => if x>10 then 2.0 else ~2.0)),
		  c = { 1 = { } },
		  d = case 2 of 0 -> (fn x:int => x*2) | 1 -> (fn x:int => x*4) | 2 -> (fn x:int => x*10),
		  e = { i = 10, j = 10.0, k = true, l = (fn y:real => y > 0.0), m = { 1 = { 2 = 0 } } } }
   
   of { a = 16, b = _, c = { 1 = { } }, d = x, e = y } -> case y of { i = val, k = _, j = _, l = _, m = { 1 = { 2 = 0 } } } -> (x val)
   
   =>
   100 *)
		  
 prettyPrintConfig(evaluate(Config(Expression(Case(
	Record([(Lab("a"),App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
						  App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
							  Value(Concrete(N(2)))))),
			 (Lab("b"),Value(Fun(Var("x"),Bool,
							 Condition(Variable(Var("x")),
									   Value(Fun(Var("x"),Int,Condition(BoolExpr(LESS,Variable(Var("x")),Value(Concrete(N(0)))),	
																		Value(Concrete(R(1.0))),Value(Concrete(R(~1.0)))))),
									   Value(Fun(Var("x"),Int,Condition(BoolExpr(MORE,Variable(Var("x")),Value(Concrete(N(10)))),
																	    Value(Concrete(R(2.0))),Value(Concrete(R(~2.0)))))))))),
			 (Lab("c"),Record([(Lab("1"),Record([]))])),
			 (Lab("d"),Case(Value(Concrete(N(2))),
						    [(PVal(N(0)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(2))))))),
							 (PVal(N(1)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(4))))))),
							 (PVal(N(2)),Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(10)))))))])),
			 (Lab("e"),Value(VRecord([(Lab("i"),Concrete(N(10))),(Lab("j"),Concrete(R(10.0))),(Lab("k"),Concrete(B(true))),
								      (Lab("l"),Fun(Var("y"),Real,BoolExpr(MORE,Variable(Var("y")),Value(Concrete(R(0.0)))))),
								      (Lab("m"),VRecord([(Lab("1"),VRecord([(Lab("2"),Concrete(N(0)))]))]))])))]),
	[(PRecord([(Lab("a"),PVal(N(16))),(Lab("b"),PWildcard),(Lab("c"),PRecord([])),(Lab("d"),PVar(Var("x"))),(Lab("e"),PVar(Var("y")))]),
	  Case(Variable(Var("y")),[(PRecord([(Lab("i"),PVar(Var("val"))),(Lab("k"),PWildcard),(Lab("j"),PWildcard),
										 (Lab("l"),PWildcard),(Lab("m"),PRecord([(Lab("1"),PRecord([(Lab("2"),PVal(N(0)))]))]))]),
								App(Variable(Var("x")),Variable(Var("val"))))])),
	 (PRecord([(Lab("b"),PVar(Var("y"))),(Lab("a"),PVar(Var("x"))),(Lab("c"),PRecord([(Lab("1"),PRecord([]))])),
			   (Lab("d"),PVar(Var("z"))),
			   (Lab("e"),PRecord([(Lab("m"),PVar(Var("f"))),(Lab("i"),PVal(N(10))),(Lab("k"),PVar(Var("a"))),
								  (Lab("j"),PWildcard),(Lab("l"),PVar(Var("g")))]))]),
	  Value(Concrete(N(10))))
	 (* App(App(Variable(Var("y")),Variable(Var("a"))),ArithExpr(TIMES,Variable(Var("x")),Value(Concrete(N(~1))))) *)
	 
								])),[],[])));
(* case { a = (fn x:int => x*x) ((fn x:int => x*x) (2)),
		  b = (fn x:bool => if x then (fn x:int => if x<0 then 1.0 else ~1.0) else (fn x:int => if x>10 then 2.0 else ~2.0)),
		  c = { 1 = { } },
		  d = case 2 of 0 -> (fn x:int => x*2) | 1 -> (fn x:int => x*4) | 2 -> (fn x:int => x*10),
		  e = { i = 10, j = 10.0, k = true, l = (fn y:real => y > 0.0), m = { 1 = { 2 = 0 } } } }
   
   of { a = 16, b = _, c = { { } }, d = x, e = y } -> case y of { i = val, k = _, j = _, l = _, m = { 1 = { 2 = 0 } } } -> (x val)
   |  { b = y, a = x, c = { 1 = { } }, d = z, e = { m = f, i = 10, k = a, j = _, l = g} } 
			-> (y a) (x * ~1)
   
   =>
   100 *)
 
(evaluate(Config(Expression(LetRec(
Var("x"),TFun(Int,Int),
Fun(Var("y"),Int,Condition(BoolExpr(LESS_EQ,Variable(Var("y")),Value(Concrete(N(1)))),
						   Value(Concrete(N(1))),
						   ArithExpr(PLUS,App(Variable(Var("x")),ArithExpr(SUBTRACT,Variable(Var("y")),Value(Concrete(N(1))))),
										  App(Variable(Var("x")),ArithExpr(SUBTRACT,Variable(Var("y")),Value(Concrete(N(2)))))))),
App(Variable(Var("x")),Value(Concrete(N(11)))))),[],[])));
(* let val rec x:(int->int) = (fn y:int => if y<=1 then 1 else x(y-1)+x(y-2)) in (x 11) end 
   i.e. 11th fibonacci number => 144 *)
   
(evaluate(Config(Expression(LetRec(
Var("x"),TFun(Int,Int),
Fun(Var("y"),Int,Condition(BoolExpr(LESS_EQ,Variable(Var("y")),Value(Concrete(N(0)))),
						   Value(Concrete(N(1))),
						   ArithExpr(TIMES,Variable(Var("y")),
										   App(Variable(Var("x")),ArithExpr(SUBTRACT,Variable(Var("y")),Value(Concrete(N(1)))))))),
App(Variable(Var("x")),Value(Concrete(N(10)))))),[],[])));
(* let val rec x:(int->int) = (fn y:int => if y<=0 then 1 else y*(x (y-1)) in (x 10) end
   i.e. factorial of 10 => 3628800 *)
   
(* Takes a long time 
(evaluate(Config(Expression(LetRec(
Var("z"),TFun(TRecord([(Lab("x"),Int),(Lab("y"),Int)]),Int),
Fun(Var("x"),TRecord([(Lab("x"),Int),(Lab("y"),Int)]),
    Case(Variable(Var("x")),
	    [(PRecord([(Lab("x"),PVar(Var("x"))),(Lab("y"),PVar(Var("y")))]),
		  Condition(BoolExpr(EQ,Variable(Var("x")),Value(Concrete(N(0)))),
					ArithExpr(PLUS,Variable(Var("y")),Value(Concrete(N(1)))),
					Condition(BoolExpr(EQ,Variable(Var("y")),Value(Concrete(N(0)))),
							  App(Variable(Var("z")),Record([(Lab("x"),ArithExpr(SUBTRACT,Variable(Var("x")),Value(Concrete(N(1))))),(Lab("y"),Value(Concrete(N(1))))])),
							  App(Variable(Var("z")),Record([(Lab("x"),ArithExpr(SUBTRACT,Variable(Var("x")),Value(Concrete(N(1))))),
															 (Lab("y"),App(Variable(Var("z")),Record([(Lab("x"),Variable(Var("x"))),
																									  (Lab("y"),ArithExpr(SUBTRACT,Variable(Var("y")),Value(Concrete(N(1)))))])))])))))])),
App(Variable(Var("z")),Record([(Lab("x"),Value(Concrete(N(3)))),(Lab("y"),Value(Concrete(N(3))))])))),[],[])));
*)
(* ackermann(3,3) => 61 *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VList([Concrete(N(1)),Concrete(N(2)),Concrete(N(3))])),
	[(PVal(EmptyList),Value(Concrete(N(1)))),
	 (PCons(PVar(Var("x")),PVar(Var("y"))),Value(Concrete(N(2))))])),[],[])));
(* 2 *)
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(Concrete(EmptyList)),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PWildcard,Value(Concrete(B(false))))])),[],[])));
(* true *)
	 
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VList([Concrete(N(1))])),
	[(PVal(EmptyList),Value(Concrete(B(true)))),
	 (PWildcard,Value(Concrete(B(false))))])),[],[])));
(* false *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(VList([Concrete(N(1)),Concrete(N(2)),Concrete(N(3))])),
	[(PVal(EmptyList),Value(Concrete(N(0)))),
	 (PCons(PVal(N(1)),PCons(PVal(N(3)),PVar(Var("x")))),Value(Concrete(N(4)))),
	 (PCons(PVal(N(1)),PCons(PVal(N(2)),PCons(PVal(N(3)),PVar(Var("x"))))),Value(Concrete(N(6))))])),[],[])));
(* 6 *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	List([ArithExpr(TIMES,Value(Concrete(N(2))),Value(Concrete(N(3)))),
		  App(Value(Fun(Var("x"),Int,ArithExpr(TIMES,Variable(Var("x")),Variable(Var("x"))))),
			  App(Value(Fun(Var("y"),Int,ArithExpr(TIMES,Variable(Var("y")),Variable(Var("y"))))),
				  Value(Concrete(N(2))))),
		  Case(Condition(Value(Concrete(B(true))),Value(VList([Concrete(N(10))])),Value(Concrete(EmptyList))),
			 [(PVal(EmptyList),Value(Concrete(N(0)))),
			  (PCons(PVal(N(1)),PWildcard),Value(Concrete(N(1)))),
			  (PCons(PVar(Var("x")),PWildcard),Variable(Var("x")))])]),
  [(PVal(EmptyList),Value(VRecord([(Lab("a"),Concrete(N(~1))),(Lab("b"),Concrete(EmptyList))]))),
   (PCons(PVar(Var("x")),PVar(Var("y"))),Record([(Lab("a"),Variable(Var("x"))),(Lab("b"),Variable(Var("y")))]))])),[],[])));
(* {a=6, b=[16,10]} *)

prettyPrintConfig(evaluate(Config(Expression(LetRec(
	Var("x"),TFun(TList(THole(TypeHole(TypeVar("a")))),Int),
	Fun(Var("l"),TList(THole(TypeHole(TypeVar("a")))),
		Case(Variable(Var("l")),
		  [(PVal(EmptyList),Value(Concrete(N(0)))),
		   (PCons(PWildcard,PVar(Var("rest"))),
		    ArithExpr(PLUS,Value(Concrete(N(1))),App(Variable(Var("x")),Variable(Var("rest")))))])),
	App(Variable(Var("x")),
	    Value(VList([Concrete(N(0)),Concrete(N(2)),Concrete(N(~1))]))))),[],[])));
(* Length of list *)
 
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)