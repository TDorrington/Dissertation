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
	ArithExpr(PLUS,Value(N(3)),Value(N(4)))),[],[],[])));
(* 7, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(N(3)),Value(N(4)))),[],[],[])));
(* 12, [], [] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(N(3)),Value(N(4)))),[],[],[])));
(* ~1, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(N(3)),Value(N(4)))),[],[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* 7.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* 12.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* ~1.0, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* 0.75, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(R(3.0)),Value(N(4)))),[],[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(R(3.0)),Value(B(true)))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(B(false)),Value(B(false)))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(N(3)),Value(ValuePair(N(4),N(5))))),[],[],[])));
(* Stuck, [], [] *)	

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(N(3)),Value(R(4.0)))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(N(3)),Value(N(4))),
		ArithExpr(TIMES,Value(N(5)),Value(N(2))))),[],[],[])));
(* 9, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(N(10)),Value(N(4))),
			ArithExpr(PLUS,Value(N(3)),Value(N(~1)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(N(3)),Value(N(4))),
			ArithExpr(SUBTRACT,Value(N(3)),Value(N(2)))))),[],[],[])));
(* [(10+4)-(3+-1)] + [(3*4)*(3-2)] => 24, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(N(10)),Value(N(4))),
			ArithExpr(PLUS,Value(N(3)),Value(N(~1)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(N(3)),Value(N(4))),
			ArithExpr(SUBTRACT,Value(N(3)),Value(R(2.0)))))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(PLUS,Value(R(10.0)),Value(R(4.0))),
			ArithExpr(PLUS,Value(R(3.0)),Value(R(~1.0)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0))),
			ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(2.0)))))),[],[],[])));
(* 24.0, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(R(10.0)),Value(R(5.0))),
			ArithExpr(DIVIDE,Value(R(3.0)),Value(R(1.0)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0))),
			ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(2.0)))))),[],[],[])));
(* 11.0, [], [] *)


prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(N(3)),Value(N(4)))),[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,Value(N(2)),Value(N(4)))),[],[],[])));
(* true, [], [] *) 

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,Value(N(3)),Value(N(4)))),[],[],[])));
(* false, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(N(3)),Value(N(4)))),[],[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(N(4)),Value(N(4)))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(N(3)),Value(N(4)))),[],[],[])));
(* false, [], [] *)	

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* false, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(R(3.0)),Value(R(4.0)))),[],[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(R(3.0)),Value(N(4)))),[],[],[])));
(* Stuck, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,Value(N(3)),Value(B(true)))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE_EQ,Value(B(false)),Value(R(3.0)))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(R(10.0)),Value(R(5.0))),
			ArithExpr(DIVIDE,Value(R(3.0)),Value(R(1.0)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0))),
			ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(2.0)))))),[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(MORE,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(R(10.0)),Value(R(5.0))),
			ArithExpr(DIVIDE,Value(R(3.0)),Value(R(1.0)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0))),
			ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(2.0)))))),[],[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS_EQ,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(N(2)),Value(N(4))),
			ArithExpr(SUBTRACT,Value(N(3)),Value(N(3)))),
		ArithExpr(TIMES,
			ArithExpr(PLUS,Value(N(3)),Value(N(1))),
			ArithExpr(PLUS,Value(N(1)),Value(N(1)))))),[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(PLUS,
			ArithExpr(TIMES,Value(N(2)),Value(N(4))),
			ArithExpr(SUBTRACT,Value(N(3)),Value(N(3)))),
		ArithExpr(TIMES,
			ArithExpr(PLUS,Value(N(3)),Value(N(1))),
			ArithExpr(PLUS,Value(N(1)),Value(N(1)))))),[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,
			ArithExpr(DIVIDE,Value(R(10.0)),Value(R(5.0))),
			ArithExpr(DIVIDE,Value(R(3.0)),Value(R(1.0)))),
		ArithExpr(TIMES,
			ArithExpr(TIMES,Value(R(3.0)),Value(R(4.0))),
			ArithExpr(SUBTRACT,Value(R(3.0)),Value(R(2.0)))))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(ValuePair(N(3),B(true))),
		Value(ValuePair(N(3),B(true))))),[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(LESS,
		Value(ValuePair(N(3),B(true))),
		Value(ValuePair(N(4),B(true))))),[],[],[])));
(* Stuck, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(ValuePair(N(3),B(true))),
		Value(ValuePair(N(4),B(true))))),[],[],[])));
(* false, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(ValuePair(ValuePair(N(5),B(false)),ValuePair(N(3),B(true)))),
		Value(ValuePair(ValuePair(N(5),B(false)),ValuePair(N(3),B(true)))))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		Value(ValuePair(N(3),B(true))),
		ExpressionPair(Value(N(3)),Value(B(true))))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,Value(B(true)),Value(B(true)))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(N(4)))),[],[],[])));
(* 5, [v['a]->1], ['a->Int] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(N(4)))),[],[],[])));
(* 4, [v[''a]->1], [''a->Int] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'''),Value(N(4)))),[],[],[])));
(* ~3, [v['''a]->1], ['''a->Int] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'),Value(N(4)))),[],[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(R(3.0)),Value(va'))),[],[],[])));
(* 4.0, [v['a]->1.0], ['a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(R(4.0)))),[],[],[])));
(* Stuck *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(R(3.0)),Value(va'''))),[],[],[])));
(* 2.0, [v['''a]->1.0], ['''a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'''),Value(R(4.0)))),[],[],[])));
(* 0.25, [v['''a]->1.0], ['''a->Real] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(vb'))),[],[],[])));
(* v['''a0 + '''a0], 
  [v['a]->v['''a0], v['b]->v['''a0]],
  ['a->'''a0, 'b->'''a0] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va'),Value(va'))),[],[],[])));
(* v['''a0 * '''a0], 
  [v['a]->v['''a0]], 
  ['a->'''a0] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'),Value(va''))),[],[],[])));
(* v['''a0 - 1],
  [v['a]->v['''a0], v[''a]->1],
  ['a->'''a0,'''a0->Int,''a->Int] *)  
(* TO CHECK !!!!! *)
	
prettyPrintConfig(evaluate(Config(Expression(
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va''),Value(vb'')),
			ArithExpr(PLUS,Value(vc''),Value(vd'')))),[],[],[])));
(* 4,
   [v[''d]->1,v[''c]->1,v[''b]->1,v[''a]->1]
   [''b->int,''a64->int,''c->int,''d->int,''a->int,''a63->int] *)
   
 prettyPrintConfig(evaluate(Config(Expression(
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(vc'''),Value(vd''')))),[],[],[])));
(* v['''a7+'''a7+'''a7+'''a7],
   [v['''a6] -> v['''a7], v['''a5] -> v['''a7], v['''d] -> v['''a6], v['''c] -> v['''a6], v['''b] -> v['''a5], v['''a] -> v['''a5]]
   ['''a6 -> '''a7, '''a5 -> '''a7, '''d -> '''a6, '''c -> '''a6, '''b -> '''a5, '''a -> '''a5] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va'),Value(va'))),[],[],[])));
(* 1.0, 
  [v['a]->1.0], 
  ['a->Real] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,Value(va'),Value(va'''))),[],[],[])));
(* v['''a0 + '''a0],
  [v['a]->v['''a0],v['''a]->v['''a0]],
  ['a->'''a0,'''a->'''a0] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(TIMES,Value(va''),Value(va'''))),[],[],[])));
(* 1, 
  [v[''a]->1,v['''a]->1], 
  [''a->Int, '''a->Int, '''a0->Int] *) 

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(SUBTRACT,Value(va'''),Value(va'''))),[],[],[])));
(* v['''a - '''a],[],[] *)
	
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(DIVIDE,Value(va''),Value(va'''))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(N(4))),
		ArithExpr(TIMES,Value(N(5)),Value(vb')))),[],[],[])));
(* 2,
  [v['a]->1,v['b]->1]
  ['a->Int,'b->Int] *)

prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(N(4))),
		ArithExpr(TIMES,Value(N(5)),Value(va')))),[],[],[])));
(* 2, 
  [v['a]->1], 
  ['a->Int] *)
 
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(vc'),Value(vd')))),[],[],[])));
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
			Value(N(3))))),[],[],[])));
(* ((v['a]*v['b]) + (v['c]+v['d])) - ((v['e]*v['f])+3)
   =>
   ~1,
   [v['''a15] -> 1, v['''a16] -> 1, v['f] -> v['''a16], v['e] -> v['''a16], v['''a14] -> v['''a15], v['''a13] -> v['''a15], v['d] -> v['''a14], v['c] -> v['''a14], v['b] -> v['''a13], v['a] -> v['''a13]]
   ['''a15 -> int, '''a16 -> int, 'f -> '''a16, 'e -> '''a16, '''a14 -> '''a15, '''a13 -> '''a15, 'd -> '''a14, 'c -> '''a14, 'b -> '''a13, 'a -> '''a13]
*) 
 
prettyPrintConfig(evaluate(Config(Expression(
	ArithExpr(PLUS,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(va'),Value(vb')))),[],[],[])));
(* v[ '''a17-'''a17 + '''a17*'''a17]
  [v['b] -> v['''a17], v['a] -> v['''a17]]
  ['b -> '''a17, 'a -> '''a17] *)
  
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(va'),Value(vb')))),[],[],[])));
(* false
  [v['''a13] -> 1, v['b] -> v['''a13], v['a] -> v['''a13]]
  ['''a13 -> int, ''a14 -> int, 'b -> '''a13, 'a -> '''a13] *)
  
prettyPrintConfig(evaluate(Config(Expression(
	BoolExpr(EQ,
		ArithExpr(SUBTRACT,Value(va'),Value(vb')), 
 		ArithExpr(TIMES,Value(vc'),Value(vd')))),[],[],[])));
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
			Value(N(2))))),[],[],[])));
(* true
  [v['''a20] -> 1, v['''a21] -> 1, v['f] -> v['''a21], v['e] -> v['''a21], v['''a19] -> v['''a20], v['''a18] -> v['''a20], v['d] -> v['''a19], v['c] -> v['''a19], v['b] -> v['''a18], v['a] -> v['''a18]]
  ['''a20 -> int, '''a21 -> int, 'f -> '''a21, 'e -> '''a21, '''a19 -> '''a20, '''a18 -> '''a20, 'd -> '''a19, 'c -> '''a19, 'b -> '''a18, 'a -> '''a18] *)
  
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,Value(va'''),Value(vb''))),[],[],[])));
(* true, [v['''a]->1, v[''b]->1], ['''a->Int, ''b->Int, ''a0->Int] *)
  
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(va',B(true))),
	Value(vb''))),[],[],[])));
(* v[ (''a0,true) = (''a0,true) ]
  [v['a] -> v[''a0], v[''b] -> (v[''a0],true)]
  ['a->''a0, ''a1 -> bool, ''b -> (''a0 * ''a1)]
*)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(va''',N(1))),
	Value(vb''))),[],[],[])));
(* true
  [v['a] -> 1, v[''b] -> (1,1)]
  ['''a->int, ''a0->int, ''a1->int ''b -> (''a0 * ''a1)]
*)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(va''',B(true))),
	Value(vb''))),[],[],[])));
(* true
  [v['a] -> 1, v[''b] -> (1,true)]
  ['''a->int, ''a0->int, ''a1->bool ''b -> (''a0 * ''a1)]
*)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(va''',R(1.0))),
	Value(vb'''))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(va',vb'),ValuePair(vc',vd'''))),
	Value(ve'))),[],[],[])));
(* v[ ((v[''a215],v[''a216]),(v[''a217],1)) = ((v[''a215],v[''a216]),(v[''a217],1)) ]
  [v['''d] -> 1, v['c] -> v[''a217], v['b] -> v[''a216], v['a] -> v[''a215], v['e] -> ((v['a],v['b]),(v['c],v['''d]))]
  ['''d -> int, ''a218 -> int, 'c -> ''a217, 'b -> ''a216, 'a -> ''a215, 'a211 -> 'a, 'a212 -> 'b, 'a213 -> 'c, 'a214 -> '''d, 'a210 -> ('a213 * 'a214), 'a209 -> ('a211 * 'a212), 'e -> ('a209 * 'a210)] *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(va',vb'),ValuePair(vc',vd'''))),
	Value(vb'))),[],[],[])));
(* Stuck (recursive type) *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(va''',vb'''),ValuePair(vc''',vd'''))),
	Value(ve'))),[],[],[])));
(* true,
  [v['''d] -> 1, v['''c] -> 1, v['''b] -> 1, v['''a] -> 1, v['e] -> ((v['''a],v['''b]),(v['''c],v['''d]))]
  ['''d -> int, ''a228 -> int, '''c -> int, ''a227 -> int, '''b -> int, ''a226 -> int, '''a -> int, ''a225 -> int, 'a221 -> '''a, 'a222 -> '''b, 'a223 -> '''c, 'a224 -> '''d, 'a220 -> ('a223 * 'a224), 'a219 -> ('a221 * 'a222), 'e -> ('a219 * 'a220)] *)


prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(va',B(true))),
	Value(vb'''))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(va',vb'),ValuePair(vc',vd'))),
	Value(ve'))),[],[],[])));
(* v[ ((''a60,''a61),(''a62,''a63)) = ((''a60,v''a61),(v''a62,v''a63)) ]
  [v['d] -> v[''a63], v['c] -> v[''a62], v['b] -> v[''a61], v['a] -> v[''a60], v['e] -> ((v['a],v['b]),(v['c],v['d]))]
  ['d -> ''a63, 'c -> ''a62, 'b -> ''a61, 'a -> ''a60, 'a56 -> 'a, 'a57 -> 'b, 'a58 -> 'c, 'a59 -> 'd, 'a55 -> ('a58 * 'a59), 'a54 -> ('a56 * 'a57), 'e -> ('a54 * 'a55)] *)
prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(N(3),N(5)),ValuePair(B(true),B(false)))),
	Value(va'))),[],[],[])));
(* false,
  [v['a] -> ( (1,1), (true,true) )]
  ['a33 -> int, 'a34 -> int, 'a35 -> bool, 'a36 -> bool, 'a32 -> ('a35 * 'a36), 'a31 -> ('a33 * 'a34), 'a -> ('a31 * 'a32)]
*)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(N(3),N(5)),ValuePair(B(true),R(1.0)))),
	Value(va'))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(BoolExpr(EQ,
	Value(ValuePair(ValuePair(va''',N(5)),ValuePair(B(false),vb'''))),
	Value(ValuePair(va'',vb'')))),[],[],[])));
(* false
  [v['''b] -> 1, v[''b] -> (true,1), v['''a] -> 1, v[''a] -> (1,1)]
  [''a78 -> bool, ''a79 -> int, '''b -> int, ''b -> (''a78 * ''a79), ''a76 -> int, '''a -> int, ''a77 -> int, ''a -> (''a76 * ''a77)] *)
  
(* --- EXPRESSION PAIRS --- *)
  
prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	Value(N(3)),Value(N(5)))),[],[],[])));
(* (3,5), [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	Value(N(3)),Value(R(5.0)))),[],[],[])));	
(* (3,5.0), [], [] *)

prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	Value(B(true)),Value(R(5.0)))),[],[],[])));
(* (true,5.0), [], [] *)

prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	Value(N(3)),
	ExpressionPair(Value(N(5)),
				   Value(ValuePair(B(true),B(false)))))),[],[],[])));
(* (3,(5,(true,false))), [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	ArithExpr(PLUS,
		ArithExpr(TIMES,Value(N(5)),Value(N(2))),
		ArithExpr(SUBTRACT,Value(N(2)),Value(N(2)))),
	BoolExpr(LESS,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(TIMES,Value(va'),Value(vb'))))),[],[],[])));
(* (10, v[ '''a0+'''a0 < '''a0*'''a0])
  [v['b] -> v['''a0], v['a] -> v['''a0]]
  ['b -> '''a0, 'a -> '''a0] *)

prettyPrintConfig(evaluate(Config(Expression(ExpressionPair(
	BoolExpr(EQ,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(TIMES,Value(va'),Value(vb'))),
	BoolExpr(LESS,
		ArithExpr(TIMES,Value(va'),Value(vb')),
		ArithExpr(PLUS,Value(va'),Value(vb'))))),[],[],[])));
(* (false,true),
  [v['''a48] -> 1, v['b] -> v['''a48], v['a] -> v['''a48]]
  ['''a51 -> int, '''a50 -> int, '''a48 -> int, ''a49 -> int, 'b -> '''a48, 'a -> '''a48] *)
		
(* --- CONDITIONS --- *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(B(true)),Value(N(5)),Value(N(2)))),[],[],[])));
(* 5, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(B(false)),Value(N(5)),Value(N(2)))),[],[],[])));
(* 2, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(B(true)),
	BoolExpr(LESS,Value(N(2)),Value(N(3))),
	BoolExpr(MORE,Value(N(2)),Value(N(3))))),[],[],[])));
(* true, [], [] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(B(false)),
	BoolExpr(EQ,Value(N(2)),Value(N(3))),
	BoolExpr(EQ,Value(ValuePair(N(2),B(true))),ExpressionPair(Value(N(2)),Value(B(true)))))),
	[],[],[])));
(* true, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	ExpressionPair(
		BoolExpr(EQ,Value(va'''),Value(vb''')),
		ArithExpr(DIVIDE,Value(vb'),Value(vc'))),
	Value(N(3)))),[],[],[])));
(* (true,1.0),
  [v['c] -> 1.0, v['b] -> 1.0, v['''b] -> 1, v['''a] -> 1, v['a] -> true]
  ['c -> real, 'b -> real, '''b -> int, '''a -> int, ''a52 -> int, 'a -> bool] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	BoolExpr(EQ,Value(va'),Value(va')),
	Value(B(false)))),[],[],[])));
(* true, [v['a]->true], ['a->bool,''a0->bool] *)
	
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'),
	BoolExpr(EQ,Value(va'),Value(vb'')),
	Value(B(false)))),[],[],[])));
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
	Value(N(2)))),[],[],[])));
(* v[ ('''a60+'''a60 + '''a60+'''a60) + ('''a60+'''a60 + '''a60+'''a60)]
  [v['''b] -> v['''a60], v['''a] -> v['''a60], v[''a] -> true]
  ['''b -> '''a60, '''a -> '''a60, ''a -> bool] *)
 
prettyPrintConfig(evaluate(Config(Expression(Condition(
	Value(va'''),Value(B(true)),Value(B(false)))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(LESS,Value(N(2)),Value(N(5))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(va'''),Value(vb''')),
			ArithExpr(PLUS,Value(vc'''),Value(vd'''))),
		ArithExpr(PLUS,
			ArithExpr(PLUS,Value(ve'''),Value(vf''')),
			ArithExpr(PLUS,Value(vg'''),Value(vh''')))),
	Value(N(2)))),[],[],[])));
(* v['''a67+'''a67+'''a67+'''a67  + '''a67+'''a67+'''a67+'''a67],
  [v['''a66] -> v['''a67], v['''a63] -> v['''a67], v['''a65] -> v['''a66], v['''a64] -> v['''a66], v['''h] -> v['''a65], v['''g] -> v['''a65], v['''f] -> v['''a64], v['''e] -> v['''a64], v['''a62] -> v['''a63], v['''a61] -> v['''a63], v['''d] -> v['''a62], v['''c] -> v['''a62], v['''b] -> v['''a61], v['''a] -> v['''a61]]
  ['''a66 -> '''a67, '''a63 -> '''a67, '''a65 -> '''a66, '''a64 -> '''a66, '''h -> '''a65, '''g -> '''a65, '''f -> '''a64, '''e -> '''a64, '''a62 -> '''a63, '''a61 -> '''a63, '''d -> '''a62, '''c -> '''a62, '''b -> '''a61, '''a -> '''a61] *)
  
prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(EQ,Value(R(3.0)),Value(R(2.0))),
	Value(N(2)),Value(N(3)))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(LESS_EQ,Value(va''),Value(vb'')),
	ArithExpr(DIVIDE,Value(va'),ArithExpr(PLUS,Value(vb'),Value(vc'''))),
	Value(R(2.0)))),
	[],[],[])));
(* 0.5
  [v['''a69] -> 1.0, v['a] -> 1.0, v['''c] -> v['''a69], v['b] -> v['''a69], v[''b] -> 1, v[''a] -> 1]
  ['''a69 -> real, 'a -> real, '''c -> '''a69, 'b -> '''a69, ''b -> int, ''a -> int, '''a68 -> int]
*)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,Value(va''),Value(vb'')),
	Value(R(2.0)),
	ArithExpr(TIMES,Value(N(2)),ArithExpr(PLUS,Value(va''),Value(vb''))))),
	[],[],[])));
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
	Value(N(2)))),
	[],[],[])));
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
	Value(N(2)))),
	[],[],[])));
(* v[if ('''a79+'''a79 > '''a79-'''a79) then ('a+'b + 'c+'d) else 2]
  [v['''a78] -> v['''a79], v['''a77] -> v['''a78], v['''a76] -> v['''a78], v['d] -> v['''a77], v['c] -> v['''a77], v['b] -> v['''a76], v['a] -> v['''a76]]
  ['''a78 -> '''a79, '''a77 -> '''a78, '''a76 -> '''a78, 'd -> '''a77, 'c -> '''a77, 'b -> '''a76, 'a -> '''a76] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	BoolExpr(MORE,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(SUBTRACT,Value(vc'),Value(N(2)))),
	ArithExpr(PLUS,
		ArithExpr(PLUS,Value(va'),Value(vb')),
		ArithExpr(PLUS,Value(vc'),Value(vd'))),
	Value(N(2)))),
	[],[],[])));
(* 4,
  [v['d] -> 1, v['''a80] -> 1, v['c] -> 1, v['b] -> v['''a80], v['a] -> v['''a80]]
  ['d -> int, '''a82 -> int, '''a81 -> int, '''a80 -> int, 'c -> int, 'b -> '''a80, 'a -> '''a80] *)
 
prettyPrintConfig(evaluate(Config(Expression(Condition(
	ExpressionPair(Value(N(3)),Value(N(2))),
	Value(N(2)),Value(N(3)))),[],[],[])));
(* Stuck *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(LESS,Value(N(2)),Value(N(3))),
			  BoolExpr(MORE,Value(R(5.0)),Value(R(2.0))),
			  BoolExpr(EQ,Value(B(true)),Value(B(true)))),
	Value(N(3)),Value(N(2)))),[],[],[])));
(* 3, [], [] *)

prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(LESS,Value(N(2)),Value(N(3))),
			  Value(va'),
			  BoolExpr(EQ,Value(B(true)),Value(B(true)))),
	Value(va'),Value(B(false)))),[],[],[])));
(* true, [v['a]->true], ['a->bool] *)
 
 prettyPrintConfig(evaluate(Config(Expression(Condition(
	Condition(BoolExpr(EQ,ExpressionPair(Value(va''),ArithExpr(PLUS,Value(N(2)),Value(vb''))),
						  Value(ValuePair(B(true),N(3)))),
			  BoolExpr(LESS,Value(N(1)),ArithExpr(PLUS,Value(va'''),Value(vb'''))),
			  Value(B(false))),
	Condition(Value(va''),
			  ArithExpr(PLUS,ArithExpr(PLUS,Value(va'''),Value(vb''')),Value(vc''')),
			  ArithExpr(SUBTRACT,ArithExpr(SUBTRACT,Value(va'''),Value(vb''')),Value(vc'''))),
	Value(N(100)))),[],[],[])));
(* if (if (v[''a],2+''b)=(true,3) then (1<v['''a]+v['''b]) else false
   then (if v[''a] then (v['''a]+v['''b]+v['''c]) else (v['''a]-v['''b]-v['''c])
   else 100 
   =>
   3,
   [v['''c] -> 1, v['''a83] -> 1, v['''b] -> v['''a83], v['''a] -> v['''a83], v[''a] -> true, v[''b] -> 1]
   ['''c -> int, '''a84 -> int, '''a83 -> int, '''b -> '''a83, '''a -> '''a83, ''a -> bool, ''b -> int] *)
 
(* --- CASE --- *)

val x = Variable(Var("x"));
val y = Variable(Var("y"));
val xy = VariablePair(Var("x"),Var("y"));
val a = Variable(Var("a"));
val b = Variable(Var("b"));
val ab = VariablePair(Var("a"),Var("b"));

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(N(2),N(3))),
	xy,
	ArithExpr(PLUS,x,y))),[],[],[])));
(* 5, [], ['a86->int], ['a85->int], [x->2,y->3] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(R(3.0),R(5.0))),
	xy,
	ArithExpr(DIVIDE,x,y))),[],[],[])));
(* 0.6, [], ['a88->real, 'a87->real], [x->3.0,y->5.0] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(ValuePair(N(3),N(5)),ValuePair(N(3),N(5)))),
	xy,
	BoolExpr(EQ,x,y))),[],[],[])));
(* true, [],
  ['a94 -> int, 'a93 -> int, 'a90 -> ('a93 * 'a94), 'a92 -> int, 'a91 -> int, 'a89 -> ('a91 * 'a92)]
  [x -> (3,5), y -> (3,5)] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(Value(N(3)),Value(N(4))),
	xy,
	BoolExpr(LESS,x,y))),[],[],[])));
(* true, [], ['a96->int, 'a95->int], [x->3,y->4] *)	

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ArithExpr(PLUS,Value(N(3)),Value(N(5))),
				   BoolExpr(LESS,Value(R(1.0)),Value(R(2.0)))),
	xy,
	BoolExpr(EQ,BoolExpr(EQ,x,x),y))),[],[],[])));
(* true, [], ['a98->bool, '97->int], [x->8,y->true] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ExpressionPair(ArithExpr(PLUS,Value(N(3)),Value(N(5))),
								  BoolExpr(LESS,Value(R(1.0)),Value(R(2.0)))),
				   Value(B(true))),
	xy,
	Condition(y,ExpressionPair(x,ExpressionPair(y,y)),ExpressionPair(Value(N(1)),Value(N(1)))))),
	[],[],[])));
(* ((8,true),(true,true)), [], 
  ['a100->bool,'a102->bool,'a101->int,'a99->('a101*'a102)],
  [x->(8,true),y->true] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(va'),
	xy,
	ExpressionPair(x,y))),[],[],[])));
(* (v['a103],v['a104]),
  [v['a]->(v['a103],v['a104])],
  ['a105->'a103,'a106->a'104,'a->('a105*'a106)],
  [x->v['a103],y->v['a104]] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(va',vb')),
	xy,
	ExpressionPair(ArithExpr(PLUS,x,y),ArithExpr(SUBTRACT,x,y)))),[],[],[])));
(* (v['''a109+'''a109],v['''a109-'''a109]),
  [v['a108]->v['''109],v['a107]->v['''a109],v['b]->v['a108] *)
  
prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(ValuePair(va',vb'),ValuePair(vc',ValuePair(vd'',vg')))),
	xy,
	ExpressionPair(BoolExpr(EQ,x,Value(ValuePair(N(1),B(true)))),
				   BoolExpr(EQ,y,Value(ValuePair(ValuePair(B(false),N(5)),ValuePair(ve''',vf'''))))))),[],[],[])));
(* (true,v[ ((true,1),(v[''a120],v[''a121])) = ((false,5),(1,1)) ]),
   [v['''f] -> 1, v['a117] -> v[''a121], v['''e] -> 1, v[''d] -> v[''a120], v['a114] -> (true,1), v['a113] -> true, v['a112] -> 1, v['g] -> v['a117], v['c] -> v['a114], v['b] -> v['a113], v['a] -> v['a112]]
   ['''f -> int, ''a121 -> int, 'a117 -> ''a121, '''e -> int, ''a120 -> int, ''d -> ''a120, 'a118 -> bool, 'a119 -> int, 'a114 -> ('a118 * 'a119), 'a113 -> bool, 'a112 -> int, 'g -> 'a117, 'a116 -> ''d, 'a115 -> ('a116 * 'a117), 'c -> 'a114, 'a111 -> ('a114 * 'a115), 'b -> 'a113, 'a -> 'a112, 'a110 -> ('a112 * 'a113)]
   [x -> (v['a112],v['a113]), y -> (v['a114],(v[''d],v['a117]))] *)
   
prettyPrintConfig(evaluate(Config(Expression(Case(
	Condition(Value(va'),
			  Value(ValuePair(ValuePair(N(5),N(6)),ValuePair(N(7),N(8)))),
			  Value(vb')),
	xy,
	ArithExpr(PLUS,
			  Case(x,ab,ArithExpr(PLUS,a,b)),
			  Case(y,ab,ArithExpr(PLUS,a,b))))),[],[],[])));
(* 26,
   [v['a] -> true]
   ['a131 -> int, 'a130 -> int, 'a129 -> int, 'a128 -> int, 'a127 -> int, 'a126 -> int, 'a123 -> ('a126 * 'a127), 'a125 -> int, 'a124 -> int, 'a122 -> ('a124 * 'a125), 'a -> bool]
   [a -> 7, b -> 8, a -> 5, b -> 6, x -> (5,6), y -> (7,8)] *)
(* TO CHECK *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ArithExpr(PLUS,Value(N(1)),Value(N(1))),ArithExpr(SUBTRACT,Value(N(4)),Value(N(1)))),
	xy,
	ArithExpr(TIMES,
			  Case(ExpressionPair(x,Value(N(2))),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(2))))),
			  Case(ExpressionPair(Value(N(3)),y),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(3)))))))),[],[],[])));

(* case (1+1,4-1) of (x,y) -> (case (x,2) of (a,b) -> a*b*2) * (case (3,y) of (a,b) -> a*b*3)
   =>
   216, [], 
   ['a137 -> int, 'a136 -> int, 'a135 -> int, 'a134 -> int, 'a133 -> int, 'a132 -> int],
   [a -> 3, b -> 3, a -> 2, b -> 2, x -> 2, y -> 3]
*)  

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ArithExpr(PLUS,Value(va'),Value(vb')),ArithExpr(SUBTRACT,Value(vc'),Value(vd'))),
	xy, ArithExpr(PLUS,x,y))),[],[],[])));
(* v[ '''a144+'''a144 + '''a144-'''a144 ],
  [v['''a143] -> v['''a144], v['''a142] -> v['''a144], v['''a139] -> v['''a143], v['''a138] -> v['''a142], v['d] -> v['''a139], v['c] -> v['''a139], v['b] -> v['''a138], v['a] -> v['''a138]]
  ['''a143 -> '''a144, '''a142 -> '''a144, '''a139 -> '''a143, 'a141 -> '''a143, '''a138 -> '''a142, 'a140 -> '''a142, 'd -> '''a139, 'c -> '''a139, 'b -> '''a138, 'a -> '''a138]
  [x -> v[ v['''a142] + v['''a142] ], y -> v[ v['''a143] - v['''a143] ]]":
*)  
	
prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ArithExpr(PLUS,Value(va'),Value(vb')),ArithExpr(SUBTRACT,Value(vc'),Value(vd'))),
	xy, 
	ArithExpr(PLUS,
			  Case(ExpressionPair(x,Value(N(2))),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(2))))),
			  Case(ExpressionPair(Value(N(3)),y),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(3)))))))),[],[],[])));
(* 8,
  [v['''a156] -> 1, v['''a150] -> v['''a156], v['''a153] -> 1, v['''a149] -> v['''a153], v['''a146] -> v['''a150], v['''a145] -> v['''a149], v['d] -> v['''a146], v['c] -> v['''a146], v['b] -> v['''a145], v['a] -> v['''a145]]
  ['''a156 -> int, '''a150 -> '''a156, 'a155 -> '''a156, 'a154 -> int, '''a153 -> int, 'a152 -> int, '''a149 -> '''a153, 'a151 -> '''a153, '''a146 -> '''a150, 'a148 -> '''a150, '''a145 -> '''a149, 'a147 -> '''a149, 'd -> '''a146, 'c -> '''a146, 'b -> '''a145, 'a -> '''a145]
  [a -> 3, b -> v[ v['''a156] - v['''a156] ], a -> v[ v['''a153] + v['''a153] ], b -> 2, x -> v[ v['''a149] + v['''a149] ], y -> v[ v['''a150] - v['''a150] ]] *)

prettyPrintConfig(evaluate(Config(Expression(Case(
	ExpressionPair(ArithExpr(PLUS,Value(va'),Value(vb')),ArithExpr(SUBTRACT,Value(vc'),Value(vd'))),
	xy, 
	ArithExpr(PLUS,
			  Case(ExpressionPair(x,Value(va''')),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vb''')))),
			  Case(ExpressionPair(Value(vc'''),y),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vd'''))))))),[],[],[])));
(* v[ (('''a173+'''a173) * '''a173 * '''a173) + ('''a173 * ('''a173-'''a173) * '''a173)) ],
  [v['''a172] -> v['''a173], v['''a167] -> v['''a173], v['''a171] -> v['''a172], v['''c] -> v['''a172], v['''d] -> v['''a171], v['''a170] -> v['''a171], v['''a162] -> v['''a170], v['''a166] -> v['''a167], v['''a165] -> v['''a167], v['''b] -> v['''a166], v['''a] -> v['''a166], v['''a161] -> v['''a165], v['''a158] -> v['''a162], v['''a157] -> v['''a161], v['d] -> v['''a158], v['c] -> v['''a158], v['b] -> v['''a157], v['a] -> v['''a157]]
  ['''a172 -> '''a173, '''a167 -> '''a173, '''a171 -> '''a172, '''c -> '''a172, '''d -> '''a171, '''a170 -> '''a171, '''a162 -> '''a170, 'a169 -> '''a170, 'a168 -> '''c, '''a166 -> '''a167, '''a165 -> '''a167, '''b -> '''a166, '''a -> '''a166, 'a164 -> '''a, '''a161 -> '''a165, 'a163 -> '''a165, '''a158 -> '''a162, 'a160 -> '''a162, '''a157 -> '''a161, 'a159 -> '''a161, 'd -> '''a158, 'c -> '''a158, 'b -> '''a157, 'a -> '''a157]
  [a -> v['''c], b -> v[ v['''a170] - v['''a170] ], a -> v[ v['''a165] + v['''a165] ], b -> v['''a], x -> v[ v['''a161] + v['''a161] ], y -> v[ v['''a162] - v['''a162] ]] *)
  
prettyPrintConfig(evaluate(Config(Expression(ArithExpr(PLUS,
	Case(ExpressionPair(ArithExpr(PLUS,Value(N(1)),Value(N(1))),ArithExpr(SUBTRACT,Value(N(4)),Value(N(1)))),
		 xy,
		 ArithExpr(TIMES,
			  Case(ExpressionPair(x,Value(N(2))),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(2))))),
			  Case(ExpressionPair(Value(N(3)),y),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(N(3))))))),
	Case(
	ExpressionPair(ArithExpr(PLUS,Value(va'),Value(vb')),ArithExpr(SUBTRACT,Value(vc'),Value(vd'))),
	xy, 
	ArithExpr(PLUS,
			  Case(ExpressionPair(x,Value(va''')),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vb''')))),
			  Case(ExpressionPair(Value(vc'''),y),ab,ArithExpr(TIMES,a,ArithExpr(TIMES,b,Value(vd''')))))))),[],[],[])));
(* 218, 
[v['''a198] -> 1, v['''a197] -> v['''a198], v['''a192] -> v['''a198], v['''a196] -> v['''a197], v['''c] -> v['''a197], v['''d] -> v['''a196], v['''a195] -> v['''a196], v['''a185] -> v['''a195], v['''a191] -> v['''a192], v['''a190] -> v['''a192], v['''b] -> v['''a191], v['''a] -> v['''a191], v['''a184] -> v['''a190], v['''a181] -> v['''a185], v['''a180] -> v['''a184], v['d] -> v['''a181], v['c] -> v['''a181], v['b] -> v['''a180], v['a] -> v['''a180]]
['''a198 -> int, '''a197 -> '''a198, '''a192 -> '''a198, '''a196 -> '''a197, '''c -> '''a197, '''d -> '''a196, '''a195 -> '''a196, '''a185 -> '''a195, 'a194 -> '''a195, 'a193 -> '''c, '''a191 -> '''a192, '''a190 -> '''a192, '''b -> '''a191, '''a -> '''a191, 'a189 -> '''a, '''a184 -> '''a190, 'a188 -> '''a190, '''a181 -> '''a185, 'a183 -> '''a185, '''a180 -> '''a184, 'a182 -> '''a184, 'd -> '''a181, 'c -> '''a181, 'b -> '''a180, 'a -> '''a180, 'a179 -> int, 'a178 -> int, 'a177 -> int, 'a176 -> int, 'a175 -> int, 'a174 -> int]
[a187 -> v['''c], b187 -> v[ v['''a195] - v['''a195] ], a186 -> v[ v['''a190] + v['''a190] ], b186 -> v['''a], x -> v[ v['''a184] + v['''a184] ], y -> v[ v['''a185] - v['''a185] ], a -> 3, b -> 3, a -> 2, b -> 2, x -> 2, y -> 3]
*)

prettyPrintConfig(evaluate(Config(Expression(Case(
	Value(ValuePair(N(3),N(4))),
	xy,
	Case(ExpressionPair(ArithExpr(PLUS,x,y),
						ArithExpr(TIMES,x,y)),
		 xy,
		 ExpressionPair(ArithExpr(TIMES,Value(N(2)),x),ArithExpr(TIMES,Value(N(3)),y))))),[],[],[])));
(* case (3,4) of (x,y) -> [case (x+y,x*y) of (x,y) -> (x*2,y*3)]
   =>
  (14,36), [], 
  ['a231 -> int, 'a230 -> int, 'a228 -> int, 'a227 -> int] *)
  
(* use "C:/Users/Thomas/Documents/GitHub/Dissertation/include-all.sml"; *)