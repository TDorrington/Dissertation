use "C:/Users/Thomas/Documents/GitHub/Dissertation/SUBSTITUTION-sig.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/substitution.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/datatypes.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/typeof.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/gen.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/unify.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/narrow.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/rule-operation.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/rule-conditional.sml";

(* Basic case: 4+3=7 -------------------------------------------------------------- *)
(* int *)
elabPhraseOperation(Config( Expression(Plus(Value(N(4)),Value(N(3)))), [], []));
(* evaluates to <Value(N(7)),[],[]> *)

(* real *)
elabPhraseOperation(Config( Expression(Plus(Value(R(4.0)),Value(R(3.0)))), [], []));
(* evaluates to <Value(R(7.0),[],[] *)

(* Type variable case: checks 'a in v['a]+3 instantiated to correct type ---------- *)
(* int *)
elabPhraseOperation(Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))), 
								 Value(N(3)))), [], []));
(* evaluates to <Value(R(4),[V['a]->1],['a->Int] *)
								 
(* real *)
elabPhraseOperation(Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))), 
								  Value(R(3.0)))), [], []));
(* evaluates to <Value(R(4.0),[V['a]->1.0],['a->Real] *)

(* Type variable case again: check 'a1 and 'a2 in v['a1]+v['a2] instantiated to type int *)
elabPhraseOperation(Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a1")))),
										    Value(VHole(ValueHole(TypeVar("a2")))))), [], []));
(* evaluates to <Value(V[''a1]+V[''a2],[V['a1]->Value(V[''a1]), V['a2]->Value(V[''a2])],
									   ['a1 -> ''a1, 'a2 -> ''a2] *)

(* Stuck configuration: true+4=stuck *)								 
elabPhraseOperation(Config( Expression(Plus(Value(B(true)),Value(N(4)))), [], []));
(* evaluates to <Stuck,[],[]> *)

(* Check if substitution contains v['a]->5, then v['a]+3=8 *)
elabPhraseOperation(Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))),
								 Value(N(3)))),
				 [ (ValueHole(TypeVar("a")), N(5)) ],
				 [ (TypeHole (TypeVar("a")), Int ) ]));
(* evaluates to <Value(N(8)), [V['a]->5], ['a->Int]> *)

(* Basic case: if true then 3 else 4 *)				 
elabPhraseCondition(Config( Expression(Condition(Value(B(true)), Value(N(3)), Value(N(4)))), [], []));
(* evaluates to <Value(N(3)),[],[]> *)

(* Basic case: if false then 3 else 4 *)		
elabPhraseCondition(Config( Expression(Condition(Value(B(false)), Value(N(3)), Value(N(4)))), [], []));
(* evaluates to <Value(N(4)),[],[]> *)

(* Type variable case: check 'a in 'if v['a] then 3 else 4' instantiated to type bool *)
elabPhraseCondition(Config( Expression(Condition(Value(VHole(ValueHole(TypeVar("a")))),
									  Value(N(3)), Value(N(4)))), [], []));
(* evaluates to <Value(N(3)),[V['a]->true],['a->Bool]> *)
									  
(* Check if substitution contains v['a]->true then 'if v['a] then 3 else 4 = 3 *)
elabPhraseCondition(Config( Expression(Condition(Value(VHole(ValueHole(TypeVar("a")))),
									   Value(N(3)), Value(N(4)))),
				 [ (ValueHole(TypeVar("a")), B(true)) ],
				 [ (TypeHole (TypeVar("a")), Bool)    ]));
(* evaluates to <Value(N(3)),[V['a]->true],['a->Bool]> *)
				 