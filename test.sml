(* TO ASK:

	- should IF not performs constraints on two return expressions having same type 
*)

use "C:/Users/Thomas/Documents/GitHub/Dissertation/SUBSTITUTION-sig.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/substitution.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/datatypes.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/typeof.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/gen.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/unify.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/narrow.sml";
use "C:/Users/Thomas/Documents/GitHub/Dissertation/rules.sml";

(* Basic case: 4+3=7 -------------------------------------------------------------- *)
(* int *)
val c1 = Config( Expression(Plus(Value(N(4)),Value(N(3)))), [], []);

(* real *)
val c1' = Config( Expression(Plus(Value(R(4.0)),Value(R(3.0)))), [], []);

(* Type variable case: checks 'a in v['a]+3 instantiated to correct type ---------- *)
(* int *)
val c2 = Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))), 
								 Value(N(3)))), [], []);

(* real *)
val c2' = Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))), 
								  Value(R(3.0)))), [], []);

(* Type variable case again: check 'a1 and 'a2 in v['a1]+v['a2] instantiated to type int *)
val c3 = Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a1")))),
								 Value(VHole(ValueHole(TypeVar("a2")))))), [], []);

(* Stuck configuration: true+4=stuck *)								 
val c4 = Config( Expression(Plus(Value(B(true)),Value(N(4)))), [], []);

(* Check if substitution contains v['a]->5, then v['a]+3=8 *)
val c5 = Config( Expression(Plus(Value(VHole(ValueHole(TypeVar("a")))),
								 Value(N(3)))),
				 [ (ValueHole(TypeVar("a")), N(5)) ],
				 [ (TypeHole (TypeVar("a")), Int ) ]);

(* Basic case: if true then 3 else 4 *)				 
val c6 = Config( Expression(Condition(Value(B(true)), Value(N(3)), Value(N(4)))), [], []);

(* Basic case: if false then 3 else 4 *)		
val c7 = Config( Expression(Condition(Value(B(false)), Value(N(3)), Value(N(4)))), [], []);

(* Type variable case: check 'a in 'if v['a] then 3 else 4' instantiated to type bool *)
val c8 = Config( Expression(Condition(Value(VHole(ValueHole(TypeVar("a")))),
									  Value(N(3)), Value(N(4)))), [], []);
									  
(* Check if substitution contains v['a]->true then 'if v['a] then 3 else 4 = 3 *)
val c9 = Config( Expression(Condition(Value(VHole(ValueHole(TypeVar("a")))),
									  Value(N(3)), Value(N(4)))),
				 [ (ValueHole(TypeVar("a")), B(true)) ],
				 [ (TypeHole (TypeVar("a")), Bool)    ]);
				 