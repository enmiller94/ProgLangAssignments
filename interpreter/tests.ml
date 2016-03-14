open Types

(* You can test expressions of type resultS or resultC and how they are evaluated *)
(* These will only work once you have compiled types.ml *)

(* This is the one kind of test you can write. *)
let t0a = evaluate (NumC 2.3) = Num 2.3

(* You can also use interp directly to specify a custom environment. *)
let t0b = let env1 = bind "x" (Num 3.1) empty
          in interp env1 (NumC 2.3) = Num 2.3

(* You can also test desugar to make sure it behaves properly. *)
let t0c = desugar (NumS 2.3) = NumC 2.3

(* Or you can combine with evaluate to get to the final value. *)
let t0d = evaluate (desugar (NumS 2.3)) = Num 2.3

(*BOOLEAN TESTS*)
let t1a0 = evaluate (BoolC true) = Bool true 
let t1a1 = evaluate (BoolC false) = Bool false 
let t1c0 = desugar (BoolS true) = BoolC true 
let t1c1 = desugar (BoolS false) = BoolC false 
let t1d0 = evaluate (desugar (BoolS true)) = Bool true 
let t1d1 = evaluate (desugar (BoolS false)) = Bool false 

(*CONDITIONAL TESTS*)
let t2a = evaluate (IfC (BoolC true, NumC 2.3, NumC 4.3)) = Num 2.3 
let t2b = evaluate (IfC (BoolC false, NumC 2.3, NumC 4.3)) = Num 4.3 
(*let t2c = evaluate (IfC (NumC 2 = NumC 2, NumC 2.3, NumC 4.3)) = Num 2.3*)
(*Need case where exception is hit.*)
let t3a = evaluate (desugar (NotS (BoolS true))) = Bool false 
let t3b = evaluate (desugar (NotS (BoolS false))) = Bool true 
(*let t3c = evaluate (NotS ())*)
(*let t3c = evaluate (NotS ()) = true*)
(*Need case where exception is hit.*)
let t3a = evaluate (desugar (OrS (BoolS true, BoolS true))) = Bool true 
let t3b = evaluate (desugar (OrS (BoolS false, BoolS true))) = Bool true 
let t3c = evaluate (desugar (OrS (BoolS true, BoolS false))) = Bool true 
let t3d = evaluate (desugar (OrS (BoolS false, BoolS false))) = Bool false 
(*Need more than just true / false*)

let t4a = evaluate (desugar (AndS (BoolS true, BoolS true))) = Bool true 
let t4b = evaluate (desugar (AndS (BoolS true, BoolS false))) = Bool false 
let t4c = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false 
let t4d = evaluate (desugar (AndS (BoolS false, BoolS false))) = Bool false 
(*Need more than just true / false*)


