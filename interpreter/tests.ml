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
<<<<<<< HEAD

(*BOOLEAN TESTS*)
let t1a0 = evaluate (BoolC true) = Bool true
let t1a1 = evaluate (BoolC false) = Bool false
let t1c0 = desugar (BoolS true) = BoolC true
let t1c1 = desugar (BoolS false) = BoolC false
let t1d0 = evaluate (desugar (BoolS true)) = Bool true
let t1d1 = evaluate (desugar (BoolS false)) = Bool false

=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da