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

(* Boolean *)

let tb1 = evaluate (BoolC true) = Bool true
let tb2 = evaluate (BoolC false) = Bool false
let tb3 = evaluate (desugar (BoolS true)) = Bool true
let tb4 = evaluate (desugar (BoolS false)) = Bool false

(* Conditionals *)

let tc1 = evaluate (IfC (BoolC true, NumC 3.5, NumC 5.8)) = Num 3.5
let tc2 = evaluate (IfC (BoolC false, NumC 3.5, NumC 5.8)) = Num 5.8
let tc3 = evaluate (IfC (BoolC true, (IfC (BoolC false, NumC 3.5, NumC 5.8)), NumC 8.2)) = Num 5.8
let tc4 = evaluate (IfC (BoolC false, (IfC (BoolC false, NumC 3.5, NumC 5.8)), NumC 8.2)) = Num 8.2

let tc5 = evaluate (desugar (NotS (BoolS true))) = Bool false
let tc6 = evaluate (desugar (NotS (BoolS false))) = Bool true

let tc7 = evaluate (desugar (OrS (BoolS true, BoolS true))) = Bool true
let tc8 = evaluate (desugar (OrS (BoolS false, BoolS false))) = Bool false
let tc9 = evaluate (desugar (OrS (BoolS true, BoolS false))) = Bool true
let tc10 = evaluate (desugar (OrS (BoolS false, BoolS true))) = Bool true

let tc11 = evaluate (desugar (AndS (BoolS true, BoolS true))) = Bool true
let tc12 = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false
let tc13 = evaluate (desugar (AndS (BoolS true, BoolS false)) = Bool false
let tc14 = evaluate (desugar (AndS (BoolS false, BoolS false))) = Bool false

(* Arithemetic *)

let ta1 = evaluate (IfC (BoolC true, ArithC ("+", NumC 6.0, NumC 6.0), ArithC ("-", NumC 6.0, NumC 6.0))) = Num 12.0
let ta2 = evaluate (IfC (BoolC false, ArithC ("+", NumC 6.0, NumC 6.0), ArithC ("-", NumC 6.0, NumC 6.0))) = Num 0.0
let ta3 = evaluate (IfC (BoolC true, ArithC ("*", NumC 2.0, NumC 4.0), ArithC ("/", NumC 6.0, NumC 6.0))) = Num 8.0
let ta4 = evaluate (IfC (BoolC false, ArithC ("*", NumC 2.0, NumC 4.0), ArithC ("/", NumC 6.0, NumC 6.0))) = Num 1.0

(* Comparison *)

let tco1 = evaluate (IfC (BoolC true, CompC (">", NumC 8.0, NumC 8.0), CompC (">=", NumC 8.0, NumC 8.0))) = Bool false
let tco2 = evaluate (IfC (BoolC false, CompC (">", NumC 8.0, NumC 8.0), CompC (">=", NumC 8.0, NumC 8.0))) = Bool true
let tco3 = evaluate (IfC (BoolC true, CompC ("<", NumC 8.0, NumC 9.0), CompC ("<=", NumC 8.0, NumC 9.0))) = Bool true
let tco4 = evaluate (IfC (BoolC false, CompC ("<", NumC 8.0, NumC 9.0), CompC ("<=", NumC 8.0, NumC 9.0))) = Bool true
let tco5 = evaluate (desugar (IfS (BoolS true, CompS (">", NumS 8.0, NumS 8.0), CompS (">=", NumS 8.0, NumS 8.0)))) = Bool false

(* Equality *)

let te1 = evaluate (EqC (NumC 4.0, NumC 4.0)) = Bool true
let te2 = evaluate (EqC (Numc 4.0, NumC 7.0)) = Bool false
let te3 = evaluate (desugar (NeqS (NumS 4.0, NumS 4.0))) = Bool false
let te4 = evaluate (desugar (NeqS (NumS 4.0, NumS 7.0))) = Bool true
let te5 = evaluate (desugar (EqS (Nums 4.0, NumS 4.0))) = Bool true
let te6 = evaluate (desugar (EqS (Nums 4.0, NumS 70))) = Bool false
