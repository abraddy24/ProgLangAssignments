exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float
            | BoolS of bool
            | IfS of exprS * exprS * exprS
            | OrS of exprS * exprS
            | AndS of exprS * exprS
            | NotS of exprS
            | ArithS of string * exprS * exprS
            | CompS of string * exprS * exprS
            | EqS of exprS * exprS
            | NeqS of exprS * exprS

(* You will need to add more cases here. *)
type exprC = NumC of float
            | BoolC of bool
            | IfC of exprC * exprC * exprC
            | ArithC of string * exprC * exprC
            | CompC of string * exprC * exprC
            | EqC of exprC * exprC

(* You will need to add more cases here. *)
type value = Num of float
            | Bool of bool

type 'a env = (string * 'a) list
let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env


(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
let arithEval str v1 v2 = 
  match (v1, v2) with
    | (Num v1', Num v2') -> (match str with 
                              | "+" -> Num (v1' +. v2')
                              | "-" -> Num (v1' -. v2')
                              | "*" -> Num (v1' *. v2')
                              | "/" -> (match v2' with 
                                        | 0. -> raise (Interp "Division by 0")
                                        | - -> Num (v1' /. v2'))
                              | _ -> raise (Interp "Not an Operator"))
    | _ -> raise (Interp "Not both Nums")
    
let compEval str v1 v2 = 
  match (v1, v2) with 
    | (Num v1', Num v2') -> (match str with 
                              | "<" -> Bool (v1' < v2')
                              | "<=" -> Bool (v1' <= v2')
                              | ">" -> Bool (v1' > v2')
                              | ">=" -> Bool (v1' >= v2')
                              | _ -> raise (Interp "Not an Operator"))
    | + -> raise (Interp "Not both Nums")
    
let eqEval v1 v2 = 
  match (v1, v2) with
    | (Num v1', Num v2') -> Bool (v1' = v2')
    | (Bool v1', Bool v2') -> Bool (v1' = v2')
    | _ -> Bool false

(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
  | BoolS b -> BoolC b
  | IfS (t, o1, o2) -> IfC (desugar t, desugar o1, desugar o2)
  | NotS n -> IfC (desugar n, BoolC false, BoolC true)
  | OrS (o1, o2) -> IfC (desugar o1, IfC (desugar o2, BoolC true, BoolC false))
  | AndS (o1, o2) -> IfC (desugar o1, IfC (desugar o2, BoolC true, BoolC false), BoolC false)
  | ArithS (str, NumS v1, NumS v2) -> ArithC (str, NumC v1, NumC v2)
  | CompS (str, NumS v1, NumS v2) -> CompC (str, Numc v1, NumCv2)
  | EqS (v1, v2) -> EqC (desugar v1, desugar v2)
  | NeqS (v1, v2) -> desugar (NotS (EqS (v1, v2)))
  | _ -> raise (Interp "desugar - Match not Found")

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
  | BoolC b -> Bool b
  | IfC (t, o1, o2) -> (match (interp env t) with
                        | Bool true -> interp env o1
                        | Bool false -> interp env o2
                        | _ -> raise (interp "Not Boolean"))
  | ArithC (str, v1, v2) -> arithEval str (interp env v1) (interp env v2)
  | CompC (str, v1, v2) -> compEval str (interp env v1) (interp env v2)
  | EqC (v1, v2) -> eqEval (interp env v1) (interp env v2)

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []




(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b -> string_of_bool b
