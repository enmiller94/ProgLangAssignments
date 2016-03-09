exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
<<<<<<< HEAD
type exprS = NumS of float | BoolS of bool


(* You will need to add more cases here. *)
type exprC = NumC of float | BoolC of bool | IfC of (exprC * exprC * exprC)


(* You will need to add more cases here. *)
type value = Num of float | Bool of bool

=======
type exprS = NumS of float

(* You will need to add more cases here. *)
type exprC = NumC of float


(* You will need to add more cases here. *)
type value = Num of float
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da

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
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | NumS i        -> NumC i
<<<<<<< HEAD
  | BoolS b 	    -> BoolC b

=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da

(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | NumC i        -> Num i
<<<<<<< HEAD
  | BoolC b 	    -> Bool b
  | IfC (test, option1, option2) -> if evaluate test
                                    then evaluate option1
                                    else evaluate option2

=======
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da

(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []


<<<<<<< HEAD
(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b 			    -> string_of_bool b
=======


(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
>>>>>>> 17b0c067ccbf27a7f4c5760788690d53093424da