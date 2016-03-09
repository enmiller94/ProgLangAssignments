exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)

(* You will need to add more cases here. *)
type exprS = NumS of float | BoolS of bool | IfS of (exprS * exprS * exprS) 
    | OrS of (exprS * exprS) | AndS of (exprS * exprS) | NotS of exprS


(* You will need to add more cases here. *)
type exprC = NumC of float | BoolC of bool | IfC of (exprC * exprC * exprC) 


(* You will need to add more cases here. *)
type value = Num of float | Bool of bool


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
                        | BoolS b 	    -> BoolC b
                        | IfS (test, option1, option2) -> let test' = desugar test in
                                                            let option1' = desugar option1 in
                                                              let option2' = desugar option2 in
                                                                IfC (test', option1', option2')
                        | OrS (BoolS o1, BoolS o2) -> if o1
                                          then BoolC true 
                                          else if o2 
                                          then BoolC true 
                                          else BoolC false 
                        | AndS (BoolS a1, BoolS a2) -> if a1 
                                           then BoolC true 
                                           else if a2 
                                           then BoolC true 
                                           else BoolC false 
                        | NotS (BoolS n) -> if n 
                                            then BoolC false 
                                            else BoolC true 


(* You will need to add cases here. *)
(* interp : Value env -> exprC -> value *)
let rec interp env r = match r with
                       | NumC i        -> Num i
                       | BoolC b 	    -> Bool b
                       | IfC (test, option1, option2) -> (match (interp env test) with 
                                                         | Bool true -> interp env option1 
                                                         | Bool false -> interp env option2 
                                                         | _ -> raise (Interp "Not Boolean")) 


(* evaluate : exprC -> val *)
let evaluate exprC = exprC |> interp []



(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Num i           -> string_of_float i
  | Bool b 			    -> string_of_bool b
