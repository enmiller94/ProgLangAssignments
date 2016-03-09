exception Desugar of string      (* Use for desugarer errors *)
exception Interp of string       (* Use for interpreter errors *)


type exprS = NumS of float | BoolS of bool
type exprC = NumC of float | BoolC of bool | IfC of (exprC * exprC * exprC)
type value = Num of float | Bool of bool 


type exprS = NumS of float
type exprC = NumC of float
type value = Num of float


(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC 
val interp : value env -> exprC -> value 
val evaluate : exprC -> value 
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value


(* result post-processing *)
val valToString : value -> string
