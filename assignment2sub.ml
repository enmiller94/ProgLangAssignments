(* Programming Languages, Assignment 2 *)
(*
   You should write your functions in this file.
   All your functions should have their parameter types specified.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)


(*
   Write a function `getnth` that takes as input a pair of an integer n and a list
   of strings. It then returns the nth element in the list, counting from 1. If
   the list does not have n elements, it should raise an exception (Failure "getnth").
   It should have type: int * string list -> string
*)
let rec getnth ((n, lst): int * string list) =
	match lst with
	| []-> raise (Failure "getnth")
	| element::rest -> if n = 1 
					   then element 
					   else getnth(n-1,rest)



(*
   Write a function `lookup` that takes as input a pair of a string s and a list
   consisting of pairs of a string and an integer. It then goes through the list
   looking for a string that equals s, and if it finds one returns "Some i" where
   i is the corresponding integer in the pair. If it does not find a suitable
   pair then it returns "None".
   It should have type: string * (string * int) list -> int option
*)
let rec lookup ((s, lst): string * (string * int) list) = 
	match lst with
	| [] -> None 
	| (element_1, element_2)::rest -> if element_1 = s 
					   				  then Some element_2
					   				  else lookup (s, rest)




(*
   Write a function `inPairs` that takes a list of integers and returns a list
   of pairs of integers as follows: Starting from the head of the list, it puts
   together the next two integers in a pair and keeps going. If the list has an
   odd number of elements then the last element is left out. Example:
   inPairs [1; 2; 3; 4; 5] = [(1, 2); (3, 4)]
   It should have type: int list -> (int * int) list
*)
let rec inPairs ((lstInt): int list) = 
	match lstInt with
	| [] -> []
	| a::b::rest -> (a, b)::inPairs rest
	| a::rest -> []
	
	



(*
   Write a function `flatten` that takes as input a list of lists of integers
   and "flattens it out", returning a single list of integers which is the result
   of concatenating all the lists. Example:
   flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
   It should have type: int list list -> int list
*)
let rec flatten ((lst): int list list) =
	match lst with
	| []-> []
	| a::rest -> if a = [] 
				 then flatten rest
				 else a @ flatten rest



(*
   Write a function `remove` that takes as input a pair of an integer n and a
   list of integers, and removes from that list any occurrence of n.
   It should have type: int * int list -> int list
*)
let rec remove ((n, lstInt): int * int list) =
	match lstInt with 
	| [] -> []
	| a::rest when a = n -> remove (n, rest)
	| a::rest -> a::remove (n, rest)



(*
   Write a function `removeDups` that takes a list of integers and returns a
   list of the integers in the same order, but where any later occurrence of
   an integer after its first occurrence is removed. For example:
   removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
   It should have type: int list -> int list
*)
let rec removeDups ((lst): int list) =
	match lst with 
	| [] -> []
	| a::rest -> a::remove (a, removeDups rest)




(*
   Write a function `collateSome` that takes as input a list of int options
   and returns a list of integers. The list should consist of those integers that
   were part of a `Some v` in the list, and in the same order. For example:
   collateSome [Some 1; None; Some 2; Some 1; None; Some 3] = [1; 2; 1; 3]
   It should have type: int option list -> int list
*)
let rec collateSome ((lstOpt): int option list) =
	match lstOpt with
	| [] -> []
	| Some a::rest -> a::collateSome rest
	| None::rest -> collateSome rest



(*
   Write a function `unzip2` that takes as input a list of pairs of integers
   and returns a pair of lists of integers, formed by unpacking the pairs in the
   original list. For example:
   unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
   It should have type: (int * int) list -> int list * int list
*)
let rec unzip2 ((lst): (int * int) list) =
	match lst with
	| [] ->  ([],[])
	| (a, b)::rest -> let first, second = unzip2 rest in a::first, b::second




(*
   Write a function `makeChange` that takes as input a pair of an integer `n` and a
   list of distinct integers assumed to be in decreasing order. It should return
   an option type as follows: If there is a way to use the provided integers so that
   they add up to n, then it should return "Some lst" where lst is the list of the
   used integers, using the larger integers as much as possible. If there is no way
   it should return "None". Examples:
   makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
   makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
   makeChange (20, [13; 11]) = None
   Note that it tries to use the largest integers as much as possible. Make sure to
   write some good tests of this behavior.
   It should have type: int * int list -> int list option
*)
let rec makeChange ((n, lstDecr): int * int list) = 
	let lstEnd = [] in 
		match lstDecr with 
		| [] -> None 
		| a::rest -> if n - a = 0 
			     then Some (a::lstEnd)
			     else if n - a < 0 
			     then makeChange (n, rest) 
			     else let x = makeChange (n - a, lstDecr) in Some (a::lstEnd)