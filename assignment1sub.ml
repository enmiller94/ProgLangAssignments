(* This is an OCAML comment *)
(*
   You should write your functions in this file.
   All your functions should have their parameter types specified.
   Write your code right below the corresponding comment describing the
   function you are asked to write.
*)


(*
   Write a function named "fixLastTwo" that takes a triple of integers
   (x, y, z) and possibly rearranges the last two so that they are in increasing
   order (and just keeps the first in place).
   It should have type: int * int * int -> int * int * int
*)
let fixLastTwo (x, y, z): int*int*int =
	if y > z then (x, z, y)
	else (x, y, z)


(*
   Write a function named "order" that takes a triple of integers and
   returns a triple of the same integers but in increasing order.
   You may want to use the function from the previous part.
   It should have type: int * int * int -> int * int * int
*)
let order ((x, y, z): int*int*int) = 
	let (x',y',z') = fixLastTwo(x, y, z) in 
	if x' > y' then 
		if x' > z' then (y', z', x') 
		else (y', x', z')
	else (x', y', z')


(*
   Write a function "distance" that given a pair of integers returns the
   distance between them. For instance the distance between 10 and 4 is 6,
   as is the distance between 4 and 10.
   It should have type: int * int -> int
*)
let distance ((x, y): int*int) = 
	if x > y then x-y
	else y-x


(*
   Write a function "greeting" that given a pair of an integer (age) and
   a string (name) creates the string: "Greetings <name>, you are <age> years old!".
   You will need to look in the Pervasives module for "string concatenation"
   and for how to convert an int to a string (NOT to a char).
   It should have type: int * string -> string
   You may see "bytes" instead of "string" as a type.
*)
let greeting ((age, name): int*string)=
	"Greetings " ^ name ^ ", you are " ^ string_of_int age ^ " years old!"


(*
   Write a function "greeting2" that is similarly given a pair of an integer (age)
   and a string (name) and creates the string: "Greetings <name>, you are ..." where
   the dots depend on the age:
   - If the age is 0 or less, it should say "not born yet!".
   - If the age is 1 to 20, it should say "a youngster!".
   - If the age is more than 20, it should say "young at heart!".
   It should have type: int * string -> string
   You may see "bytes" instead of "string" as a type.
*)
let greeting2 ((age, name): int*string) = 
	if age <= 0 then "Greetings " ^ name ^ ", you are not born yet!"
	else if age > 0 && age <21 then "Greetings " ^ name ^ ", you are a youngster!"
	else "Greetings " ^ name ^ ", you are young at heart!"


(*
   Write a function "tooShort" that is given a pair of an integer and a string
   and returns a boolean indicating whether that integer is strictly larger than
   the length of the string. The function "String.length" returns the length of
   a string.
   It should have type: int * string -> bool
*)
let tooShort ((i, s): int*string)= 
	if i > String.length s then true
	else false


(*
   Write a function "totalLength" that is given a pair of strings and returns
   their total length.
   It should have type string * string -> int
*)
let totalLength ((s1, s2): string*string) =
	String.length s1+ String.length s2



(*
   Write a function "orderedByLength" that is given a triple of strings and returns
   a boolean indicating whether the strings are ordered in increasing length. For a
   fully correct solution, your code should not compute the length of a specific
   string more than once.
   It should have type: string * string * string -> bool
*)
let orderedByLength ((x, y, z): string*string*string) = 
	let x_l = String.length x and
		y_l = String.length y and
		z_l = String.length z in

	x_l <= y_l && y_l <= z_l



(*
   Write a function "prodInRange" that is given a pair of integers, and it returns
   a boolean indicating whether their product is strictly between 10 and 20. For a
   fully correct solution, your code should not compute the product of the two
   integers more than once.
   It should have type: int * int -> bool
*)
let prodInRange ((x, y): int*int) =
	let prod = x*y in 

	if prod > 10 && prod < 20 then true
	else false
