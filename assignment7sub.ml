(* Programming Languages, Assignment 7 *)
(*
   You should write your functions in this file.
   You should NOT specify the types of your functions. Let the system determine
   them for you.

   The instructions for this assignment reside in an auxiliary file, assignment7doc.md
   You should start by reading that file.
*)
(* ---------------------------------
              HELPERS
   ---------------------------------

   Place your "helpers" implementations here.
*)
let rec range a b = if a > b then [] else a :: range (a + 1) b

(*
Write a function `range1` that takes as input a single integer as input 
and returns the list of integers from 1 to that integer. This should be 
extremely easy. Use `range`. It should have type `int -> int list`.
*)
let range1 n = range 1 n

(*
Write a function `tabulate` that takes as input a function of type 
`int -> 'a` and also an integer `n` and uses it to generate the `'a list` 
consisting of the values of the provided function on the integers `1` 
through `n`. Reference solution is one line. 
Should have type: `(int -> 'a) -> int -> 'a list` 
*)
let tabulate f n = List.map f (range1 n)

(* ---------------------------------
              PICTURES
   ---------------------------------

   Place our Pictures implementations here after the type declarations and
   sword definition.
*)
type pixel = D | H
type row = pixel list
type pic = row list

exception IncompatibleDims

let sword = [
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;D;D;D;D;D;D;D;D;D;D;D;D;D];
[D;H;H;H;H;D;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;D;D;D;D;D;D;D;D;D;D];
[D;D;H;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;H;H;H;H;D;D;D;D;D;D;D;D;D];
[D;D;D;D;H;H;H;H;D;D;D;D;D;D;D;D];
[D;D;D;D;D;D;H;H;H;D;D;D;D;D;D;D];
[D;D;D;D;D;D;D;H;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;H;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;D;H;H;H;D;D;D;D];
[D;D;D;D;D;D;D;D;H;H;D;D;H;D;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;H;D;D];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H];
[D;D;D;D;D;D;D;D;D;D;D;D;D;D;H;H]]

let doodad = [
[H;H;H;H;H;H;H];
[H;D;D;D;D;D;H];
[H;D;H;H;H;D;H];
[H;D;H;D;H;D;H];
[H;D;H;D;D;D;H];
[H;D;H;H;H;H;H];
[H;D;D;D;D;D;D];
[H;H;H;H;H;H;H]]

let base = [
[D;H;D;H];
[H;H;H;H];
[D;D;D;D]]

(*
   These two functions provided to you. Study how they work before continuing!
*)
(*
You are provided with a function `valid_pic` that is given a picture and 
returns a boolean indicating whether the picture is "valid", in the sense 
of having all rows of equal length. Every function that follows assumes that 
it is provided a valid picture. Study this function, and the next.
*)
let valid_pic pic =
   match List.map List.length pic with
   | [] -> true
   | x :: xs -> List.for_all ((=) x) xs


(*
You are provided a function `dims_pic` that is given a (valid) picture and 
returns a pair of the number of rows and number of columns of the picture.
*)
let dims_pic pic = 
   match pic with 
   | [] -> (0, 0) 
   | row :: _ -> (List.length pic, List.length row) 

(*
Write a function `string_of_pxl` that takes as input a pixel and it returns a string, 
a single dot if the pixel is a D, the hash/pound sign # if the pixel is H. This is a very 
simple function. Should have type: `pixel -> string`
*)
let string_of_pxl pxl = 
   if pxl = D
   then "."
   else (* H *) "#" 

(*
Write a function `string_of_row` that takes as input a row and returns a string 
consisting of the concatenation of strings corresponding to the pixels in the row.  
The resulting string should include a newline `"\n"` at the end. Reference solution 
is 1 line. Should have type: `row -> string`
*)
let string_of_row rw = List.fold_right (fun pxl rest -> (string_of_pxl pxl) ^ rest) rw ("\n")

(*
Write a function `string_of_pic` that takes as input a picture and returns a string 
consisting of the concatenation of the strings produced by each row. The rows already contain 
newline characters. You do not need to add any extra newlines. Reference solution is 1 line. 
Should have type: `pic -> string`
*)
let string_of_pic pic = List.fold_right (fun rw rest -> (string_of_row rw) ^ rest) pic ("")

(*
Write a function `flip_vertical` that takes as input a picture and returns a picture that 
is a vertical flip of the original one. Reference solution is 1 line.
Should have type: `pic -> pic`
*)
let flip_vertical pic = List.fold_right (fun rw rest -> List.rev rw :: rest) pic []

(*
Write a function `flip_horizontal` that takes as input a picture and returns a picture that 
is a horizontal flip of the original one. Reference solution is 1 line. 
Should have type: `pic -> pic`
*)
let flip_horizontal pic = List.rev pic

(*
Write a function `flip_both` that takes as input a picture and returns a picture that is a 
"double flip" of the original one, doing both a horizontal and a vertical flip. 
The order of flips would not matter. Reference solution is 1 line. 
Should have type: `pic -> pic`
*)
let flip_both pic = flip_vertical (flip_horizontal pic)

(*
Write a function `mirror_vertical` that takes as input a picture and returns a picture that 
contains two copies of the original picture, one below the other but where the second copy 
is a mirror image of the first one, as produced by `flip_vertical`. You do not have to use 
`flip_vertical` along the way. It will have twice the number of rows as the original one. 
Reference solution is 1 line. 
Should have type: `pic -> pic`
*)
let mirror_vertical pic = List.fold_right (fun rw rest -> (rw @ List.rev rw) :: rest) pic []

(*
Write a function `mirror_horizontal` that takes as input a picture and returns a picture that contains 
two copies of the original picture, one next the other horizontally, but where the second copy is a mirror 
image of the first one as produced for example by `flip_horizontal` (but you will likely not be able to 
call `flip_horizontal`). It will have twice as long rows as the original one. Reference solution is 1 line. 
Should have type: `pic -> pic`
*)
let mirror_horizontal pic = pic @ flip_horizontal pic

(*
Write a function `mirror_both` that takes as input a picture and returns a picture that contains 
four copies of the original picture, arranged in a 2x2 format, and each obtained from the nearby one via a 
flip in that direction. When applied to the sword picture, it should produce four swords emanating from its center. 
Reference solution is 1 line. Should have type: `pic -> pic`
*)
let mirror_both pic = mirror_horizontal (mirror_vertical pic)

(*
Write a function `pixelate` that takes as input a function `f` of type `int -> int -> pixel` and two 
integers `m` and `n`, in that order, and produces a picture of `m` rows and `n` "columns". The pixel at 
the i-th row and j-th column (in other words the j-th pixel in the i-th row) is determined by the value 
of the function `f`. Use `tabulate` from earlier (in two places). Reference solution is 1-2 lines. 
Should have type: `(int -> int -> pixel) -> int -> int -> pic`
*)
let pixelate f m n = List.map (fun row_num -> List.fold_right (fun col_num rest -> (f row_num col_num) :: rest) (tabulate (fun y -> y) n) []) (tabulate (fun x -> x) m)

(*
Write a function `stack_vertical` that takes as input two pictures and places them one atop the other 
in a vertical fashion. It should raise the exception `IncompatibleDims` if the pictures have different 
number of columns. Reference solution is 2 lines. 
Should have type: `pic -> pic -> pic`.
*)
let stack_vertical pic1 pic2 = let (h1, w1) = (dims_pic pic1) and (h2, w2) = (dims_pic pic2) in
                                  if w1 = w2
                                  then pic1 @ pic2
                                  else raise (IncompatibleDims)

(*
Write a function `stack_horizontal` that takes as input two pictures and places them next to 
each other in a horizontal fashion. It should raise the exception `IncompatibleDims` if the pictures 
have different number of rows. You should use `List.fold_right2`. You can handle the dimension check 
by either using the function `dims` provided earlier or letting `List.fold_right2` throw its exception 
(see documentation) and catching it. Or try them both! Reference solution is 2-4 lines. 
Should have type: `pic -> pic -> pic`.
*)
let stack_horizontal pic1 pic2 = try (List.fold_right2 (fun rw1 rw2 rest -> (rw1 @ rw2) :: rest) pic1 pic2 []) with 
                                 | _ -> raise (IncompatibleDims) 

(*
Write a function `invert` that takes as input a picture and returns the same picture with the 
two "colors" inverted. Reference solution is 2-4 lines. Should have type: `pic -> pic`
*)
let invert pic = List.fold_right (fun rw rest -> (List.map (fun pxl -> if pxl = D
                                                                       then H
                                                                       else D) rw) :: rest) pic []

(*
Write a function `transpose` that takes as input a picture and returns the 
result of "transposing" the picture, i.e. turning its rows into columns. This 
one is short but tricky. The reference solution is 4 lines and uses `List.fold_right`, 
`List.map` and `List.map2` along with a let binding and a conditional. Start by working out 
manually in a small example how a recursive implementation might function (but your final 
solution is not meant to be recursive; this would just help you figure out the kind of work 
that your `fold_right` would have to do). Should have type: `pic -> pic`
*)
let transpose pic = List.fold_right (fun rw rest -> if rest = [] 
                                                    then List.map (fun pix -> [pix]) rw
                                                    else List.map2 (fun lst1 lst2 -> lst1 :: lst2) rw rest) pic []
