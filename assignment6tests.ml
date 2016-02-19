(* These tests assume that you have implemented the function `take` as suggested *)
let t1a = take 4 (const 3) = [3; 3; 3; 3]
let t1b = take 10 (const 100) = [100; 100; 100; 100; 100; 100; 100; 100; 100; 100]

let t2a = take 5 (alt 3 4) = [3; 4; 3; 4; 3]
let t2b = take 6 (alt 5 10) = [5; 10; 5; 10; 5; 10]

let t3a = take 3 (seq 2 6) = [2; 8; 14]
let t3b = take 5 (seq 5 10) = [5; 15; 25; 35; 45]

let t4a = take 5 (from_f (fun x -> x * x)) = [1; 4; 9; 16; 25]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t4b = try (ignore (from_f (fun _ -> raise (Failure ""))); true) with
          | _ -> false
let t4c = take 5 (from_f (fun x -> x * 1)) = [1; 2; 3; 4; 5]
let t4d = take 5 (from_f (fun x -> x + x)) = [2; 4; 6; 8; 10]

let t5a = take 5 (from_list [3; 5; 6]) = [3; 5; 6; 3; 5]
let t5b = take 5 (from_list [1; 1; 1]) = [1; 1; 1; 1; 1]
let t5c = take 10 (from_list [3; 5; 6; 5; 3]) = [3; 5; 6; 5; 3; 3; 5; 6; 5; 3]

let t6a = take 3 (drop 3 (seq 2 6)) = [20; 26; 32]
let t6b = take 3 (drop 5 (seq 5 10)) = [55; 65; 75]
let t6c = take 3 (drop 10 (seq 1 1)) = [11; 12; 13]
let t6d = take 3 (drop 0 (seq 2 6)) = [2; 8; 14]

let t7a = take 6 (prepend [1; 2] (const 3)) = [1; 2; 3; 3; 3; 3]
let t7b = take 10 (prepend [1; 2; 3; 4; 5] (const 3)) = [1; 2; 3; 4; 5; 3; 3; 3; 3; 3]
let t7c = take 6 (prepend [] (const 3)) = [3; 3; 3; 3; 3; 3]

let t8a = take 6 (map (fun x -> x * x) (seq 1 1)) = [1; 4; 9; 16; 25; 36]
(* The next test ensures that the function is not called until the corresponding
   value is actually needed. *)
let t8b = try (ignore (map (fun _ -> raise (Failure "")) (seq 1 1)); true) with
          | _ -> false
let t8c = take 6 (map (fun x -> x) (seq 1 2)) = [1; 3; 5; 7; 9; 11]
let t8d = take 6 (map (fun x -> x + x) (seq 1 1)) = [2; 4; 6; 8; 10; 12]
let t8e = take 6 (map (fun x -> 0) (seq 1 1)) = [0; 0; 0; 0; 0; 0]

let t9a = take 3 (pair_up (seq 1 1)) = [(1, 2); (3, 4); (5, 6)]
let t9b = take 3 (pair_up (seq 1 2)) = [(1, 3); (5, 7); (9, 11)]
let t9c = take 1 (pair_up (seq 1 100)) = [(1, 101)]

let t10a = take 3 (zip2 (seq 1 2) (seq 2 3)) = [(1, 2); (3, 5); (5, 8)]
let t10b = take 3 (zip2 (seq 1 1) (seq 1 0)) = [(1, 1); (2, 1); (3, 1)]

let t11a = take 4 (accum (+) 0 (seq 1 1)) = [0; 1; 3; 6]

let t12a = take 4 (filter (fun x -> x mod 2 = 0) (seq 1 1)) = [2; 4; 6; 8]
let t12b = take 4 (filter (fun x -> x mod 2 = 1) (seq 1 1)) = [1; 3; 5; 7]
let t12c = take 4 (filter (fun x -> x > 5) (seq 1 1)) = [6; 7; 8; 9]

let t13a = take 3 (collect 3 (seq 1 1)) = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]
let t13b = take 3 (collect 3 (seq 1 2)) = [[1; 3; 5]; [7; 9; 11]; [13; 15; 17]]
let t13c = take 5 (collect 1 (seq 1 1)) = [[1]; [2]; [3]; [4]; [5]]

let t14a = take 5 (flatten (collect 3 (seq 1 1))) = [1; 2; 3; 4; 5]
let t14b = take 5 (flatten (collect 3 (seq 1 2))) = [1; 3; 5; 7; 9]
let t14c = take 5 (flatten ([[]; [1]; []; [1; 5; 6; 7]]) = [1; 1; 5; 6; 7]
