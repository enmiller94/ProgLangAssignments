let t1a = range1 5 = [1; 2; 3; 4; 5]
let t1b = range1 1 = [1]

let t2a = tabulate (fun i -> i) 5 = [1; 2; 3; 4; 5]
let t2b = tabulate (fun i -> 2 * i) 5 = [2; 4; 6; 8; 10]

let t3a = string_of_pxl D = "."
let t3b = string_of_pxl H = "#"

let t4a = string_of_row [D;H;D;H] = ".#.#\n"
let t4b = string_of_row [] = "\n"

let t5a = string_of_pic base = ".#.#\n####\n....\n"
let t5b = string_of_pic doodad = "#######\n#.....#\n#.###.#\n#.#.#.#\n#.#...#\n#.#####\n#......\n#######\n"
let t5c = string_of_pic [[]] = "\n"

let t6a = flip_vertical base = [[H;D;H;D];[H;H;H;H];[D;D;D;D]]
let t6b = flip_vertical doodad = [[H;H;H;H;H;H;H];[H;D;D;D;D;D;H];[H;D;H;H;H;D;H];[H;D;H;D;H;D;H];[H;D;D;D;H;D;H];[H;H;H;H;H;D;H];[D;D;D;D;D;D;H];[H;H;H;H;H;H;H]]
let t6c = flip_vertical [[]] = [[]]

let t7a = flip_horizontal base = [[D;D;D;D];[H;H;H;H];[D;H;D;H]]
let t7b = flip_horizontal doodad = [[H;H;H;H;H;H;H];[H;D;D;D;D;D;D];[H;D;H;H;H;H;H];[H;D;H;D;D;D;H];[H;D;H;D;H;D;H];[H;D;H;H;H;D;H];[H;D;D;D;D;D;H];[H;H;H;H;H;H;H]]
let t7c = flip_horizontal [[]] = [[]]

let t8a = flip_both base = [[D;D;D;D];[H;H;H;H];[H;D;H;D]]
let t8b = flip_both doodad = [[H;H;H;H;H;H;H];[D;D;D;D;D;D;H];[H;H;H;H;H;D;H];[H;D;D;D;H;D;H];[H;D;H;D;H;D;H];[H;D;H;H;H;D;H];[H;D;D;D;D;D;H];[H;H;H;H;H;H;H]]
let t8c = flip_both [[H;D;H;D]] = [[D;H;D;H]]
let t8d = flip_both [[]] = [[]]

let t9a = mirror_vertical base = [[D;H;D;H;H;D;H;D];[H;H;H;H;H;H;H;H];[D;D;D;D;D;D;D;D]]
let t9b = mirror_vertical doodad = [[H;H;H;H;H;H;H;H;H;H;H;H;H;H];
									[H;D;D;D;D;D;H;H;D;D;D;D;D;H];
									[H;D;H;H;H;D;H;H;D;H;H;H;D;H];
									[H;D;H;D;H;D;H;H;D;H;D;H;D;H];
									[H;D;H;D;D;D;H;H;D;D;D;H;D;H];
									[H;D;H;H;H;H;H;H;H;H;H;H;D;H];
									[H;D;D;D;D;D;D;D;D;D;D;D;D;H];
									[H;H;H;H;H;H;H;H;H;H;H;H;H;H]]
let t10b = mirror_horizontal doodad = [[H;H;H;H;H;H;H];
									[H;D;D;D;D;D;H];
									[H;D;H;H;H;D;H];
									[H;D;H;D;H;D;H];
									[H;D;H;D;D;D;H];
									[H;D;H;H;H;H;H];
									[H;D;D;D;D;D;D];
									[H;H;H;H;H;H;H];
									[H;H;H;H;H;H;H];
									[H;D;D;D;D;D;D];
									[H;D;H;H;H;H;H];
									[H;D;H;D;D;D;H];
									[H;D;H;D;H;D;H];
									[H;D;H;H;H;D;H];
									[H;D;D;D;D;D;H];
									[H;H;H;H;H;H;H]]

let t9c = mirror_vertical [[]] = [[]]

let t10a = mirror_horizontal base = [[D;H;D;H];[H;H;H;H];[D;D;D;D];[D;D;D;D];[H;H;H;H];[D;H;D;H]]
let t10b = mirror_horizontal doodad = [[H;H;H;H;H;H;H];
									[H;D;D;D;D;D;H];
									[H;D;H;H;H;D;H];
									[H;D;H;D;H;D;H];
									[H;D;H;D;D;D;H];
									[H;D;H;H;H;H;H];
									[H;D;D;D;D;D;D];
									[H;H;H;H;H;H;H];
									[H;H;H;H;H;H;H];
									[H;D;D;D;D;D;D];
									[H;D;H;H;H;H;H];
									[H;D;H;D;D;D;H];
									[H;D;H;D;H;D;H];
									[H;D;H;H;H;D;H];
									[H;D;D;D;D;D;H];
									[H;H;H;H;H;H;H]]

let t11a = mirror_both base = [[D;H;D;H;H;D;H;D];[H;H;H;H;H;H;H;H];[D;D;D;D;D;D;D;D];[D;D;D;D;D;D;D;D];[H;H;H;H;H;H;H;H];[D;H;D;H;H;D;H;D]]
let t11b = mirror_both doodad = [[H;H;H;H;H;H;H;H;H;H;H;H;H;H];
									[H;D;D;D;D;D;H;H;D;D;D;D;D;H];
									[H;D;H;H;H;D;H;H;D;H;H;H;D;H];
									[H;D;H;D;H;D;H;H;D;H;D;H;D;H];
									[H;D;H;D;D;D;H;H;D;D;D;H;D;H];
									[H;D;H;H;H;H;H;H;H;H;H;H;D;H];
									[H;D;D;D;D;D;D;D;D;D;D;D;D;H];
									[H;H;H;H;H;H;H;H;H;H;H;H;H;H];
									[H;H;H;H;H;H;H;H;H;H;H;H;H;H];
									[H;D;D;D;D;D;D;D;D;D;D;D;D;H];
									[H;D;H;H;H;H;H;H;H;H;H;H;D;H];
									[H;D;H;D;D;D;H;H;D;D;D;H;D;H];
									[H;D;H;D;H;D;H;H;D;H;D;H;D;H];
									[H;D;H;H;H;D;H;H;D;H;H;H;D;H];
									[H;D;D;D;D;D;H;H;D;D;D;D;D;H];
									[H;H;H;H;H;H;H;H;H;H;H;H;H;H]]

let t12a = pixelate (fun i j -> D) 3 6 = [[D;D;D;D;D;D];[D;D;D;D;D;D];[D;D;D;D;D;D]]
let t12b = pixelate (fun i j -> D) 1 1 = [[D]]
let t12c = pixelate (fun i j -> if j = 1 then D else H) 2 2 = [[D;H];[D;H]]

let t13a = stack_vertical base base = [[D;H;D;H];[H;H;H;H];[D;D;D;D];[D;H;D;H];[H;H;H;H];[D;D;D;D]]
(*let t13b = try stack_vertical base sword with*)
(*let t13b = try stack_vertical base sword with
          | _ -> true*)

let t14a = stack_horizontal base base = [[D;H;D;H;D;H;D;H];[H;H;H;H;H;H;H;H];[D;D;D;D;D;D;D;D]]
(**)

let t15a = invert base = [[H;D;H;D];[D;D;D;D];[H;H;H;H]]
let t15b = invert [[]] = [[]]

let t16a = transpose base = [[D;H;D];[H;H;D];[D;H;D];[H;H;D]]
