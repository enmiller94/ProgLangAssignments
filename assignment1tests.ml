(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (1, 2, 3) = (1, 2, 3)
let t1d = fixLastTwo (1, 3, 2) = (1, 2, 3)

let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (1, 2, 3) = (1, 2, 3)
let t2d = order (3, 2, 1) = (1, 2, 3)
let t2e = order (1, 3, 2) = (1, 2, 3)

let t3a = distance (6, 3) = 3
let t3b = distance (0, 0) = 0
let t3c = distance (3, 6) = 3

let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (1001, "Whosit") = "Greetings Whosit, you are 1001 years old!"

let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (1, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5c = greeting2 (20, "Jackson") = "Greetings Jackson, you are a youngster!"
let t5d = greeting2 (21, "Jackson") = "Greetings Jackson, you are young at heart!"
let t5e = greeting2 (30, "Jackson") = "Greetings Jackson, you are young at heart!"

let t6a = tooShort (4, "tree") = false
let t6b = tooShort (5, "tree") = true
let t6c = tooShort (100, "tree") = true
let t6d = tooShort (1, "tree") = false

let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("", "") = 0

let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("lon", "one", "ate") = true
let t8c = orderedByLength ("lo", "one", "ated") = true
let t8d = orderedByLength ("lg", "one", "a") = false

let t9a = prodInRange (3, 5) == true
let t9b = prodInRange (2, 5) == false
let t9c = prodInRange (4, 5) == false
let t9d = prodInRange (5, 5) == false
let t9e = prodInRange (1, 5) == false
