(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false


let t1b = (thunk (fun () -> 5)) () = 5
let t1c = (thunk (fun () -> 10)) () = 10
let t1d = (thunk (fun () -> 15 + 5)) () = 20
let t1e = (thunk (fun () -> 15 - 5)) () = 10
let t1f = (thunk (fun () -> 15 * 5)) () = 75
let t1g = (thunk (fun () -> 15.0 /. 5.0)) () = 3.0


let t2a = (thunk_of_value 4) () = 4
let t2b = (thunk_of_value 10) () = 10
let t2c = (thunk_of_value 150.0 ) () = 150.0


let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6
let t3c = thunk_of_eval ((fun x -> x + x), 5) () = 10
let t3d = thunk_of_eval ((fun x -> x *. 2.0), 5.0) () = 10.0
let t3e = thunk_of_eval ((fun x -> x /. 10.0), 5.0) () = 0.5
let t3f = thunk_of_eval ((fun x -> x - 2), 5) () = 3


let t4a = try_thunk (fun () -> raise (Failure "hi")) = None
let t4b = try_thunk (fun () -> raise (Not_found)) = None
let t4c = try_thunk (fun () -> raise (Invalid_argument "hi")) = None
let t4d = try_thunk (fun () -> 10) = Some 10
let t4e = try_thunk (fun () -> 100 - 100) = Some 0
let t4f = try_thunk (fun () -> 10.0) = Some 10.0


let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)
let t5c = thunk_of_pair ((fun () -> 4 + 6), (fun () -> 5 / 5)) () = (10, 1)
let t5d = thunk_of_pair ((fun () -> 4.0), (fun () -> 5.0)) () = (4.0, 5.0)
let t5e = thunk_of_pair ((fun () -> 4.0 -. 4.0), (fun () -> 5.0 *. 5.0)) () = (0.0, 25.0)


let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8
let t6c = thunk_map ((fun () -> 4), (fun x -> 2 - x)) () = (-2)
let t6d = thunk_map ((fun () -> 4.0), (fun x -> 2.0 /. x)) () = 0.5
let t6e = thunk_map ((fun () -> 4), (fun x -> 2 + x)) () = 6


(*)
let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]
*)


let t8a = insert (empty, "foo", 3) = [("foo", 3)]
let t8b = insert ([("doo", 5)], "foo", 3) = [("doo", 5); ("foo", 3)]
let t8c = insert ([("goo", 5)], "foo", 3) = [("foo", 3); ("goo", 5)]
let t8d = insert ([("foo", 3); ("hoo", 7)], "goo", 5) = [("foo", 3); ("goo", 5); ("hoo", 7)]
let t8e = insert ([("foo", 3); ("goo", 5); ("hoo", 7)], "goo", 5) = [("foo", 3); ("goo", 5); ("hoo", 7)]


let t9a = has ([("foo", 2)], "foo") = true
let t9b = has (empty, "foo") = false
let t9c = has ([("foo", 2); ("foo", 2); ("foo", 2); ("foo", 2)], "foo") = true
let t9d = has ([("aoo", 2); ("boo", 2); ("coo", 2); ("doo", 2); ("foo", 2)], "foo") = true
let t9e = has ([("boo", 2)], "foo") = false


let t10a = lookup ([("bar", 3); ("foo", 2)], "bar") = 3
let t10b = try (lookup ([("bar", 3); ("foo", 2)], "baz"); false)
           with Not_found -> true
(* In the following test the search should fail because your code
   should stop looking after baz, since "baz" > "bar".
   This is of course not a "proper" table, but it is a good test that
   your code behaves properly. *)
let t10c = try (lookup ([("baz", 3); ("bar", 2)], "bar"); false)
           with Not_found -> true
let t10d = try (lookup (empty, "baz"); false)
           with Not_found -> true
let t10e = try (lookup ([("bar", 3); ("foo", 2); ("zoo", 5)], "hoo"); false)
           with Not_found -> true
let t10f = lookup ([("foo", 3); ("goo", 5); ("hoo", 7)], "hoo") = 7


let t11a = lookup_opt ([("bar", 3); ("foo", 2)], "bar") = Some 3
(* Again the search should be stopping after "foo" *)
let t11b = lookup_opt ([("foo", 2); ("bar", 3)], "bar") = None
let t11c = lookup_opt ([("foo", 3); ("goo", 5); ("zoo", 7)], "hoo") = None
let t11d = lookup_opt ([("foo", 3); ("goo", 5); ("hoo", 7)], "hoo") = Some 7
let t11e = lookup_opt ([("foo", 3); ("goo", 5); ("hoo", 7)], "goo") = Some 5
let t11f = lookup_opt (empty, "bar") = None


let t12a = delete ([("bar", 3); ("baz", 1); ("foo", 2)], "baz") = [("bar", 3); ("foo", 2)]
let t12b = delete (empty, "baz") = []
let t12c = delete ([("foo", 3); ("goo", 5); ("hoo", 7)], "hoo") = [("foo", 3); ("goo", 5)]
let t12d = delete ([("foo", 3); ("goo", 5); ("hoo", 7)], "goo") = [("foo", 3); ("hoo", 7)]
let t12e = delete ([("foo", 3); ("goo", 5); ("zoo", 7)], "hoo") = [("foo", 3); ("goo", 5); ("zoo", 7)]
let t12f = delete ([("foo", 3); ("goo", 5); ("hoo", 7)], "foo") = [("goo", 5); ("hoo", 7)]


let t13a = keys [("bar", 3); ("foo", 2)] = ["bar"; "foo"]
let t13b = keys empty = []
let t13c = keys [("bar", 3)] = ["bar"]
let t13d = keys [("bar", 3); ("foo", 2); ("goo", 5); ("hoo", 7)] = ["bar"; "foo"; "goo"; "hoo"]


let t14a = is_proper [("bar", 3); ("foo", 2)] = true
let t14a = is_proper [("foo", 3); ("goo", 5); ("hoo", 7)] = true
let t14a = is_proper [("zoo", 3); ("goo", 5); ("hoo", 7)] = false
let t14a = is_proper [("foo", 3); ("goo", 5); ("hoo", 7); ("poo", 3); ("zoo", 5); ("too", 7)] = false
let t14a = is_proper empty = true
