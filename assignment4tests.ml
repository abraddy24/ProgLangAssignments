(* THUNKS *)
(* This complicated test ensures you don't call the thunk too soon *)
let t1a = let f = fun () -> raise (Failure "")
          in try (try (thunk f) with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t1b = (thunk (fun () -> 5)) () = 5
let t1c = (thunk (fun () -> "Hello")) () = "Hello"
let t1d = (thunk (fun () -> 5.2)) () = 5.2
let t1e = (thunk (fun () -> Some 3)) () = Some 3

let t2a = (thunk_of_value 4) () = 4
let t2b = (thunk_of_value 8) () = 8
let t2c = (thunk_of_value 5.4) () = 5.4
let t2d = (thunk_of_value 93.4) () = 93.4

let t3a = try (try (thunk_of_eval ((fun x -> raise (Failure "")), 4))
               with Failure "" -> (fun () -> false)) ()
          with Failure "" -> true
             | _ -> false
let t3b = thunk_of_eval ((fun x -> x + 1), 5) () = 6
let t3c = thunk_of_eval ((fun x -> x * 5), 3) () = 15
let t3d = thunk_of_eval ((fun x -> x - 2), 6) () = 4
let t3e = thunk_of_eval ((fun x -> x / 3), 9) () = 3

let t4a = try_thunk (fun () -> raise (Failure "hi")) = None
let t4b = try_thunk (fun () -> raise (Not_found)) = None
let t4c = try_thunk (fun () -> raise (Invalid_argument "Hello")) = None
let t4d = try_thunk (fun () -> 5) = Some 5
let t4e = try_thunk (fun () -> 50 + 6) = Some 56
let t4f = try_thunk (fun () -> 2.3) = Some 2.3

let t5a = let f = fun () -> raise (Failure "")
          in try (try (thunk_of_pair (f, f)) with Failure "" -> (fun () -> (1, 1))) () =
                  (0, 0)
             with Failure "" -> true
                | _ -> false
let t5b = thunk_of_pair ((fun () -> 4), (fun () -> 5)) () = (4, 5)
let t5c = thunk_of_pair ((fun () -> 8.7), (fun () -> 6.3)) () = (8.7, 6.3)
let t5d = thunk_of_pair ((fun () -> 3 + 2), (fun () -> 3 - 1)) () = (5, 2)

let t6a = let f = fun () -> raise (Failure "")
          in try (try thunk_map (f, f)
                  with Failure "" -> (fun () -> false)) ()
             with Failure "" -> true
                | _ -> false
let t6b = thunk_map ((fun () -> 4), (fun x -> 2 * x)) () = 8
let t6c = thunk_map ((fun () -> 5), (fun x -> 5 + x)) () = 10
let t6d = thunk_map ((fun () -> 8), (fun x -> x / 2)) () = 4
let t6e = thunk_map ((fun () -> 7), (fun x -> x - 7)) () = 0

let t7a = let f = fun () -> raise (Failure "")
          in try (try thunk_of_list [f; f]
                  with Failure "" -> (fun () -> [])) () = []
             with Failure "" -> true
                | _ -> false
let t7b = let f = fun () -> 5
          in thunk_of_list [f; f] () = [5; 5]
let t7c = let f = fun () -> 8 in thunk_of_list [f; f; f] () = [8; 8; 8]
let t7d = let f = fun () -> 2 + 3 in thunk_of_list [f] () = [5]

let t8a = insert (empty, "foo", 3) = [("foo", 3)]
let t8b = insert ([("foo", 3); ("hello", 4)], "bob", 2) = [("bob", 2); ("foo", 3); ("hello", 4)]
let t8c = insert ([("bob", 2); ("hello", 4)], "foo", 3) = [("bob", 2); ("foo", 3); ("hello", 4)]
let t8d = insert ([("bob", 2); ("foo", 3)], "hello", 4) = [("bob", 2); ("foo", 3); ("hello", 4)]

let t9a = has ([("foo", 2)], "foo") = true
let t9b = has ([("foo", 2)], "bob") = false
let t9c = has ([("bob", 2); ("foo", 3); ("hello", 4)], "bob") = true
let t9d = has ([("bob", 2); ("foo", 3); ("hello", 4)], "hello") = true
let t9e = has ([("bob", 2); ("foo", 3); ("hello", 4)], "foo") = true
let t9f = has ([("bob", 2); ("foo", 3); ("hello", 4)], "rob") = false 

let t10a = lookup ([("bar", 3); ("foo", 2)], "bar") = 3
let t10b = try (lookup ([("bar", 3); ("foo", 2)], "baz"); false)
           with Not_found -> true
(* In the following test the search should fail because your code
   should stop looking after baz, since "baz" > "bar".
   This is of course not a "proper" table, but it is a good test that
   your code behaves properly. *)
let t10c = try (lookup ([("baz", 3); ("bar", 2)], "bar"); false)
           with Not_found -> true
let t10d = lookup ([("bob", 2); ("foo", 3); ("hello", 4)], "bob") = 2
let t10e = lookup ([("bob", 2); ("foo", 3); ("hello", 4)], "hello") = 4
let t10f = lookup ([("bob", 2); ("foo", 3); ("hello", 4)], "foo") = 3
let t10g = try (lookup ([("bob", 2); ("foo", 3); ("hello", 4)], "hi"); false) with Not_found -> true


let t11a = lookup_opt ([("bar", 3); ("foo", 2)], "bar") = Some 3
(* Again the search should be stopping after "foo" *)
let t11b = lookup_opt ([("foo", 2); ("bar", 3)], "bar") = None
let t11c = lookup_opt ([("bob", 2); ("foo", 3); ("hello", 4)], "bob") = Some 2
let t11d = lookup_opt ([("bob", 2); ("foo", 3); ("hello", 4)], "hello") = Some 4
let t11e = lookup_opt ([("bob", 2); ("foo", 3); ("hello", 4)], "foo") = Some 3

let t12a = delete ([("bar", 3); ("baz", 1); ("foo", 2)], "baz") = [("baz", 1); ("foo", 2)]

let t13a = keys [("bar", 3); ("foo", 2)] = ["bar"; "foo"]

let t14a = is_proper [("bar", 3); ("foo", 2)] = true
