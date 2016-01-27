let t1a = getnth (3, ["hi"; "there"; "you"]) = "you"
let t1b = try (getnth (3, ["hi"; "there"]); false)  with
            | Failure "getnth" -> true
            | _ -> false
let t1c = try (getnth(0, ["hey"; "you"; "there"]); false) with
			| Failure "getnth" -> true
			| _ -> false
let t1d = try (getnth(4, ["hey"; "hi"; "hello"]); false) with
			| Failure "getnth" -> true
			| _ -> false

let t2a = lookup ("you", []) = None
let t2b = lookup ("you", [("him", 2); ("you", 3)]) = Some 3
let t2c = lookup ("hello", [("yes", 4); ("bob", 89); ("franklin", 25); ("hello", 850)]) = Some 850
let t2d = lookup ("", [("lady", 8); ("carpenter", 9); ("", 20); ("hello", 78); ("quincy", 7); ("minutemen", 65)]) = Some 20

let t3a = inPairs [1; 2; 3; 4; 5] = [(1, 2); (3, 4)]
let t3b = inPairs [5; 3; 6; 2; 8; 7] = [(5, 3); (6, 2); (8, 7)]
let t3c = inPairs [] = []
let t3d = inPairs [1] = []
let t3e = inPairs [3; 5] = [(3, 5)]

let t4a = flatten [[1; 2; 3]; []; [4; 5]; [6]] = [1; 2; 3; 4; 5; 6]
let t4b = flatten [[1;2]; [3]; []] = [1; 2; 3]
let t4c = flatten [] = []
let t4d = flatten [[1]; []] = [1] 

let t5a = remove (3, [3; 4; 3; 1]) = [4; 1]
let t5b = remove (4, []) = []
let t5c = remove (9, [1; 2; 3; 4; 5]) = [1; 2; 3; 4; 5]
let t5d = remove (4, [4; 4; 4; 4; 4]) = []

let t6a = removeDups [4; 1; 2; 1; 4; 5; 20] = [4; 1; 2; 5; 20]
let t6b = removeDups [] = []
let t6c = removeDups [5; 5; 5; 5; 5] = [5]

let t7a = collateSome [Some 1; None; Some 2; Some 1; None; Some 3] = [1; 2; 1; 3]
let t7b = collateSome [] = []
let t7c = collateSome [None] = []
let t7d = collateSome [None; Some 8; Some 5; Some 2; None; Some 9; None; None] = [8; 5; 2; 9]

let t8a = unzip2 [(1, 2); (3, 4); (5, 6)] = ([1; 3; 5], [2; 4; 6])
let t8b = unzip2 [] = ([], [])
let t8c = unzip2 [(8, 9)] = ([8], [9])
let t8d = unzip2 [(9, 8); (7, 6); (5, 4); (3,2); (1, 0)] = ([9; 7; 5; 3; 1], [8; 6; 4; 2; 0])
(*
let t9a = makeChange (20, [8; 3; 2]) = Some [8; 8; 2; 2]
let t9b = makeChange (20, [8; 3]) = Some [8; 3; 3; 3; 3]
let t9c = makeChange (20, [13; 11]) = None
*)