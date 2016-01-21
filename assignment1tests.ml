(* Add your own tests. Make sure to pay attention to edge cases. *)
let t1a = fixLastTwo (5, 1, 2) = (5, 1, 2)
let t1b = fixLastTwo (5, 2, 1) = (5, 1, 2)
let t1c = fixLastTwo (1, 2, 3) = (1, 2, 3)
let t1d = fixLastTwo (9, 8, 7) = (9, 7, 8)
let t1e = fixLastTwo (5, 3, 1) = (5, 1, 3)

let t2a = order (2, 5, 3) = (2, 3, 5)
let t2b = order (5, 3, 2) = (2, 3, 5)
let t2c = order (1, 2, 3) = (1, 2, 3)
let t2d = order (6, 5, 4) = (4, 5, 6)
let t2e = order (5, 6, 4) = (4, 5, 6)
let t2f = order (9, 5, 2) = (2, 5, 9)

let t3a = distance (6, 3) = 3
let t3b = distance (0, 0) = 0
let t3c = distance (4, 9) = 5
let t3d = distance (9, 4) = 5

let t4a = greeting (23, "Pete") = "Greetings Pete, you are 23 years old!"
let t4b = greeting (21, "Alex") = "Greetings Alex, you are 21 years old!"

let t5a = greeting2 (0, "Jackson") = "Greetings Jackson, you are not born yet!"
let t5b = greeting2 (1, "Sam") = "Greeetings Sam, you are a youngster!"
let t5c = greeting2 (20, "Bob") = "Greetings Bob, you are a youngster!:
let t5d = greeting2 (21, "Courtney") = "Greetings Courtney, you are young at heart!"
let t5d = greeting2 (50, "Rob") = "Greeting Rob, you are young at heart!"

let t6a = tooShort (4, "tree") = false
let t6b = tooShort (4, "see") = true
let t6c = tooShort (1, "Hello") = false
let t6d = tooShort (50, "Hello") = true
let t6e = tooShort (8, "friend") = true

let t7a = totalLength ("you", "me") = 5
let t7b = totalLength ("hello", "goodbye") = 12
let t7c = totalLength ("", "") = 0

let t8a = orderedByLength ("long", "one", "at") = false
let t8b = orderedByLength ("bob", "two", "see") = true
let t8c = orderedByLength ("hi", "bye", "sigh") = true
let t8d = orderedByLength ("sigh", "hi", "bye") = false
let t8e = orderedByLength ("hi", "sigh", "bye") = false

let t9a = prodInRange (3, 5) == true
let t9b = prodInRange (2, 5) == false
let t9c = prodInRange (5, 4) == false
let t9d = prodInRange (6, 2) == true
let t9e = prodInRange (3, 3) == false
let t9f = prodInrange (4, 4) == true
