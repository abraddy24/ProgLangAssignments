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

let range1 n = range 1 n

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

(*
   You need to fix this.
*)
let doodad =[
             [D; D; D; D; D; D; D; D];
             [D; D; D; D; H; D; D; D];
             [D; D; D; H; D; H; D; D];
             [D; D; H; H; H; H; H; D];
             [D; H; D; D; H; D; D; H];
             [D; H; D; D; H; D; D; H];
             [D; H; D; D; H; D; D; H]]

(*
   These two functions provided to you. Study how they work before continuing!
*)
let valid_pic pic =
   match List.map List.length pic with
   | [] -> true
   | x :: xs -> List.for_all ((=) x) xs

let dims_pic pic =
   match pic with
   | [] -> (0, 0)
   | row :: _ -> (List.length pic, List.length row)

(*
   Add your other functions here
*)

let string_of_pxl p = if p = D then "." else (* H *) "#"

let string_of_row row = List.fold_right (fun p rest -> (string_of_pxl p) ^ rest) row ("\n")

let string_of_pic pic = List.fold_right (fun row rest -> (string_of_row row) ^ rest) pic ("")

let flip_vertical pic = List.fold_right (fun row rest -> List.rev row :: rest) pic []

let flip_horizontal pic = List.rev pic

let flip_both pic = flip_vertical (flip_horizontal pic)

let mirror_vertical pic = List.fold_right (fun row rest -> (row @ List.rev row) :: rest) pic []

let mirror_horizontal pic = pic @ flip_horizontal pic

let mirror_both pic = mirror_horizontal (mirror_vertical pic)

let pixelate f m n = List.map (fun row_n -> List.fold_right (fun col_n rest -> (f row_n col_n) :: rest) (tabulate (fun y -> y) n))

let stack_vertical pic1 pic2 = let (height1, width1) = (dims_pic pic1) and (height2, width2) = (dims_pic pic2) in if (width1 = width2) then pic1 @ pic2 else raise (IncompatibleDims)

let stack_horizontal pic1 pic2 = try (List.fold_right2 (fun row1 row2 rest -> (row1 @ row2) :: rest) pic1 pic2 []) with | _ -> raise (IncompatibleDims)

let invert pic = List.fold_right (fun row rest -> (List.map (fun p -> if p = D then H else D) row) :: rest) pic []

let transpose pic = List.fold_right (fun row rest -> if rest = [] then List.map (fun p -> [p]) row else List.map2 (fun lst lst2 -> lst :: lst2) row rest) pic []
