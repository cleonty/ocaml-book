(**
type intlist = 
 | Nil
 | Cons of int * intlist
 
let rec length_intlist = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length_intlist t 

type stringlist = 
 | Nil
 | Cons of string * intlist
 
let rec length_stringlist = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length_stringlist t
*)
(*
type 'a myList = 
 | Nil
 | Cons of 'a * 'a myList

let rec length = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t
*)

type 'a myList = 
 | []
 | (::) of 'a * 'a myList

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t