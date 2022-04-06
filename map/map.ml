let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: add1 t
  
let lst1 = add1 [1; 2; 3]

let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t
  
let add1 = map (fun x -> x + 1)
let concat_bang = map (fun x -> x ^ "!")

let stringlist_of_intlist = map string_of_int

let rec map f = function
  | [] -> []
  | h :: t -> let h' = f h in h' :: map f t

let p x = print_int x; print_newline(); x + 1

let lst = map p [1; 2]


