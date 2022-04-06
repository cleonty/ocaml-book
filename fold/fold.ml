let rec sum = function 
  | [] -> 0
  | h :: t -> h + sum t

let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let rec combine init op = function
  | [] -> init
  | h :: t -> op h (combine init op t)

let sum' = combine 0 ( + )
let concat' = combine "" ( ^ )

let str = concat' ["a"; "b"; "c";]

let rec fold_right f lst acc = match lst with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc)

let rec fold_left f acc lst = match lst with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let rec fold_left ~op:(f: acc:'a -> elt:'b -> 'a) ~init:acc lst = 
  match lst with
  | [] -> acc
  | h :: t -> fold_left ~op:f ~init:(f ~acc:acc ~elt:h) t

let rec fold_right ~op:(f: elt:'a -> acc:'b -> 'b) lst ~init:acc =
  match lst with
  | [] -> acc
  | h :: t -> f ~elt:h ~acc:(fold_right ~op:f t ~init:acc)

let add ~acc ~elt = acc + elt
let s = fold_left ~op:add ~init:0 [1;2;3]

let length lst =
  List.fold_left (fun acc _ -> acc + 1) 0 lst

let rev lst = 
  List.fold_left (fun acc x -> x :: acc) [] lst

let map f lst =
  List.fold_right (fun x acc -> f x :: acc) [] lst 
  
let filter f lst =
  List.fold_right (fun x acc -> if f x then x :: acc else acc) lst []
  
let l = length [1;2;3;4]

let rec lst_and_rec = function
  | [] -> true
  | h :: t -> h && lst_and_rec t

let lst_and_fold =
	List.fold_left (fun acc elt -> acc && elt) true

let lst_and_lib =
	List.for_all (fun x -> x)

