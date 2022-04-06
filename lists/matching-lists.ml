let empty lst =
  match lst with
  | [] -> true
  | h :: t -> false

let empty lst =
  match lst with
  | [] -> true
  | _ :: _ -> false
  
let rec sum lst = 
  match lst with 
  | [] -> 0
  | h -> t -> h + sum t

let empty = function
  | [] -> true
  | _ :: _ -> false

  let rec sum = function
    | [] -> 0
    | h -> t -> h + sum t
  
  let rec length = function
    | [] -> 0
    | _ :: t -> 1 + length t 