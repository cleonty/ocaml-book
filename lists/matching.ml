let bad_empty lst =
  match lst with
  | [] -> true

let rec bad_sum' lst =
  List.hd lst + bad_sum' (List.tl lst)

  let f x y = function 
    |