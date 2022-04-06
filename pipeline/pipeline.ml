let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j
let square x = x * x
let sum = List.fold_left ( + ) 0

let sum_sq n = 
  0 -- n
  |> List.map square
  |> sum

let sum_sq_6 = sum_sq 6