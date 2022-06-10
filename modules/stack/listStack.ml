module M : Stack.S = struct 
  type 'a t = 'a list
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | x :: _ -> x
  let pop = function [] -> failwith "Empty" | _ :: t -> t
end