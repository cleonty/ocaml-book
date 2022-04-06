let get_val default o = match o with
  | None -> default
  | Some x -> x

let get_val default o = function
  | None => default
  | Some x -> x

  let rec list_max (lst : 'a list) : 'a option
    match lst with
    | [] -> None
    | h :: t -> begin 
      match list_max t with 
      | None -> Some h
      | Some m -> Some (max h m)
    end
  
  let x = list_max [1;2;3] (* 3 *)
  let y = list_max []
