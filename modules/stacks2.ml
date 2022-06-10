
module ListStackImpl = struct
  exception Empty
  type 'a t = 'a list
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: t -> t
  let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0> top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt (List.rev s);
    fprintf fmt "@, bottom of stack@]" 
end

module MyStack : Stack = struct
  type 'a t = 
    | Empty
    | Entry of 'a * 'a t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  
  let push x s = Entry (x, s)
  
  let peek = function
    | Empty -> failwith "Empty"
    | Entry (x, _) -> x
    
  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_, s) -> s
    
  let pp pp_val fmt s = ()
end

module CustomStack : Stack = struct
  type 'a entry = {top : 'a; rest : 'a t; size : int}
  and 'a t = S of 'a entry option
  exception Empty
  let empty = S None
  let is_empty = function S None -> true | _ -> false
  let size = function S None -> 0 | S (Some {size}) -> size
  let push x s = S (Some {top = x; rest = s; size = size s + 1})
  let peek = function S None -> raise Empty | S (Some {top}) -> top
  let pop = function S None -> raise Empty | S (Some {rest}) -> rest
  let pp pp_val fmt s = ()
end

module ListStack : Stack = ListStackImpl

let s1 : int ListStack.t = ListStack.(empty |> push 42)
let s2 : int ListStackImpl.t = [42]

let kupo_pp fmt s = Format.fprintf fmt "%s kupo" s

(* #install_printer ListStack.pp;; *)