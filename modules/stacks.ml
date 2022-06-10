module type StackSig = sig
  type 'a stack
  val empty: 'a stack 
  val push: 'a -> 'a stack -> 'a stack
  val peek: 'a stack -> 'a
  val pop: 'a stack -> 'a stack
end

module MyStack : StackSig = struct
  type 'a stack = 
    | Empty
    | Entry of 'a * 'a stack

  let empty = Empty
  
  let push x s = Entry (x, s)
  
  let peek = function
    | Empty -> failwith "Empty"
    | Entry (x, _) -> x
    
  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_, s) -> s
end

module ListStack : StackSig = struct
  type 'a stack = 'a list
  
  let empty = []
  
  let push x s = x :: s
  
  let peek = function
    | [] -> failwith "Empty"
    | h :: t -> h
  
  let pop = function
    | [] -> failwith "Empty"
    | h :: t -> t
end

let s = ListStack.empty
let s' = ListStack.push 1 s
let x = ListStack.peek s'

let x = ListStack.peek (ListStack.push 42 ListStack.empty)
let x' = ListStack.(peek (push 42 ListStack.empty))
let x'' = ListStack.(empty |> push 42 |> peek)

let w =
  let open ListStack in
    empty |> push 42 |> peek
    
open ListStack
let v = empty |> push 42 |> peek

