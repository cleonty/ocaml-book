module type Fact = sig
  (** [fact n] is [n] factorial. *)
  val fact : int -> int
end

module RecursiveFact: Fact = struct
  let rec fact n =
    if n = 0 then 1 else 
      n * fact (n - 1)
end

(* module NotFact : Fact = struct
  let inc x = x + 1
end *)

module TailRecursiveFact : Fact = struct 
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc)
  
  let fact n = fact_aux n 1
end

let f5 = TailRecursiveFact.fact 5

module type LIST_STACK = sig
  (** [Empty] is raised when an operation cannot be applied
      to an empty stack. *)
  exception Empty

  (** [empty] is the empty stack. *)
  val empty : 'a list

  (** [is_empty s] is whether [s] is empty. *)
  val is_empty : 'a list -> bool

  (** [push x s] pushes [x] onto the top of [s]. *)
  val push : 'a -> 'a list -> 'a list

  (** [peek s] is the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val peek : 'a list -> 'a

  (** [pop s] is all but the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val pop : 'a list -> 'a list
end

module ListStack: LIST_STACK = struct
  let empty = []

  let is_empty = function [] -> true | _ -> false

  let push x s = x :: s

  exception Empty

  let peek = function
    | [] -> raise Empty
    | x :: _ -> x

  let pop = function
    | [] -> raise Empty
    | _ :: s -> s
end

let s1 : int LIST_STACK.t = ListStack.(empty |> push 42)
let s2 = ListStack.push "jj" s

