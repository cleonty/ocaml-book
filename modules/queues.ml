module type Queue = sig
  type 'a queue
  val empty : 'a queue
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a option
  val dequeue : 'a queue -> 'a queue option
end

module ListQueueImpl = struct 
  type 'a queue = 'a list
  
  let empty = []
  
  let enqueue x q = q @ [x] (* linear time :( )*)
  
  let peek = function
    | [] -> None
    | x :: _ -> Some(x)
  
  let dequeue = function
    | [] -> None
    | _ :: q -> Some(q)
end

module TwoListQueue = struct
(*
  {front = [a; b]; back [e; d; c]}
  represents the queue a,b,c,d,e
  if [front] is empty then [back] must also be empty,
  to guarantee that the first element of the queue
  is always the head of [front]. 
    
*)
  type 'a queue = {
    front: 'a list;
    back: 'a list;
  }
  
  let empty = {
    front = [];
    back = [];
  }
  
  let peek = function
    | {front = []} -> None
    | {front = x :: _} -> Some x
  
  let enqueue x = function
    | {front = []} -> {front = [x]; back = []}
    | q -> {q with back = x :: q.back }
  
  let dequeue = function
    | {front = []} -> None
    | {front = _ :: []; back } -> Some({front = List.rev back; back = []})
    | {front = _ :: t; back } -> Some({front = t; back})
end


(* let q : int list = ListQueueImpl.(empty |> enqueue 42 |> dequeue |> enqueue 43) *)

(* module ListQueue : Queue = ListQueueImpl *)

(* let ( |> ) x f = f x *)

(* Option.map *)

let ( >>| ) opt f = 
match opt with
| None -> None
| Some x -> Some (f x)

(* Option.bind *)
let ( >>=| ) opt f = 
match opt with
| None -> None
| Some x -> f x

let q : int list option = ListQueueImpl.(empty |> enqueue 42 |> dequeue >>| enqueue 43 >>=| dequeue)

let q : int list option =
  let open ListQueueImpl in 
  empty
  |> enqueue 42
  |> dequeue
  >>| enqueue 43
  >>=| dequeue
  
module ListQueue : Queue = ListQueueImpl

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

