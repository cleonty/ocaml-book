module type Set = sig
  
  (** ['a t] is the type of the set whose elements have type ['a]. *)
  type 'a t
  
  (** [empty] is the empty set. *)
  val empty : 'a t
  
  (** [size s] is the number of elements in [s].
      [size empty] is 0. *)
  val size : 'a t -> int
  
  (** [add x s] is a set containing all the elements of [s] 
      as well as element [x]. *)
  val add : 'a -> 'a t -> 'a t
  
  (** [mem x s] is [true] iff [x] is an element of [s]. *)
  val mem : 'a -> 'a t -> bool
  
  (** [union s1 s2] is the set containing all the elements that
      are in either [s1] or [s2]. *)
  val union : 'a t -> 'a t -> 'a t

  (** [inter s1 s2] is the set containing all the elements that
      are in both [s1] and [s2]. *)
  (* val inter : 'a t -> 'a t -> 'a t *)
  
  (** [string s] is a representation of [s] as a string *)
  val string : ('a -> string) -> 'a t -> string 
end

(** [dedup lst] is [lst] but with duplicates removed.
    It also sorts the output list. *)
    let dedup lst =
      lst |> List.sort_uniq Stdlib.compare

let interior string_of_elt h t =
  t
  |> List.map string_of_elt
  |> List.fold_left (fun acc elt -> acc ^ ", " ^ elt) (string_of_elt h)

let string_of_list string_of_elt = function
  | [] -> "{}"
  | h :: t -> "{" ^ interior string_of_elt h t ^ "}"

module ListSetNoDups : Set = struct
  (** The list [a1; ...; an] represents the set {a1, ..., an}.
      The empty list [[]] represents the empty set.
      The list must not contain duplicates. *)
  type 'a t = 'a list
  
  let rep_ok lst = lst

  let rep_ok_expensive lst =
    let u = List.sort_uniq Stdlib.compare lst in
    match List.compare_lengths lst u with 0 -> lst | _ -> failwith "RI"
  
  let empty = 
    rep_ok []

  let size lst = 
    List.length (rep_ok lst)
  
  let mem x lst = 
    List.mem x (rep_ok lst)

  let add x s = 
    let s = rep_ok s in
    if mem x s then s else x :: s

  let union s1 s2 =
    (rep_ok s1) @ (rep_ok s2) |> dedup |> rep_ok
  
  let string f s = 
    string_of_list f (rep_ok s)

end

module ListSetDups : Set = struct
  (** AF: The list [a1; ...; an] represents the set {b1, ..., bm}.
      where [b1; ...; bm] is the same as [b1; ...; bm]
      but with duplicates removed *)
  type 'a t = 'a list
  
  let empty = []
  
  let mem = List.mem

  let rec size_bad = function 
    | [] -> 0
    | h :: t -> (if mem h t then 0 else 1) + size_bad t 

  let rec size s = 
    s |> dedup |> List.length
  

  let add = List.cons

  let union = List.append
  
  let string f s =
    string_of_list f (dedup s)
end

(* op_abs(AF(c)) = AF(op_concrete(c)) *)