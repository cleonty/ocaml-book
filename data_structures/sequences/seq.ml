let rec ones = 1 :: ones
let rec a = 0 :: b and b = 1 :: a
(** An ['a sequence] is an infinite list of values of type ['a].
    AF: [Cons (x, f)] is the sequence whose head is [x] and tail is [f ()]
    RI: none. *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)
let rec from n = Cons (n, fun () -> from (n + 1));;
let nats = from 0;;

(** [hd s] is the head of [s] *)
let hd (Cons (h, _)) = h

(** [tl s] is the tail of [s] *)
let tl (Cons (_, t)) = t ()

(** [take n s] is the list of the first [n] elements of [s] *)
let rec take n s =
  if n = 0 then [] else hd s :: take (n - 1) (tl s)

(** [drop n s] is all but the first [n] elements of [s] *)
let rec drop n s =
  if n = 0 then s else drop (n - 1) (tl s)
  
(** [square <a; b; c; ...>] is [<a * a; b * b; c * c; ...]. *)
let rec square (Cons (h, t)) =
  Cons (h * h, fun () -> square (t ()))

(** [sum <a1; a2; a3; ...> <b1; b2; b3; ...>] is
    [<a1 + b1; a2 + b2; a3 + b3; ...>] *)
let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1 + h2, fun () -> sum (t1 ()) (t2 ()))

(** [map f <a; b; c; ...>] is [<f a; f b; f c; ...>] *)
let rec map f (Cons (h, t)) =
  Cons (f h, fun () -> map f (t ()))

(** [map2 f <a1; b1; c1;...> <a2; b2; c2; ...>] is
    [<f a1 b1; f a2 b2; f a3 b3; ...>] *)
let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, fun () -> map2 f (t1 ()) (t2 ()))

let square' = map (fun n -> n * n)
let sum' = map2 ( + )

let rec nats = Cons (0, fun () -> map (fun x -> x + 1) nats)

let rec fibs =
  Cons (1, fun () ->
    Cons (1, fun () ->
      sum fibs (tl fibs)))

type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

module SequenceFibs = struct
  type 'a sequence = Cons of 'a * (unit -> 'a sequence)

  let hd : 'a sequence -> 'a =
    fun (Cons (h, _)) -> h

  let tl : 'a sequence -> 'a sequence =
    fun (Cons (_, t)) -> t ()

  let rec take_aux n (Cons (h, t)) lst =
    if n = 0 then lst
    else take_aux (n - 1) (t ()) (h :: lst)

  let take : int -> 'a sequence -> 'a list =
    fun n s -> List.rev (take_aux n s [])

  let nth : int -> 'a sequence -> 'a =
    fun n s -> List.hd (take_aux (n + 1) s [])

  let rec sum : int sequence -> int sequence -> int sequence =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
      Cons (h_a + h_b, fun () -> sum (t_a ()) (t_b ()))

  let rec fibs =
    Cons(1, fun () ->
      Cons(1, fun () ->
        sum (tl fibs) fibs))

  let nth_fib n =
    nth n fibs

end

module LazyFibs = struct

  type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

  let hd : 'a lazysequence -> 'a =
    fun (Cons (h, _)) -> h

  let tl : 'a lazysequence -> 'a lazysequence =
    fun (Cons (_, t)) -> Lazy.force t

  let rec take_aux n (Cons (h, t)) lst =
    if n = 0 then lst else
      take_aux (n - 1) (Lazy.force t) (h :: lst)

  let take : int -> 'a lazysequence -> 'a list =
    fun n s -> List.rev (take_aux n s [])

  let nth : int -> 'a lazysequence -> 'a =
    fun n s -> List.hd (take_aux (n + 1) s [])

  let rec sum : int lazysequence -> int lazysequence -> int lazysequence =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
      Cons (h_a + h_b, lazy (sum (Lazy.force t_a) (Lazy.force t_b)))

  let rec fibs =
    Cons(1, lazy (
      Cons(1, lazy (
        sum (tl fibs) fibs))))

  let nth_fib n =
    nth n fibs
end

let x = LazyFibs.nth_fib 30
let y = SequenceFibs.nth_fib 30 (* slow *)
