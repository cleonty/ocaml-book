let _ = print_endline "Hello world!"
let _ = print_endline ("abc" ^ "def")

let x = 3110
let avg x y = (x +. y) /. 2.

let f x y = x - y
let z = f 3 2

(** requires: [n >=0 ] *)
let rec fact n =
  if n = 0 then 1
  else n * fact (n - 1)

let _ = print_endline (string_of_int(fact(45)))

(** [pow x y] is [x] to the power of [y].
    Requires: [y >= 0] *)
let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)

let _ = print_endline (string_of_int(pow 3 12))

let id x = x
let id_int: int -> int = id

let f ~name1: arg1 ~name2: arg2 = arg1 + arg2
let f ~name1 ~name2 = name1 + name2
let f ~name1: (arg1: int) ~name2: (arg2: int) = arg1 + arg2
let f ?name:(arg1=8) arg2 = arg1 + arg2
let _ = f ~name:2 7
let _ = f 7

let add x y = x + y
let _ = (add 2) 3
let ( ^^ ) x y = max x y

(** [count n] is [n], computed by adding 1 to itself [n] times. That is,
    this function counts up from 1 to [n]. *)
let rec count n = 
  if n = 0 then 0 else 1 + count (n - 1)

let rec count_aux n acc =
  if n = 0 then acc else count_aux (n - 1) (acc + 1)

let count_tr n = count_aux n 0

(** [fact n] is [n] factorial *)
let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
  
let rec fact_aux n acc =
  if n = 0 then acc else fact_aux ( n - 1) (acc * n)

let fact_tr n = fact_aux n 1

(**
#require "zarith.top"

let rec zfact_aux n acc =
  if Z.equal n Z.zero then acc else zfact_aux ( Z.pred 1) (Z.mull acc n)

let zfact_tr n = zfact_aux n Z.one
*)


(** [lowercase_ascii c] is the lowercase ASCII equivalent of
    character [c]. *)

(** [index s c] is the index of the first occurrence of
    character [c] in string [s].  Raises: [Not_found]
    if [c] does not occur in [s]. *)

(** [random_int bound] is a random integer between 0 (inclusive)
    and [bound] (exclusive).  Requires: [bound] is greater than 0
    and less than 2^30. *)
let _ = print_endline "Camels are bae"

let print_stat name num = 
  Printf.printf "%s: %F\n%!" name num

let _ = print_stat "Leonty" 40.;

#trace fact_tr
#trace fact_aux
let x = fact_tr 10;
#untrace fact_tr
#untrace fact_aux

(* possibility 1 *)
let random_int bound =
  assert (bound > 0 && bound < 1 lsl 30);
  (* proceed with the implementation of the function *)

(* possibility 2 *)
let random_int bound =
  if not (bound > 0 && bound < 1 lsl 30)
  then invalid_arg "bound";
  (* proceed with the implementation of the function *)

(* possibility 3 *)
let random_int bound =
  if not (bound > 0 && bound < 1 lsl 30)
  then failwith "bound"