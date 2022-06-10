module type MATH = sig
  (** [fact n] is [n!] *)
end

module Math = struct
  (** [fact_aux n acc] is [n! * acc] *)
  let rec fact_aux n acc =
    if n = 0 then acc else fact_aux (n - 1) (n * acc)
  
  let fact n = fact_aux n 1
end

module MatchCheck : MATH = Math
