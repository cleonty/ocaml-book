module type X = sig
  val x : int
end

module A : X = struct
  let x = 0
end

module IncX = functor (M : X) -> struct
  let x = M.x + 1
end

let inc x = x + 1

module B = IncX(A)
module C = IncX(B)

(* syntactic sugar *)
module IncX (M : X) = struct
  let x = M.x + 1
end

(* syntactic sugar *)
module AddX (M : X) = struct 
  let add y = M.x + y
end

module Add42 = AddX(struct let x = 42 end)
