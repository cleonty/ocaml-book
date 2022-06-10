module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end

module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end