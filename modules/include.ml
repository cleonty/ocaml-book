module type Ring = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t  (* additive inverse *)
  val to_string : t -> string
end

module IntRingRep = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

module FloatRingRep = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

module IntRing : Ring = IntRingRep
module FloatRing : Ring = FloatRingRep

module type Field = sig
  include Ring
  val ( / ) : t -> t -> t
end

module IntFieldRep = struct
  include IntRingRep
  let ( / ) = Stdlib.( / )
end

module IntField : Field = IntFieldRep

module FloatFieldRep = struct
  include FloatRingRep
  let ( / ) = Stdlib.( /. )
end

module FloatField : Field = FloatFieldRep
