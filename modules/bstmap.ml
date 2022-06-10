module type Map = sig
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to
      values of type ['v]. *)
  type ('k, 'v) t

  (** [empty] does not bind any keys. *)
  val empty  : ('k, 'v) t

  (** [insert k v m] is the map that binds [k] to [v], and also contains
      all the bindings of [m].  If [k] was already bound in [m], that old
      binding is superseded by the binding to [v] in the returned map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)
  val lookup : 'k -> ('k, 'v) t -> 'v

  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
  (* val bindings : ('k, 'v) t -> ('k * 'v) list *)
end

module BstMap : Map = struct
  type ('k, 'v) tree = 
  | Leaf
  | Node of ('k, 'v) node
  and ('k, 'v) node = {
    key: 'k;
    value: 'v;
    left: ('k, 'v) tree;
    right: ('k, 'v) tree;
  }
  type ('k, 'v) t = ('k, 'v) tree
  let empty = Leaf
  let rec insert k v = function
    | Leaf -> Node{key = k; value = v; left = Leaf; right = Leaf}
    | Node {key; value; left; right} -> if (Stdlib.compare k key) < 0 then Node{key; value; left = insert k v left; right} else Node{key; value; left; right = insert k v right}
  let rec lookup k = function
    | Leaf -> failwith "not found"
    | Node {key; value; left; right} -> if (Stdlib.compare k key) = 0 then value else if (Stdlib.compare k key) < 0 then (lookup k left) else (lookup k right)
  (* let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare) *)
  (* let bindings m = m |> keys |> List.map (fun k -> (k, (lookup k m))) *)
end