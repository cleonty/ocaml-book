module type TableMap = sig
  (** [('k, 'v t)] is the type of mutable table-based maps that
      bind keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t
  
  (** [insert k v m] mutates map [m] to bind [k] to [v]. If
      [k] was already bound in [m], that binding is replaced
      by the binding to [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  
  (** [find k m] is [Some v] if [m] binds [k] to [v], and
    [None] if [m] doesn't bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option
  
  (** [remove k m] mutates map [m] to remove any binding of [k].
      if [k] was not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit
  
  (** [create hash c] creates a new table map with capacity [c]
      that will use [hash] as a function to convert keys to
      integers.
      Requires: [hash] distributes keys uniformly over integers,
      and the output of [hash] is always non-negative. *)
  val create : ('k -> int) -> int -> ('k, 'v) t
end

module HashMap : TableMap = struct
  (** 
    AF:  If [buckets] is
      [| [(k11,v11); (k12,v12); ...];
         [(k21,v21); (k22,v22); ...];
         ... |]
    that represents the map
      {k11:v11, k12:v12, ...,
       k21:v21, k22:v22, ...,  ...}.
    RI: No key appear more than once in array (so, no
    duplicate keys in association lists). All keys are
    in the right buckets: if [k] is in [buckets] at index
    [b] then [hash(k) = b]. The output of [hash] must always
    be non-negative. [hash] must run in constant time *)
   
  type ('k, 'v) t = {
    hash: 'k -> int;
    mutable size: int;
    mutable buckets: ('k * 'v) list array;
  }
  
  (** [capacity tab] is the number of buckets in [tab] .
      Efficiency O(1) *)
  let capacity tab =
    Array.length tab.buckets
  
  (** [index k tab] is the index at which key [k] should 
      be stored in the buckets of [tab], regardless
      of what happens to the load factor.
      Efficiency O(1) *)
  let index k tab = 
    tab.hash k mod capacity tab

  (** [insert_no_resize k v tab] inserts a binding from [k] to
      [v] in [tab] and doesn't resize the table.
      Efficiency: expected O(L) *)
  let insert_no_resize k v tab = 
    let b = index k tab in (* O(1)*)
    let old_bucket = tab.buckets.(b) in
    let trimmed_bucked = List.remove_assoc k old_bucket in (* expected O(L) *)
    tab.buckets.(b) <- (k, v) :: trimmed_bucked;
    if not (List.mem_assoc k old_bucket) then (* expected O(L) *)
      tab.size <- tab.size + 1;
    ()
  
  (** [load_factor tab] is the load factor of [tab], i.e
      the number of bindings divided by the number of buckets. *)
  let load_factor tab =
    float_of_int tab.size /. float_of_int (capacity tab)

  (** [rehash tab new_capacity] replaces the buckets array of [tab]
      with a new array of size [new_capacity], and re-inserts
      all the bindings of [tab] into the new array. The 
      keys are rehashed, so the bindings are lickely to land
    in new buckets. 
    Efficiency: expected O(n), where n is the number of bindings *)
  let rehash tab new_capacity =
    (* insert (k, v) into tab *)
    let rehash_binding (k, v) =
      insert_no_resize k v tab
    in
    (* insert all bindings of bucket into tab *)
    let rehash_bucket bucket =
      List.iter rehash_binding bucket
    in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity []; (* O(n) *)
    tab.size <- 0;
    (* [rehash_binding] is called by [rehash_bucket] once for every binding *)
    Array.iter rehash_bucket old_buckets (* expected O(n) *)
    
    
  (** [resize_if_needed tab] resizes and rehashes [tab] if the load
      factor is too big or too small. Load factors are allowed
      to range from 1/2 to 2 *)
  let resize_if_needed tab = 
    let lf  = load_factor tab in 
    if lf > 2.0 then
      rehash tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash tab (capacity tab / 2)
    else ()

  let insert k v tab = 
    insert_no_resize k v tab;
    resize_if_needed tab
  
  let find k tab = 
    List.assoc_opt k tab.buckets.(index k tab)

  (** [remove_no_resize k tab] removes [k] from [tab]
      and does not trigger a resize, regardless of what
      happens to the load factor.
      Efficiency: expected O(L) *)
  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old_bucket then
      tab.size <- tab.size - 1;
    ()
  
  (* Efficiency: expected O(n) *)
  let remove k tab = 
    remove_no_resize k tab;
    resize_if_needed tab
  
  (** Efficiency: O(c) *)
  let create h c = {
    hash = h;
    size = c;
    buckets = Array.make c []
  }
    
end

open Hashtbl
let t = create 16
for i = 1 to 16 do
  add t i (string_of_int i)
done

stats t;