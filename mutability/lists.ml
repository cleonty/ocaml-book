(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a] and a link to the [next] node. *)
    type 'a node = { next : 'a mlist; value : 'a }

(** An ['a mlist] is a mutable singly-linked list with elements of type ['a].
    The [option] represents the possibility that the list is empty.
    RI: The list does not contain any cycles. *)
    and 'a mlist = 'a node option ref

(** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = ref None

(** [insert_first lst n] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
    let insert_first (lst : 'a mlist) (v : 'a) : unit =
    lst := Some { next = ref !lst; value = v }
(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
    let rec to_list (lst : 'a mlist) : 'a list =
      match !lst with None -> [] | Some { next; value } -> value :: to_list next

let lst0 = empty ();;
let lst1 = lst0;;
insert_first lst0 1;;
to_list lst1;;