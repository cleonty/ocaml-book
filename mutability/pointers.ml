type 'a pointer = 'a ref option
let null: 'a pointer = None
let malloc (x : 'a) : 'a pointer = Some (ref x)
let p = malloc 42

exception Segfault

let deref (ptr : 'a pointer) : 'a =
  match ptr with None -> raise Segfault | Some r -> !r

let ( ~* ) = deref;;

let assign (ptr : 'a pointer) (x : 'a) : unit =
  match ptr with None -> raise Segfault | Some r -> r := x

let address (ptr : 'a pointer) : int =
  match ptr with None -> 0 | Some r -> Obj.magic r