type point = {x : int; y : int; mutable c : string}
let p = {x = 0; y = 0; c = "red"}
let _ = p.c <- "white"

type 'a node = {
  value : 'a;
  mutable next : 'a node option;
}

type 'a mlist = {
  mutable first: 'a node option;
}

let create_node v = {
  value = v;
  next = None;
}

let singleton v = {
  first = Some(create_node v)
}

let s = singleton 3110

let insert_first lst v = 
  match lst.first with 
  | None -> 
    lst.first <- Some (create_node v)
  | was_first -> 
    let new_first = create_node v in
      new_first.next <- was_first;
      lst.first <- Some new_first

let _ = insert_first s 55

let empty () = {
  first = None
}