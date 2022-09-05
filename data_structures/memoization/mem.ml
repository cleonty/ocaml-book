let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fibm n = 
  let memo : int option array = Array.make (n + 1) None in
  let rec f_mem n = 
    match memo.(n) with
    | Some result -> result
    | None -> 
      let result =
        if n < 2 then 1 else f_mem (n - 1) + f_mem (n - 2)
      in
      memo.(n) <- Some result;
      result
  in
  f_mem n
  
let memo f =
  let h = Hashtbl.create 11 in
  fun x ->
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y

let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    try Hashtbl.find h x
    with Not_found ->
      let y = f g x in
      Hashtbl.add h x y;
      y
  in
  g

let fib_memo n =
  let rec fib self n =
    if n < 2 then 1 else self (n - 1) + self (n - 2)
  in
  memo_rec fib