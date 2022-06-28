(* let x = 3110 *)
(* let y = ref 3110; *)
(* let z = !y *)
(* let res = x + !y *)
(* y := 2110 *)

let print_and add x y = print_int (x + y); print_newline(); x + y
