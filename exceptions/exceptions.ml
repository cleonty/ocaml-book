(*exception OhNo of string
#raise (OhNo "oops!")*)

let safe_div x y =
  try x / y with
  | Division_by_zero -> 0

let _ = match List.hd [] with
  | [] -> "empty"
  | _ :: _ -> "nonempty"
  | exception (Failure s) -> s