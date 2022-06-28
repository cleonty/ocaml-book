let fact0 = ref (fun x -> x + 0)
let fact n = if n = 0 then 1 else n * !fact0(n - 1)
;; fact0 := fact
let _ = print_int (fact 5)
