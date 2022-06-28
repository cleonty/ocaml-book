let counter = ref 0

let next_first = 
  fun () -> 
    counter := !counter + 1;
    !counter

let next_second = 
  counter := !counter + 1;
  !counter

let next_third = 
  incr counter;
   !counter
   
let next_fourth_bad () =
  let counter = ref 0 in
    incr counter;
    !counter

let next_fiveth_bad =
  fun () ->
  let counter = ref 0 in
    incr counter;
    !counter

let next_sixth =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

