let f x = match x with | (a, b) -> (b, a)

type ('a, 'b) disj = Left of 'a | Right of 'b
let f x = match x with | Left p  -> Right p | Right p -> Left p