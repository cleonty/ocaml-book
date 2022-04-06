let add x y = x + y 
let add' t = fst t + snd t
let add'' (x, y) = x + y

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let uncurried_add = uncurry add
let curried_add = curry add'