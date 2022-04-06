let double x = 2 * x
let square x = x * x
let quad x = 4 * x
let quad' x = double (double x)
let quad'' x = x |> double |> double
let fourth x = x * x * x * x
let fourth' x = x |> square |> square

let twice f x = f (f x)

let quad''' x = twice double x
let fourth''' x = twice square x

let quad'''' = twice double
let fourth'''' = twice square

let apply f x = f x
let pipeline x f = f x
let (|>) = pipeline
let x = 5 |> double

let compose f g x = f (g x)
let sqaure_then_double = compose double square
let x = sqaure_then_double 1
let y = sqaure_then_double 2

let both f g x = (f x, g x)
let ds = both double square
let p = ds 3

let cond p f g x = 
  if p x then f x else g x