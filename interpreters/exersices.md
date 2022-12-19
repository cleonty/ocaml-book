# Exercises

## Exercise: step expressions [★]

```
  (3 + 5) * 2 (2 steps)
--> (step + operation)
  8 * 2
--> (step * operation)
  16

  if 2 + 3 <= 4 then 1 + 1 else 2 + 2 (4 steps)
--> (step + operation)
  if 5 <= 4 then 1 + 1 else 2 + 2 (4 steps)
--> (step <= operation)
  if false then 1 + 1 else 2 + 2
--> (step else branch)
  2 + 2
--> (step + operation)
```

## Exercise: step let expressions [★★]

```
  let x = 2 + 2 in x + x (3 steps)
--> (step + operation)
  let x = 4 in x + x
--> (step let expression)
  (x + x){4/x} = 4 + 4
--> (step + operation)
  8

  let x = 5 in ((let x = 6 in x) + x) (3 steps)
--> (step let expression)
  ((let x = 6 in x) + x){5/x}
 =(let x = 6 in x){5/x} + x{5/x}
 =(let x = 6 in x{5/x}) + 5
 =(let x = 6 in x) + 5  
--> (step let expression)
  x{6/x} + 5
--> (step + operator)
  11

  let x = 1 in (let x = x + x in x + x) (4 steps)
--> (step let expression)
  (let x = x + x in x + x){1/x}
 =let x = (x + x){1/x} in x + x
 =let x = 1 + 1 in x + x
--> (step + operation)
  let x = 2 in x + x
--> (step let expression)
  (x + x){2/x}
 =2 + 2
--> (step + operation)
  4
```

## Exercise: variants [★]

```
  Left (1+2) (1 step)
--> (step + operation)
  Left 3

  match Left 42 with Left x -> x+1 | Right y -> y-1 (2 steps)
--> (match: Left)
  (x + 1){42/x}
  42 + 1
--> (step + expression)
  43
```

## Exercise: application [★★]

```
  (fun x -> 3 + x) 2 (2 steps)
--> (app)
  (3 + x){2/x}
 =(3 + 2)
--> (step + expression)
  5

  let f = (fun x -> x + x) in (f 3) + (f 3) (6 steps)
--> (step let expression)
  ((f 3) + (f 3)){(fun x -> x + x) / f}
= ((f 3){(fun x -> x + x), f} + (f 3){(fun x -> x + x) / f})
= ((f{(fun x -> x + x) 3{(fun x -> x + x) 3)})} + (f{(fun x -> x + x) / f} 3{(fun x -> x + x) 3)}))
= (fun x -> x + x) 3 + (fun x -> x + x) 3
--> (app)
  (x + x) {3 / x} + (fun x -> x + x) 3
= (3 + 3) + (fun x -> x + x) 3
--> (step + expression)
= 6 + (fun x -> x + x) 3
-->(app)
   6 + (x + x){3 / x}
=  6 + (3 + 3)
--> (step + expression)
   (6 + 6)
--> (step + expression)
  12

  let f = fun x -> x + x in let x = 1 in let g = fun y -> x + f y in g 3 (7 steps)
--> (let)
    let x = 1 in let g = fun y -> x + f y in g 3 {fun x -> x + x / f}
=   let x = 1 in let g = fun y -> x + (fun x -> x + x) y in g 3
--> (let)
    (let g = fun y -> x + (fun x -> x + x) y in g 3){x / 1}
=   (let g = fun y -> 1 + (fun x -> x + x) y in g 3)
--> (let)
    (g 3){fun y -> 1 + (fun x -> x + x) y / g}
=   (fun y -> 1 + (fun x -> x + x) y) 3
--> (app)
    (1 + (fun x -> x) y){3 / y}
=   1 + (fun x -> x) 3
--> (app)
    1 + (x + x){3 / x}
=   1 + (3 + 3)
--> (step + operation)
    1 + 6
--> (step + operation)
    7

  let f = (fun x -> fun y -> x + y) in let g = f 3 in (g 1) + (f 2 3) (9 steps)
```


