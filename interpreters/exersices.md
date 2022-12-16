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
