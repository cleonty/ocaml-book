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
--> (let)
    (let g = f 3 in (g 1) + (f 2 3)){(fun x -> fun y -> x + y) / f}
=   (let g = (fun x -> fun y -> x + y) 3 in (g 1) + ((fun x -> fun y -> x + y) 2 3))
--> (app)
=   (let g = (fun y -> x + y){3 / x} in (g 1) + ((fun x -> fun y -> x + y) 2 3))
    (let g = fun y -> 3 + y in (g 1) + ((fun x -> fun y -> x + y) 2 3))
--> (let)
    ((g 1) + ((fun x -> fun y -> x + y) 2 3))){fun y -> 3 + y / g}
=   (((fun y -> 3 + y) 1) + ((fun x -> fun y -> x + y) 2 3)))
--> (app)
    (3 + y){1 / y} + (fun x -> fun y -> x + y) 2 3
=   (3 + 1) + (fun x -> fun y -> x + y) 2 3
=   (+)
    4 + (fun x -> fun y -> x + y) 2 3
--> (app)
    4 + ((fun y -> x + y) {2 / x}) 3
=   4 + (fun y -> 2 + y) 3
--> (app)
    4 + (2 + y) {3 / y}
=   4 + (2 + 3)
--> (+)
    4 + 5
--> (+)
    9
```

## Exercise: desugar list [★]

  - `[]` is syntactic sugar for `Left 0`.
  - `e1 :: e2` is syntactic sugar for `Right (e1, e2)`. 

What is the core OCaml expression to which `[1; 2; 3]` desugars?

Answer: `Right(1, Right(2, Right(3, (Left 0)))`


```
let notempty = (fun l -> match l with Left x -> false | Right x -> true) in notempty (Left 0) 
```

## Exercise: generalize patterns [★★★★]

```
p ::= i | (p1, p2) | Left p | Right p | x | _

e ::= ...
    | match e with | p1 -> e1 | p2 -> e2 | ... | pn -> en
```

```
(* This rule should implement evaluation of e. *)
match e with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> match e' with | p1 -> e1 | p2 -> e2 | ... | pn -> en
  if e --> e'

(* This rule implements moving past p1 to the next pattern. *)
match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> match v with | p2 -> e2 | ... | pn -> en
  if there does not exist an s such that v =~ p1 // s

(* This rule implements matching v with p1 then proceeding to evaluate e1. *)
match v with | p1 -> e1 | p2 -> e2 | ... | pn -> en
--> e1 s (* something involving e1 *)
  if v =~ p1 // s
```

```
    match (1 + 2, 3) with
    | (1, 0) -> 4
    | (1, x) -> x
    | (x, y) -> x + y
--> (step +)
    match (3, 3) with
    | (1, 0) -> 4
    | (1, x) -> x
    | (x, y) -> x + y
--> (doesn't match)
    match (3, 3) with
    | (1, x) -> x
    | (x, y) -> x + y    
--> (doesn't match)
    match (3, 3) with
    | (x, y) -> x + y  
--> (match)
    (x + y){3 / x} {3 / y}
  = 3 + 3
--> (step +)
    6
```

Exercise: simple expressions [★]

```
<{}, 110 + 3*1000> ==> 3110
  because <{}, 110> ==> 110
  and     <{}, 3*1000> ==> 3000
    because<{}, 3> ==> 3
    and    <{}, 1000> ==> 3
    and    3*1000 is 3000
  and 110 + 3000 is 3110
```

```
<{}, if 2 + 3 < 4 then 1 + 1 else 2 + 2>
  because <{}, 2 + 3 < 4> ==> false
    because <{}, 2 + 3> ==> 5
      because <{}, 2> ==> 2
      and     <{}, 3> ==> 3
      and     2 + 3 is 5
    and     <{}, 4> ==> 4
    and     5 < 4 is false
  and     <{}, 2 + 2> ==> 4
      because <{}, 2> ==> 2
      and     <{}, 2> ==> 2
      and     2 + 2 is 2
```

## Exercise: lexical scope and shadowing [★★]

```
<[], let x=0 in x + (let x=1 in x)> ==> 1
  because <[], 0> ==> 0
  and     <[x->0], x + (let x = 1 in x)>
    because <[x->0], x> ==> 0
    and     <[x->0], let x = 1 in x>
      because     <[x->0], 1> ==> 1
      and         <[x->1], x> ==> 1
    and 0 + 1 is 1
```

```
<[], let x=1 in let f=fun y -> x in let x=2 in f 0>  ==> 1        (let)
  because <[], 1> ==> 1                                           (const)
  and     <[x->1], let f = fun y -> x in let x=2 in f 0> ==> 1    (let)
    because <[x->1], fun y -> x> ==>  (|fun y -> x , [x->1]|)     (closure)
    and     <[f->(|fun y -> x , [x->1]|), x->1], let x=2 in f 0> ==> 1  (let)
      because <[f->(|fun y -> x , [x->1]|), x->1], 2> ==> 2       (const)
      and     <[f->(|fun y -> x , [x->1]|), x->2], f 0> ==>       
        because <[f->(|fun y -> x , [x->1]|), x->2], f> ==> (|fun y -> x , [x->1]|)
        and     <[f->(|fun y -> x , [x->1]|), x->2], 0> ==> 0
        and     <[y->0,x->1], x> ==> 1
```

## Exercise: dynamic scope [★★★]

```
let x = 5 in
let f y = x + y in
let x = 4 in
f 3
```

Dynamic scope `<[x->4, y->3], x + y> ==> 7`
Lexical scope `<[y->3, 5 + y> ==> 8`

## Exercise: more dynamic scope [★★★]

```
let x = 5 in
let f y = x + y in
let g x = f x in
let x = 4 in
g 3
```
Dynamic scope `<[x->4, y->3], x + y> ==> 7`
Lexical scope `<[x->5, y->3], x + y> ==> 8`

```
let f y = x + y in
let x = 3 in
let y = 4 in
f 2
```

Dynamic scope `<[x->3, y->2], x + y> ==> 5`
Lexical scope `Unbound variable `x``

## Exercise: constraints [★★]

1. fun x -> ( + ) 1 x
```
{} |- fun x -> ( + ) 1 x : 'a -> 'b -| {}
  {x: 'a} |- (+) 1 x : 'c -| {int -> int = 'a, }
    {x: 'a} |- (+) 1 : 'c -| C = {int -> int -> int = int -> 'a} = {int -> int = 'a}
      I |- ( + ) : int -> int -> int -| {}
      I |- 1 : int -| {}
      C = {int -> int -> int = int -> 'a}
    {x: 'a} |- x : 'a -| {}
    {x: 'a} |- 
```
fun b -> if b then false else true
fun x -> fun y -> if x <= y then y else x


## Exercise: unify [★★]

```
X = int
Y = X -> X
```


```
 Our result, which we'll call U, is:
  unify ({ X = int, Y = X -> X }) = {| int/X, (int -> int)/Y |}
   because it's equal to U';{| int/X |},
    where U' is:
     unify ({| int/X |}{ Y = X -> X })
     = unify ({ Y = int -> int })
     = {| (int -> int)/Y |}
      because U' equals U'';{| (int -> int)/Y |},
       where U'' is
        unify ({| (int -> int)/Y |}{})
        = {| |} (the empty substitution).
```

```
X -> Y = Y -> Z
Z = U -> W

{|U -> W / Z|} X -> Y = Y -> (U -> W)
X = Y
Y = U -> W
{|Y/X|, |U -> W / Y| }

{|U -> W / X|, |U -> W / Y|}
```

## Exercise: infer apply [★★★]

```
let apply f x = f x
let apply = fun f -> fun x -> f x

I |- fun f -> fun x -> f x : 'a -> 'b -> 'c -| {'a = 'b -> 'c}
  I, f : 'a |- fun x -> f x : 'b -> 'c -| {'a = 'b -> 'c}
    I, f : 'a, x : 'b |- f x : 'c -| {'a = 'b -> 'c}
      I, f : 'a, x : 'b |- f : 'a -| {}
      I, f : 'a, x : 'b |- x : 'b -| {}
		   
{('b -> 'c) / 'a} 'a -> 'b -> 'c
=substitute
=('b -> 'c) -> 'b -> 'c
rename
=('a -> 'b) -> 'a -> 'b
Inferred Type: ('a -> 'b) -> 'a -> 'b
```

## Exercise: infer double [★★★]

```
let double f x = f (f x)
let double -> fun f -> fun x -> f (f x)

I |- fun f -> fun x -> f (f x) : 'a -> 'b -> 'd -| {'a = 'c -> 'd, 'a = 'b -> 'c}
  I, f : 'a |- fun x -> f (f x) : 'b -> 'd -| {'a = 'c -> 'd, 'a = 'b -> 'c}
  I, f : 'a, x : 'b |- f (f x) : 'd -| {'a = 'c -> 'd, 'a = 'b -> 'c}
    I, f : 'a, x : 'b |- f : 'a -| {}
    I, f : 'a, x : 'b |- f x : 'c -| {'a = 'b -> 'c}
      I, f : 'a, x : 'b |- f : 'a -| {}
      I, f : 'a, x : 'b |- x : 'b -| {}

'a = 'c -> 'd
'a = 'b -> 'c

'c -> 'd = 'b -> 'c

'c = 'b
'd = 'c

'd = 'b

'a -> 'b -> 'd = ('c -> 'd) -> 'b -> 'd = ('d -> 'd) -> 'd -> 'd

Inferred Type: ('a -> 'a) -> 'a -> 'a

```


## Exercise: infer S [★★★★]

Using the HM type inference algorithm, infer the type of the following definition:

```
let s x y z = (x z) (y z)
let s = fun x -> fun y -> fun z -> (x z) (y z)

I |- fun x -> fun y -> fun z -> (x z) (y z) : 'a -> 'b -> 'c -> 'f -| {'d = 'e -> 'f, 'a = 'c -> 'd, 'b = 'c -> 'e}
  I, x : 'a |- fun y -> fun z -> (x z) (y z) : 'b -> 'c -> 'f -| {'d = 'e -> 'f, 'a = 'c -> 'd, 'b = 'c -> 'e}
    I, x : 'a, y : 'b |- fun z -> (x z) (y z) : 'c -> 'f -| {'d = 'e -> 'f, 'a = 'c -> 'd, 'b = 'c -> 'e}
      I, x : 'a, y : 'b, z : 'c |- (x z) (y z) : 'f -| {'d = 'e -> 'f, 'a = 'c -> 'd, 'b = 'c -> 'e}
        I, x : 'a, y : 'b, z : 'c |- (x z) : 'd -| { 'a = 'c -> 'd } 
          I, x : 'a, y : 'b, z : 'c |- x : 'a -| {}
          I, x : 'a, y : 'b, z : 'c |- z : 'c -| {}
        I, x : 'a, y : 'b, z : 'c |- (y z) : 'e-| { 'b = 'c -> 'e } 
          I, x : 'a, y : 'b, z : 'c |- y : 'b -| {}
          I, x : 'a, y : 'b, z : 'c |- z : 'c -| {}

unify
'd = 'e -> 'f
'a = 'c -> 'd
'b = 'c -> 'e

{'e -> 'f/'d}

'a = 'c -> ('e -> 'f)
'b = 'c -> 'e

   'a -> 'b -> 'c -> 'f
 = ('c -> ('e -> 'f)) -> ('c -> 'e) -> 'c -> 'f
 = ('c -> 'e -> 'f) -> ('c -> 'e) -> 'c -> 'f
 = ('c -> 'e -> 'f) -> ('c -> 'e) -> 'c -> 'f
 = rename
 = ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
```