## Exercise: propositions as types [★★]

For each of the following propositions, write its corresponding type in OCaml.

`true -> p` is `unit -> 'p` 

`p /\ (q /\ r)` is `('p, ('q, 'r))`

`(p \/ q) \/ r` is `Left (Left of 'a | Right of 'b) | Right 'r`

`false -> p` is `empty -> 'p`

## Exercise: programs as proofs [★★★]

For each of the following propositions, determine its corresponding type in OCaml, then write an OCaml let definition to give a program of that type. Your program proves that the type is inhabited, which means there is a value of that type. It also proves the proposition holds.

`p /\ q -> q /\ p`

```
let f x = match x with | (a, b) -> (b, a)
```



`p \/ q -> q \/ p`

```
type ('a, 'b) disj = Left of 'a | Right of 'b
let f x = match x with | Left p  -> Right p | Right p -> Left p
```

## Exercise: evaluation as simplification [★★★]

Consider the following OCaml program:

`let f x = snd ((fun x -> x, x) (fst x))`

1. What is the type of that program?
`'a * 'b -> 'a`

2. What is the proposition corresponding to that type?
`A /\ B => A`

3. How would f (1,2) evaluate in the small-step semantics?

  f (1, 2)
= snd ((fun x -> x, x) (fst (1, 2)))
= snd ((fun x -> x, x) 1))
= snd (1 1))
= 1

4. What simplified implementation of f does that evaluation suggest? (or perhaps there are several, though one is probably the simplest?)

`fst`

5. Does your simplified f still have the same type as the original? (It should.)

Yes.

Your simplified f and the original f are both proofs of the same proposition, but evaluation has helped you to produce a simpler proof.