# CONSTANTS

```
Rule: env |- i : int -| {}
Rule: env |- b : bool -| {}
```

## Examples

```
{} |- 3110 : int -| {}
{} |- true : bool -| {}
```

# NAMES

```
Rule: env |- n : env(n) -| {}
```

## Examples

```
{x : int} |- x : int -| {}
{} |/- x
{( + ) : int -> int -> int } |- ( + ) : int -> int -> int -| {}
```

# IF THEN ELSE

```
Rule:
env |- if e1 then e2 else e3 : 't -| C1, C2, C3, C
  if fresh 't
  and env |- e1 : t1 -| C1
  and env |- e2 : t2 -| C2
  and env |- e3 : t3 -| C3
  and C = {t1 = bool, 't = t2, 't = t3}
```

## Example

```
{} |- if true then 0 else 1: 'a 
  {} |- true : bool -| {}
  {} |- 0 : int -| {}
  {} |- 1 : int -| {}
  C = {bool = bool, 'a = int, 'a = int}
```

# Anonymous functions

```
Rule:
env |- fun x -> e : 't1 -> t2 -| C
  if fresh 't1
  and env, x : 't1 |- e : t2 -| C
```

## Example

```
{} |- fun x -> if x then 1 else 0 : 'a -> 'b -| {'a = bool, 'b = int}
  {x: 'a} |- if x then 1 else 0 : 'b -| C where C is {'a = bool, 'b = int}
    {x : 'a} |- x : 'a -| {}
    {x : 'a} |- 1 : int -| {}
    {x : 'a} |- 0 : int -| {}
    C = {'a = bool, 'b = int, 'b = int}
```

# Function application

```
Rule:
env |- e1 e2 : 't -| C1, C2, C
  if fresh 't
  and env |- e1 : t1 -| C1
  and env |- e2 : t2 -| C2
  and C = {t1 = t2 -> 't}
```

## Example

`I` is an env that binds `+`, `*`, etc.

```
I |- (+) 1 : 'a -| C = {int -> int -> int = int -> 'a} = {int -> int = 'a}
  I |- ( + ) : int -> int -> int -| {}
  I |- 1 : int -| {}
  C = {int -> int -> int = int -> 'a}
  
```

# Solving algebraic equations

```
5x + 2y =  9 
 x -  y = -1

 x = y - 1
5(y-1) + 2y = 9
5y - 5 + 2y = 9
7y = 14
y = 2
x = 2 - 1 = 1

5(1) + 2(2) =  9
1 - 2 = -1

9 = 9
-1 = -1
```

- Eliminate a variable
- Find value of other variable
- Use that to find value of first variable
- Solution is a *substitution* that *unifies* set of equations

# Solving constraints

## Develop intuition

### 1st attempt
```
'x -> ('x -> int) = int -> 'y
'x -> 'x = 'y

{'x -> 'x / 'y}
'x -> ('x -> int) = int -> ('x -> 'x)

'x = int
'x -> int = 'x -> 'x

{int / 'x}

int -> int = int -> int

int = int
int = int

{'x -> 'x / 'y}; {int / 'x}
```

-----

### 2nd attempt
```
'x -> ('x -> int) = int -> 'y
'x -> 'x = 'y

'x -> ('x -> int) = int -> 'y
'x = int
'x -> int = 'y

{int / 'x}

int -> int = 'y

{int -> int / y}

{int / 'x}; {int -> int / y}
```

# Type Inference Example

```
I |- fun f -> fun x -> f (( + ) x 1) : 'a -> 'b -> 'e -| 'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c
  I, f : 'a |- fun x -> f (( + ) x 1) : 'b -> 'e -| 'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c
    I, f : 'a, x : 'b |- f (( + ) x 1) : 'e -| 'a = 'd -> 'e, 'c = int -> 'd, int -> int -> int = 'b -> 'c
      I, f : 'a, x : 'b |- f : 'a -| {}
      I, f : 'a, x : 'b |- (( + ) x) 1 : 'd -| 'c = int -> 'd, int -> int -> int = 'b -> 'c
        I, f : 'a, x : 'b |- ( + ) x : 'c -| int -> int -> int = 'b -> 'c
          I, f : 'a, x : 'b |- ( + ) : int -> int -> int -| {}
          I, f : 'a, x : 'b |- x : 'b -| {}
        I, f : 'a, x : 'b |- 1 : int -| {}

---
'a = 'd -> 'e
'c = int -> 'd
int -> int -> int = 'b -> 'c

{ 'd -> 'e / 'a}
---
'c = int -> 'd
int -> int -> int = 'b -> 'c

{ 'd -> 'e / 'a}
---
int -> int -> int = 'b -> 'c

{ 'd -> 'e / 'a}, { int -> 'd / 'c}
---
int -> int -> int = 'b -> int -> 'd

{ 'd -> 'e / 'a}, { int -> 'd / 'c}
---
int = 'b
int -> int = int -> 'd

{ 'd -> 'e / 'a}, { int -> 'd / 'c}
---
int -> int = int -> 'd

{ 'd -> 'e / 'a}, { int -> 'd / 'c}, { int / 'b}
---
int = int
int = 'd

{ 'd -> 'e / 'a}, { int -> 'd / 'c}, { int / 'b}
---
int = 'd

{ 'd -> 'e / 'a}, { int -> 'd / 'c}, { int / 'b}, { int / 'd}
----
----
'a -> 'b -> 'e
('d -> 'e) -> 'b -> 'e
('d -> 'e) -> int -> 'e
(int -> 'e) -> int -> 'e
```

# Naive inference for let

```
Rule:
env |- let x = e1 in e2 : t2 -| C1, C2
  if env |- e1 : t1 -| C1
  and env, x : t1 |- e2 : t2 -| C2
```

## Example

```
{} |- let x = 42 in  : int -| {}
  {} |- 42 : int -| {}
  {x : int} |- x : int -| {}
```

## Polymorphism is tricky

```
let id = fun x -> x in
let a = id 0 in
id true
``` 

Naive rule doesn't work

- Puts `id : 'a -> 'a` in environment
- Later when `id` is applied
- `id 0` generates `'a -> 'a = int -> 'b`
- `id true` generates `'a -> 'a = bool -> 'c`
- so `'a = int = bool` needs to hold but cannot!

# Type schemes

- Inspired by universal quantification in logic
  - for all `x`, it holds that `0*x=0`
- A type scheme is written `'a.t`
  - `'a` is a type variable that is in scope in t
  - `t` is a type
- Extended syntax: `'a1 ... 'an.t`
  - Like in logic: for all `x`, `y`, `z`, it holds that `x(y + z) = xy + xz`

# Generalization and instantiation 

```
let id = fun x -> x in ...
```

- **Generalize** type to 'a . 'a -> 'a in environment
- At each application, **instantiate** type with new type variables
  - at `id 0`, instantiate to `'b -> 'b`
  - at `id true`, instantiate to `'c -> 'c`
  - Now each use is independent of the others

```
env |- let x = e1 in e2 : t2 -| C1, C2
  if env |- e1 : t1 -| C1
  and generalize (C1, env, x : t1)
    |- e2 : t2 -| C2

env |- : instantiate(env(n)) -| {}
```

## Instantiation

- Doesn't change a type
- Changes a type scheme to a type
  - Get rid of the `'a1 ... 'an` before the dot
  - Substitute a fresh type variable for each of them
- E.g. `'a -> 'a -> 'a` becomes `'b -> 'b` for a fresh `'b`

## Generalization

`generalize(C1, env, x : t1)`
- Fully finish inference of binding expression:
  - use `unify` to solve C1
  - Apply resulting substitution to `env` and `t1`, yielding `env1` and `u1` 
- Generalize `u1` to to a type scheme `s1`
- Do generalize variables in `u1`
- Do not generalize variables also in `env`: they are constrained by outside code
- Return `env1`, `x : s1`