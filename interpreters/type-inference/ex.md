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
