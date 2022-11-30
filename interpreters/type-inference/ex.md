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