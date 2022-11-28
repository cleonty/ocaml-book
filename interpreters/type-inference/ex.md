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