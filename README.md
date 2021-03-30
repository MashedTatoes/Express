# Express

### Toy expression evaluator for me to learn Haskell & basics of a langauge interpreter

## Syntax:

### Arithmetic:

Only +/- supported so far

Example:

```
let tokens = lexer "+ 1 + 1 2"
interpret $ parse tokens
> NumInt 4

```

```
let tokens = lexer "+ 1 5 - + 4 5"
interpret $ parse tokens
> NumInt (-3)

```
