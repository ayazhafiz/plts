# co_lc

A lambda calculus with stackful coroutines and defunctionalization, compiled to
a bytecode stack machine.

The language is statically typed with complete inference; however, type
annotations cannot be supplied by the programmer.

## Language example

```
let fib = \n ->
  let {} = yield in
  if n < 2
  then n
  else (fib (n - 1)) + (fib (n - 2))

in
let runFib = fib 28 in
let exec = \totals ->
  stat runFib
  | `Pending ->
    let {} = resume runFib in
    exec {0, totals.1 + 1}
  | `Done n -> {n, totals.1}
in exec {0, 0}
```
