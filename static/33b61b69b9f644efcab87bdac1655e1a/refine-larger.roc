# cor +solve -elab
let x : [A, B, C] = A in
#   ^
let z : [A, B, C, D, E] = when x is
#   ^
  | A | B as y -> y
#            ^    ^
#   ^^^^^^^^^^
  | C -> A
#        ^
#   ^
in z

> cor-out +solve -elab
> let x : [A, B, C] = A in
> #   ^ [A, B, C]
> let z : [A, B, C, D, E] = when x is
> #   ^ [A, B, C, D, E]
>   | A | B as y -> y
> #                 ^ [A, B, C, D, E]
> #            ^ [A, B, C, D, E]
> #   ^^^^^^^^^^ [A, B, C]
>   | C -> A
> #        ^ [A, B, C, D, E]
> #   ^ [A, B, C]
> in z
> 