# cor +solve -elab
let x : [A, B, C] = A in
#   ^
let z = when x is
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
> let z = when x is
> #   ^ [A, B]
>   | A | B as y -> y
> #                 ^ [A, B]
> #            ^ [A, B]
> #   ^^^^^^^^^^ [A, B, C]
>   | C -> A
> #        ^ [A, B]
> #   ^ [A, B, C]
> in z
> 