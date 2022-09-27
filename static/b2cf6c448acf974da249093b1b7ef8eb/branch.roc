# cor +solve -elab
let y = \x -> when x is
#   ^
  | A -> F
  | B -> G
  | C -> H

in let z = y A
#      ^

in let y = \x -> when x is
#      ^
  | A -> F
  | B -> G
  | _ -> H

in let z = y A
#      ^
in let z = y D
#      ^

in let y = \x -> when x is
#      ^
  | A -> F
  | B -> G
  | x -> x

in let z = y A
#      ^
in let z = y D
#      ^

in Eof

> cor-out +solve -elab
> let y = \x -> when x is
> #   ^ - [A, B, C] -> [F, G, H]*
> #   ^ + [A, B, C] -> [F, G, H]
>   | A -> F
>   | B -> G
>   | C -> H
> 
> in let z = y A
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> 
> in let y = \x -> when x is
> #      ^ - [A, B]* -> [F, G, H]*
> #      ^ + [A, B]* -> [F, G, H]
>   | A -> F
>   | B -> G
>   | _ -> H
> 
> in let z = y A
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> in let z = y D
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> 
> in let y = \x -> when x is
> #      ^ - [A, B, F, G]a -> [A, B, F, G]a
> #      ^ + [A, B, F, G]a -> [A, B, F, G]a
>   | A -> F
>   | B -> G
>   | x -> x
> 
> in let z = y A
> #      ^ - [A, B, F, G]*
> #      ^ + [A, B, F, G]
> in let z = y D
> #      ^ - [A, B, F, G, D]*
> #      ^ + [A, B, F, G, D]
> 
> in Eof
> 