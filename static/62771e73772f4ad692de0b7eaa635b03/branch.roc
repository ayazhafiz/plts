# cor +solve -elab
let y1 = \x -> when x is
#   ^^
  | A -> F
  | B -> G
  | C -> H

in let z = y1 A
#      ^

in let y2 = \x -> when x is
#      ^^
  | A -> F
  | B -> G
  | _ -> H

in let z = y2 A
#      ^
in let z = y2 D
#      ^

in let y3 = \x -> when x is
#      ^^
  | A -> F
  | B -> G
  | x -> x

in let z = y3 A
#      ^
in let z = y3 D
#      ^

in let eater = \y1 -> \y2 -> \y3 ->
#      ^^^^^
  when y1 A is | _ ->
    when y2 C is | _ ->
      when y3 E is | _ -> Z

in let z = eater y1 y2 y3
#      ^   ^^^^^

in Eof

> cor-out +solve -elab
> let y1 = \x -> when x is
> #   ^^ - [A, B, C] -> [F, G, H]*
> #   ^^ + [A, B, C] -> [F, G, H]
>   | A -> F
>   | B -> G
>   | C -> H
> 
> in let z = y1 A
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> 
> in let y2 = \x -> when x is
> #      ^^ - [A, B]* -> [F, G, H]*
> #      ^^ + [A, B]* -> [F, G, H]
>   | A -> F
>   | B -> G
>   | _ -> H
> 
> in let z = y2 A
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> in let z = y2 D
> #      ^ - [F, G, H]*
> #      ^ + [F, G, H]
> 
> in let y3 = \x -> when x is
> #      ^^ . [A, B, F, G]a -> [A, B, F, G]a
>   | A -> F
>   | B -> G
>   | x -> x
> 
> in let z = y3 A
> #      ^ - [A, B, F, G]*
> #      ^ + [A, B, F, G]
> in let z = y3 D
> #      ^ - [A, B, F, G, D]*
> #      ^ + [A, B, F, G, D]
> 
> in let eater = \y1 -> \y2 -> \y3 ->
> #      ^^^^^ - ([A]* -> *) -> ([C]* -> *) -> ([E]* -> *) -> [Z]*
> #      ^^^^^ + ([A]* -> *) -> ([C]* -> *) -> ([E]* -> *) -> [Z]
>   when y1 A is | _ ->
>     when y2 C is | _ ->
>       when y3 E is | _ -> Z
> 
> in let z = eater y1 y2 y3
> #          ^^^^^ - ([A, B, C] -> [F, G, H]*)
> #          ^^^^^ -   -> ([A, B, C]* -> [F, G, H]*)
> #          ^^^^^ -        -> ([A, B, E, F, G]a -> [A, B, F, G, E]a) -> [Z]*
> #          ^^^^^ + ([A, B, C] -> [F, G, H])
> #          ^^^^^ +   -> ([A, B, C]* -> [F, G, H]) -> ([A, B, E, F, G]a -> [A, B, F, G, E]a) -> [Z]
> #      ^ - [Z]*
> #      ^ + [Z]
> 
> in Eof
> 