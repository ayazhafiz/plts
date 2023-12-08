# cor +solve -elab
sig after : ({} -> ([A] -> [B])) -> ([A] -> [B])
let after = \cont ->
  let f = \a -> cont {} a in
  f
;;

let nestForever =
  let nester = \x -> after nester in
#     ^^^^^^
  nester
;;

> cor-out +solve -elab
> sig after : ({} -> ([A] -> [B])) -> ([A] -> [B])
> let after = \cont ->
>   let f = \a -> cont {} a in
>   f
> ;;
> 
> let nestForever =
>   let nester = \x -> after nester in
> #     ^^^^^^ {}
> #     ^^^^^^   -[nester1]-> [A]
> #     ^^^^^^                  -[f1
> #     ^^^^^^                      <..{}
> #     ^^^^^^                           -[nester1]-> [A]
> #     ^^^^^^                                         -[f1 ..]-> 
> #     ^^^^^^                                         [
> #     ^^^^^^                                         B
> #     ^^^^^^                                         ]>]-> 
> #     ^^^^^^                  [B]
>   nester
> ;;
> 