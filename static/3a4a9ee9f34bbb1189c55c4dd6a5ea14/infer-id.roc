# cor +solve -elab
let id = \x -> x;;
#   ^^

> cor-out +solve -elab
> let id = \x -> x;;
> #   ^^ 'a -> 'a
> 