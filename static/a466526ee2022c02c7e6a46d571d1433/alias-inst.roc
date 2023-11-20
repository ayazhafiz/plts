# cor +solve -elab

X a : a -> a

sig main0 : X {}
let main0 = \x -> x;;
#   ^^^^^

> cor-out +solve -elab
> 
> X a : a -> a
> 
> sig main0 : X {}
> let main0 = \x -> x;;
> #   ^^^^^ {} -> {}
> 