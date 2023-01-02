# +solve -elab
# +ir -print
# +eval -print
let f = \x -> x in
#   ^
let g = f true in
#   ^
g

> +solve -elab
> let f = \x -> x in
> #   ^ bool -> bool
> let g = f true in
> #   ^ bool
> g
> 

> +ir -print
> let f = \x -> x in
> let g = f true in
> g

> +eval -print
> true : bool