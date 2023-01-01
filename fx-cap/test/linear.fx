# +solve -elab
let f = \x -> x in
#   ^
let g = f true in
#   ^
g
