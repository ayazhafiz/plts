# +solve -elab
# +ir -print
# +eval -print
let rec fact = \n ->
#       ^^^^
  let ltN = @lt n in
  if ltN 1 then
    1
  else if false then
    n
  else
    # sugar all this
    let mulN = @mul n in
    let subN = @sub n in
    let subNOne = subN 1 in
    let factRest = fact subNOne in
    mulN factRest
in
fact 6

> +solve -elab
> let rec fact = \n ->
> #       ^^^^ int -> int
>   let ltN = @lt n in
>   if ltN 1 then
>     1
>   else if false then
>     n
>   else
>     # sugar all this
>     let mulN = @mul n in
>     let subN = @sub n in
>     let subNOne = subN 1 in
>     let factRest = fact subNOne in
>     mulN factRest
> in
> fact 6
> 

> +ir -print
> let rec fact =
>   \n ->
>     let ltN = @lt n in
>     if ltN 1
>     then 1
>     else
>       if false
>       then n
>       else
>         let mulN = @mul n in
>         let subN = @sub n in
>         let subNOne = subN 1 in
>         let factRest = fact subNOne in
>         mulN factRest
> in
> fact 6

> +eval -print
> 720 : int
