# cor +solve -elab
# cor +mono -print
# cor +eval -print
proto thunkDefault a : () -> () -> a
#     ^^^^^^^^^^^^

let thunkDefault = \() -> \() -> T1
#   ^^^^^^^^^^^^
let thunkDefault = \() -> \() -> T2
#   ^^^^^^^^^^^^

entry test1 =
  let useT1 = \T1 -> () in
  useT1 (thunkDefault () ())
  #      ^^^^^^^^^^^^

entry test2 =
  let useT2 = \T2 -> () in
  useT2 (thunkDefault () ())
  #      ^^^^^^^^^^^^

> cor-out +solve -elab
> proto thunkDefault a : () -> () -> a
> #     ^^^^^^^^^^^^ () -[[] + ~1:a:thunkDefault]-> () -[[] + ~2:a:thunkDefault]-> a
> 
> let thunkDefault = \() -> \() -> T1
> #   ^^^^^^^^^^^^ () -[[`6]]-> () -[[`5]]-> T1
> let thunkDefault = \() -> \() -> T2
> #   ^^^^^^^^^^^^ () -[[`4]]-> () -[[`3]]-> T2
> 
> entry test1 =
>   let useT1 = \T1 -> () in
>   useT1 (thunkDefault () ())
> #        ^^^^^^^^^^^^ () -[[`6]]-> () -[[`5]]-> T1
> 
> entry test2 =
>   let useT2 = \T2 -> () in
>   useT2 (thunkDefault () ())
> #        ^^^^^^^^^^^^ () -[[`4]]-> () -[[`3]]-> T2
> 

> cor-out +mono -print
> let `5~1 =
>   \() -> T1
> 
> let `6(thunkDefault)~1 =
>   \() -> `5~1
> 
> let `2~1 =
>   \T1 -> ()
> 
> entry test1~1 =
>   `2~1 (`6(thunkDefault)~1 () ())
> 
> let `3~1 =
>   \() -> T2
> 
> let `4(thunkDefault)~1 =
>   \() -> `3~1
> 
> let `1~1 =
>   \T2 -> ()
> 
> entry test2~1 =
>   `1~1 (`4(thunkDefault)~1 () ())

> cor-out +eval -print
> test1~1 = ()
> 
> test2~1 = ()