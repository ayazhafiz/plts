# cor +solve -elab
# cor +ir -print
# cor +eval -print
proto thunkDefault a : () -> () -> a

let thunkDefault = \() -> \() -> T1
#   ^^^^^^^^^^^^

proto thunkDefault2 a : () -> () -> a

let echoT1 = \T1 -> T1

let thunkDefault2 =
#   ^^^^^^^^^^^^^
    \() -> \() ->
        echoT1 (thunkDefault () ())
        #       ^^^^^^^^^^^^

entry main = echoT1 (thunkDefault2 () ())
             #       ^^^^^^^^^^^^^

> cor-out +solve -elab
> proto thunkDefault a : () -> () -> a
> 
> let thunkDefault = \() -> \() -> T1
> #   ^^^^^^^^^^^^ () -[[`5]]-> () -[[`4]]-> T1
> 
> proto thunkDefault2 a : () -> () -> a
> 
> let echoT1 = \T1 -> T1
> 
> let thunkDefault2 =
> #   ^^^^^^^^^^^^^ () -[[`2]]-> () -[[`1]]-> T1
>     \() -> \() ->
>         echoT1 (thunkDefault () ())
> #               ^^^^^^^^^^^^ () -[[`5]]-> () -[[`4]]-> T1
> 
> entry main = echoT1 (thunkDefault2 () ())
> #                    ^^^^^^^^^^^^^ () -[[`2]]-> () -[[`1]]-> T1
> 

> cor-out +ir -print
> let `4~1 =
>   \() -> T1
> 
> let `5(thunkDefault)~1 =
>   \() -> `4~1
> 
> let `3(echoT1)~1 =
>   \T1 -> T1
> 
> let `1~1 =
>   \() -> `3(echoT1)~1 (`5(thunkDefault)~1 () ())
> 
> let `2(thunkDefault2)~1 =
>   \() -> `1~1
> 
> let `3(echoT1)~2 =
>   \T1 -> T1
> 
> entry main~1 =
>   `3(echoT1)~2 (`2(thunkDefault2)~1 () ())

> cor-out +eval -print
> main~1 = T1