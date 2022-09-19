# cor +solve -elab
# cor +mono -print
# cor +eval -print
proto thunkDefault a : () -> () -> a
#     ^^^^^^^^^^^^

let thunkDefault = \() -> \() -> T1
#   ^^^^^^^^^^^^

let uut1 = \() -> \() -> T1
#   ^^^^

let useT1 = \T1 -> ()

entry test1 =
  let f = choice {
      | thunkDefault
      | \() -> \() -> T1
  } in
  useT1 (f () ())
  #      ^

proto thunkDefault2 a : () -> () -> a

let thunkDefault2 = \() -> \() -> T1
#   ^^^^^^^^^^^^^

entry test2 =
  let f = choice {
  #   ^
      | thunkDefault
      | thunkDefault2
  } in
  useT1 (f () ())
  #      ^

entry test3 =
  let f = choice {
  #   ^
      | thunkDefault ()
      | thunkDefault2 ()
  } in
  useT1 (f ())
  #      ^

> cor-out +solve -elab
> proto thunkDefault a : () -> () -> a
> #     ^^^^^^^^^^^^ () -[[] + ~1:a:thunkDefault]-> () -[[] + ~2:a:thunkDefault]-> a
> 
> let thunkDefault = \() -> \() -> T1
> #   ^^^^^^^^^^^^ () -[[`9]]-> () -[[`8]]-> T1
> 
> let uut1 = \() -> \() -> T1
> #   ^^^^ () -[[`7]]-> () -[[`6]]-> T1
> 
> let useT1 = \T1 -> ()
> 
> entry test1 =
>   let f = choice {
>       | thunkDefault
>       | \() -> \() -> T1
>   } in
>   useT1 (f () ())
> #        ^ () -[[`4,`9]]-> () -[[`3,`8]]-> T1
> 
> proto thunkDefault2 a : () -> () -> a
> 
> let thunkDefault2 = \() -> \() -> T1
> #   ^^^^^^^^^^^^^ () -[[`2]]-> () -[[`1]]-> T1
> 
> entry test2 =
>   let f = choice {
> #     ^ () -[[] + ~1:?51:thunkDefault2 + ~1:?51:thunkDefault]->
>   () -[[] + ~2:?51:thunkDefault2 + ~2:?51:thunkDefault]-> ?51
>       | thunkDefault
>       | thunkDefault2
>   } in
>   useT1 (f () ())
> #        ^ () -[[`2,`9]]-> () -[[`1,`8]]-> T1
> 
> entry test3 =
>   let f = choice {
> #     ^ () -[[] + ~2:?65:thunkDefault2 + ~2:?65:thunkDefault]-> ?65
>       | thunkDefault ()
>       | thunkDefault2 ()
>   } in
>   useT1 (f ())
> #        ^ () -[[`1,`8]]-> T1
> 

> cor-out +mono -print
> let `3~1 =
>   \() -> T1
> 
> let `8~1 =
>   \() -> T1
> 
> let `4~1 =
>   \() -> choice {
>            | `3~1
>            | `8~1 }
> 
> let `3~2 =
>   \() -> T1
> 
> let `8~2 =
>   \() -> T1
> 
> let `9(thunkDefault)~1 =
>   \() -> choice {
>            | `3~2
>            | `8~2 }
> 
> let `5(useT1)~1 =
>   \T1 -> ()
> 
> entry test1~1 =
>   `5(useT1)~1 (choice {
>                  | `4~1
>                  | `9(thunkDefault)~1 } () ())
> 
> let `1~1 =
>   \() -> T1
> 
> let `2(thunkDefault2)~1 =
>   \() -> `1~1
> 
> let `3~3 =
>   \() -> T1
> 
> let `8~3 =
>   \() -> T1
> 
> let `9(thunkDefault)~2 =
>   \() -> choice {
>            | `3~3
>            | `8~3 }
> 
> let `5(useT1)~2 =
>   \T1 -> ()
> 
> entry test2~1 =
>   `5(useT1)~2 (choice {
>                  | `2(thunkDefault2)~1
>                  | `9(thunkDefault)~2 } () ())
> 
> let `1~2 =
>   \() -> T1
> 
> let `8~4 =
>   \() -> T1
> 
> let `5(useT1)~3 =
>   \T1 -> ()
> 
> entry test3~1 =
>   `5(useT1)~3 (choice {
>                  | `1~2
>                  | `8~4 } ())

> cor-out +eval -print
> test1~1 = () | () | () | ()
> 
> test2~1 = () | () | ()
> 
> test3~1 = () | ()