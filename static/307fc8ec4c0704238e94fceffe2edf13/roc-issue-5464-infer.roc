# cor +solve -elab
# cor +mono -print
# TODO # cor +ir -print
# TODO # cor +eval -print

let succeed = \ok -> \toNext -> toNext (Ok ok);;

let fail = \err-> \toNext -> toNext (Err err);;

let await = \fromResult -> \next ->
    \continue -> fromResult (\result ->
        let inner = when result is
            | Ok v -> next v
            | Err e -> fail e
        end
        in
        inner continue)
;;


let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;

let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;

let main =
    await (outLine "What's your first name?")
        (\x -> await (inLine)
            (\firstName -> await (outLine "What's your last name?")
                (\y -> await (inLine)
                    (\lastName -> outLine (~str_concat "Hello " firstName " " lastName "!")))))
;;

run main_handler =
#   ^^^^^^^^^^^^
    let op = main (\x -> Done x) in
    let handle = \op -> \i -> \t -> when op is
        | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
        | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
        | Done x -> Done x t
    end
    in
    handle op 0 EntryPoint
;;

> cor-out +solve -elab
> # TODO # cor +ir -print
> # TODO # cor +eval -print
> 
> let succeed = \ok -> \toNext -> toNext (Ok ok);;
> 
> let fail = \err-> \toNext -> toNext (Err err);;
> 
> let await = \fromResult -> \next ->
>     \continue -> fromResult (\result ->
>         let inner = when result is
>             | Ok v -> next v
>             | Err e -> fail e
>         end
>         in
>         inner continue)
> ;;
> 
> 
> let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;
> 
> let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;
> 
> let main =
>     await (outLine "What's your first name?")
>         (\x -> await (inLine)
>             (\firstName -> await (outLine "What's your last name?")
>                 (\y -> await (inLine)
>                     (\lastName -> outLine (~str_concat "Hello " firstName " " lastName "!")))))
> ;;
> 
> run main_handler =
> #   ^^^^^^^^^^^^ [
> #   ^^^^^^^^^^^^   Done [Err ?*, Ok {}]?*
> #   ^^^^^^^^^^^^     [
> #   ^^^^^^^^^^^^       EntryPoint,
> #   ^^^^^^^^^^^^       Stdin
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]?*>,
> #   ^^^^^^^^^^^^       Stdout Str
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]?*>
> #   ^^^^^^^^^^^^       ]?*
> #   ^^^^^^^^^^^^   ]?*
>     let op = main (\x -> Done x) in
>     let handle = \op -> \i -> \t -> when op is
>         | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
>         | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
>         | Done x -> Done x t
>     end
>     in
>     handle op 0 EntryPoint
> ;;
> 

> cor-out +mono -print
> specializations:
>   let lam61 = \-[lam61 fromResult]-> next
>     \continue -[lam51 fromResult next]->
>       (fromResult
>          \result -[lam41 continue next]->
>            (let inner =
>               when result is
>                 | Okv -> next v
>                 | Erre -> fail1 e
>               end
>            in
>            inner continue))
>   
>   let lam42 = \-[lam42 continue next]-> result
>     let inner =
>       when result is
>         | Okv -> next v
>         | Erre -> fail2 e
>       end
>     in
>     inner continue
>   
>   let fail2 = \-[fail2]-> err
>     \toNext1 -[lam21 err]-> (toNext1 (Err err))
>   
>   let lam51 = \-[lam51 fromResult next]-> continue
>     fromResult
>       \result -[lam42 continue next]->
>         (let inner =
>            when result is
>              | Okv -> next v
>              | Erre -> fail2 e
>            end
>         in
>         inner continue)
>   
>   let lam41 = \-[lam41 continue next]-> result
>     let inner =
>       when result is
>         | Okv -> next v
>         | Erre -> fail2 e
>       end
>     in
>     inner continue
>   
>   let lam21 = \-[lam21 err]-> toNext1
>     toNext1 (Err err)
>   
>   let fail1 = \-[fail1]-> err
>     \toNext1 -[lam21 err]-> (toNext1 (Err err))
>   
>   let await1 = \-[await1]-> fromResult
>     \next -[lam61 fromResult]->
>       \continue -[lam51 fromResult next]->
>         (fromResult
>            \result -[lam41 continue next]->
>              (let inner =
>                 when result is
>                   | Okv -> next v
>                   | Erre -> fail1 e
>                 end
>              in
>              inner continue))
>   
>   let lam82 = \-[lam82 toNext2]-> x
>     toNext2 (Ok x)
>   
>   let lam91 = \-[lam91 s]-> toNext2
>     StdoutLine s
>       \x -[lam82 toNext2]-> (toNext2 (Ok x))
>   
>   let lam81 = \-[lam81 toNext2]-> x
>     toNext2 (Ok x)
>   
>   let outLine1 = \-[outLine1]-> s
>     \toNext2 -[lam91 s]->
>       (StdoutLine s
>          \x -[lam81 toNext2]-> (toNext2 (
>                                         Ok x)))
>   
>   let inLine2 = \-[inLine2]-> toNext3
>     StdinLine
>       \s1 -[lam111 toNext3]-> (toNext3 (Ok s1))
>   
>   let lam161 = \-[lam161]-> x1
>     (await1 inLine2)
>       \firstName -[lam151]->
>         ((await1
>             (outLine1 "What's your last name?"))
>            \y -[lam141 firstName]->
>              ((await1 inLine2)
>                 \lastName -[lam131 firstName]->
>                   (outLine1
>                      str_concat "Hello "
>                        firstName " " lastName "!")))
>   
>   let lam111 = \-[lam111 toNext3]-> s1
>     toNext3 (Ok s1)
>   
>   let inLine1 = \-[inLine1]-> toNext3
>     StdinLine
>       \s1 -[lam111 toNext3]-> (toNext3 (Ok s1))
>   
>   let lam151 = \-[lam151]-> firstName
>     (await1 (outLine1 "What's your last name?"))
>       \y -[lam141 firstName]->
>         ((await1 inLine2)
>            \lastName -[lam131 firstName]->
>              (outLine1
>                 str_concat "Hello " firstName " "
>                   lastName "!"))
>   
>   let lam141 = \-[lam141 firstName]-> y
>     (await1 inLine2)
>       \lastName -[lam131 firstName]->
>         (outLine1
>            str_concat "Hello " firstName " "
>              lastName "!")
>   
>   let lam131 = \-[lam131 firstName]-> lastName
>     outLine1
>       str_concat "Hello " firstName " " lastName
>         "!"
>   
>   let main1 =
>     (await1 (outLine1 "What's your first name?"))
>       \x1 -[lam161]->
>         ((await1 inLine1)
>            \firstName -[lam151]->
>              ((await1
>                  (outLine1
>                     "What's your last name?"))
>                 \y -[lam141 firstName]->
>                   ((await1 inLine1)
>                      \lastName -[lam131 firstName]->
>                        (outLine1
>                           str_concat "Hello "
>                             firstName " "
>                             lastName "!"))))
>   
>   let lam171 = \-[lam171]-> x2 Done x2
>   
>   let handle2 = \-[handle2]-> op1
>     \i -[lam191 handle2 op1]->
>       \t -[lam181 handle2 i op1]->
>         when op1 is
>           | StdinLinef ->
>             ((handle2
>                 (f str_concat "stdin" itos i))
>                add i 1) (Stdin t)
>           | StdoutLines2 f1 ->
>             ((handle2 (f1 {})) add i 1)
>               (Stdout s2 t)
>           | Donex3 -> Done x3 t
>         end
>   
>   let handle1 = \-[handle1]-> op1
>     \i -[lam191 handle2 op1]->
>       \t -[lam181 handle2 i op1]->
>         when op1 is
>           | StdinLinef ->
>             ((handle2
>                 (f str_concat "stdin" itos i))
>                add i 1) (Stdin t)
>           | StdoutLines2 f1 ->
>             ((handle2 (f1 {})) add i 1)
>               (Stdout s2 t)
>           | Donex3 -> Done x3 t
>         end
>   
>   let handle3 = \-[handle3]-> op1
>     \i -[lam191 handle3 op1]->
>       \t -[lam181 handle3 i op1]->
>         when op1 is
>           | StdinLinef ->
>             ((handle3
>                 (f str_concat "stdin" itos i))
>                add i 1) (Stdin t)
>           | StdoutLines2 f1 ->
>             ((handle3 (f1 {})) add i 1)
>               (Stdout s2 t)
>           | Donex3 -> Done x3 t
>         end
>   
>   let lam191 = \-[lam191 handle3 op1]-> i
>     \t -[lam181 handle3 i op1]->
>       when op1 is
>         | StdinLinef ->
>           ((handle3 (f str_concat "stdin" itos i))
>              add i 1) (Stdin t)
>         | StdoutLines2 f1 ->
>           ((handle3 (f1 {})) add i 1)
>             (Stdout s2 t)
>         | Donex3 -> Done x3 t
>       end
>   
>   let lam181 = \-[lam181 handle3 i op1]-> t
>     when op1 is
>       | StdinLinef ->
>         ((handle3 (f str_concat "stdin" itos i))
>            add i 1) (Stdin t)
>       | StdoutLines2 f1 ->
>         ((handle3 (f1 {})) add i 1) (Stdout s2 t)
>       | Donex3 -> Done x3 t
>     end
>   
>   let main_handler =
>     let op = main1 \x2 -[lam171]-> (Done x2) in
>     let handle1 =
>       \op1 -[handle1]->
>         \i -[lam191 handle1 op1]->
>           \t -[lam181 handle1 i op1]->
>             when op1 is
>               | StdinLinef ->
>                 ((handle1
>                     (f str_concat "stdin" itos i))
>                    add i 1) (Stdin t)
>               | StdoutLines2 f1 ->
>                 ((handle1 (f1 {})) add i 1)
>                   (Stdout s2 t)
>               | Donex3 -> Done x3 t
>             end
>     in
>     ((handle1 op) 0) (EntryPoint )
>   
>   
> entry_points:
>   main_handler