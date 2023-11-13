# cor +parse -print
# cor +can -print
# Task.roc
Task v op : (v -> op) -> op

sig await : Task a op -> (a -> Task b op) -> Task b op
let await = \fromResult -> \next ->
    \continue ->
        fromResult (\result ->
            let inner = next result in
            inner continue)
;;

# StdinEffect.roc
OpIn a b : [
    StdinLine (Str -> OpIn a b),
    Done a,
]b

# Stdin.roc
sig lineIn : Task Str (OpIn * *)
let lineIn = \toNext -> StdinLine (\s -> toNext s)
;;

# StdoutEffect.roc
OpOut a b : [
    StdoutLine Str ({} -> OpOut a b),
    Done a,
]b

# Stdout.roc
sig lineOut : Str -> Task {} (OpOut * *)
let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
;;

# Platform
# really, we want a syntax like [Done a](OpIn a)(OpOut a) here
Op a : [
    StdinLine (Str -> Op a),
    StdoutLine Str ({} -> Op a),
    Done a,
]

sig main : Task {} (Op *)
run main = await lineIn (\s -> lineOut s)
;;

> cor-out +parse -print
> Task 'v 'op : ('v1 -> 'op1) -> 'op2
> 
> sig await :
>   (Task 'a 'op)
>     -> ('a1 -> Task 'b 'op1) -> Task 'b1 'op2
> let await =
>   \fromResult ->
>     \next ->
>       \continue ->
>         fromResult
>           \result ->
>             (let inner = next result in
>             inner continue)
> 
> OpIn 'a 'b :
>   [
>      StdinLine (Str -> OpIn 'a1 'b1),
>      Done 'a2
>   ]'b2
> 
> sig lineIn : Task Str (OpIn '* '*)
> let lineIn =
>   \toNext -> (StdinLine \s -> toNext s)
> 
> OpOut 'a 'b :
>   [
>      StdoutLine Str ({} -> OpOut 'a1 'b1),
>      Done 'a2
>   ]'b2
> 
> sig lineOut : Str -> Task {} (OpOut '* '*)
> let lineOut =
>   \s -> \toNext -> (StdoutLine s \x -> toNext x)
> 
> Op 'a :
>   [
>      StdinLine (Str -> Op 'a1),
>      StdoutLine Str ({} -> Op 'a2),
>      Done 'a3
>   ]
> 
> sig main : Task {} (Op '*)
> run main = await lineIn \s -> lineOut s

> cor-out +can -print
> Task 'v 'op : ('v -> 'op) -> 'op
> 
> sig await :
>   (Task 'a 'op)
>     -> ('a -> Task 'b 'op) -> Task 'b 'op
> let await =
>   \fromResult ->
>     \next ->
>       \continue ->
>         fromResult
>           \result ->
>             (let inner = next result in
>             inner continue)
> 
> OpIn 'a 'b :
>   [
>      StdinLine (Str -> <..OpIn 'a 'b>),
>      Done 'a
>   ]'b
> 
> sig lineIn : Task Str (OpIn '* '*)
> let lineIn =
>   \toNext -> (StdinLine \s -> toNext s)
> 
> OpOut 'a 'b :
>   [
>      StdoutLine Str ({} -> <..OpOut 'a 'b>),
>      Done 'a
>   ]'b
> 
> sig lineOut : Str -> Task {} (OpOut '* '*)
> let lineOut =
>   \s -> \toNext -> (StdoutLine s \x -> toNext x)
> 
> Op 'a :
>   [
>      StdinLine (Str -> <..Op 'a>),
>      StdoutLine Str ({} -> <..Op 'a>),
>      Done 'a
>   ]
> 
> sig main : Task {} (Op '*)
> run main = await lineIn \s -> lineOut s
