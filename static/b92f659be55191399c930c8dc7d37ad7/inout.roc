# cor +solve -elab
# cor +ir -print
# cor +eval -print
# Task.roc
Task v op : (v -> op) -> op

sig await : Task a op -> (a -> Task b op) -> Task b op
let await = \fromResult -> \next ->
    \continue ->
        fromResult (\result ->
#       ^^^^^^^^^^
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
    StdoutLine Str (Str -> OpOut a b),
    Done a,
]b

# Stdout.roc
sig lineOut : Str -> Task Str (OpOut * *)
let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
;;

# Platform
# really, we want a syntax like [Done a](OpIn a)(OpOut a) here
Op a : [
    StdinLine (Str -> Op a),
    StdoutLine Str (Str -> Op a),
    Done a,
]

sig main : Task Str (Op *)
let main = await lineIn (\s -> lineOut s)
#          ^^^^^          ^    ^^^^^^^
;;

run main_handler =
#   ^^^^^^^^^^^^
    let op = main (\x -> Done x) in
#       ^^
    let handle = \op -> when op is
#       ^^^^^^
        | StdinLine f -> handle (f "hello")
        | StdoutLine s f -> handle (f s)
        | Done x -> x
    end
    in
    handle op
#          ^^
;;

> cor-out +solve -elab
> # Task.roc
> Task v op : (v -> op) -> op
> 
> sig await : Task a op -> (a -> Task b op) -> Task b op
> let await = \fromResult -> \next ->
>     \continue ->
>         fromResult (\result ->
> #       ^^^^^^^^^^ Task 'a 'op
>             let inner = next result in
>             inner continue)
> ;;
> 
> # StdinEffect.roc
> OpIn a b : [
>     StdinLine (Str -> OpIn a b),
>     Done a,
> ]b
> 
> # Stdin.roc
> sig lineIn : Task Str (OpIn * *)
> let lineIn = \toNext -> StdinLine (\s -> toNext s)
> ;;
> 
> # StdoutEffect.roc
> OpOut a b : [
>     StdoutLine Str (Str -> OpOut a b),
>     Done a,
> ]b
> 
> # Stdout.roc
> sig lineOut : Str -> Task Str (OpOut * *)
> let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
> ;;
> 
> # Platform
> # really, we want a syntax like [Done a](OpIn a)(OpOut a) here
> Op a : [
>     StdinLine (Str -> Op a),
>     StdoutLine Str (Str -> Op a),
>     Done a,
> ]
> 
> sig main : Task Str (Op *)
> let main = await lineIn (\s -> lineOut s)
> #                              ^^^^^^^ Str -[lineOut]-> Task Str (Op 'a)
> #                         ^ Str
> #          ^^^^^ (Task Str (Op 'a))
> #          ^^^^^   -[await]-> (Str -[lam6]-> Task Str (Op 'a))
> #          ^^^^^                -[lam2 (Task Str (Op 'a))]-> 
> #          ^^^^^                Task Str (Op 'a)
> ;;
> 
> run main_handler =
> #   ^^^^^^^^^^^^ Str
>     let op = main (\x -> Done x) in
> #       ^^ Op Str
>     let handle = \op -> when op is
> #       ^^^^^^ (Op Str) -[handle1]-> Str
>         | StdinLine f -> handle (f "hello")
>         | StdoutLine s f -> handle (f s)
>         | Done x -> x
>     end
>     in
>     handle op
> #          ^^ Op Str
> ;;
> 

> cor-out +ir -print
> proc lam21(captures_: [ `0 { [ `0 {} ] } ], next: [ `0 {} ]):
>   [ `0 { [ `0 {} ], [ `0 {} ] } ]
> {
>   let captures_stack: { [ `0 {} ] } = @get_union_struct<captures_>;
>   let fromResult: [ `0 {} ] = @get_struct_field<captures_stack, 0>;
>   let struct: { [ `0 {} ], [ `0 {} ] } = @make_struct{ fromResult, next };
>   let var: [ `0 { [ `0 {} ], [ `0 {} ] } ] = @make_union<0, struct>;
>   return var;
> }
> 
> proc lam71(captures_10: [ `0 {} ], x1: str):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack10: {} = @get_union_struct<captures_10>;
>   let struct12: { str } = @make_struct{ x1 };
>   let var12:
>         [
>            `0 { str },
>            `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>            `2 { str, [ `0 { [ `0 {} ] } ] }
>         ]
>     = @make_union<0, struct12>;
>   return var12;
> }
> 
> proc lineOut2(captures_9: [ `0 {} ], s1: str): [ `0 { str } ]
> {
>   let captures_stack9: {} = @get_union_struct<captures_9>;
>   let struct8: { str } = @make_struct{ s1 };
>   let var8: [ `0 { str } ] = @make_union<0, struct8>;
>   return var8;
> }
> 
> proc await2(captures_3: [ `0 {} ], fromResult: [ `0 {} ]): [ `0 { [ `0 {} ] } ]
> {
>   let captures_stack3: {} = @get_union_struct<captures_3>;
>   let struct2: { [ `0 {} ] } = @make_struct{ fromResult };
>   let var2: [ `0 { [ `0 {} ] } ] = @make_union<0, struct2>;
>   return var2;
> }
> 
> proc lineIn2(captures_5: [ `0 {} ], toNext: [ `0 { [ `0 {} ], [ `0 {} ] } ]):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack5: {} = @get_union_struct<captures_5>;
>   let struct3: { [ `0 { [ `0 {} ], [ `0 {} ] } ] } = @make_struct{ toNext };
>   let var3: [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ]
>     = @make_union<0, struct3>;
>   let struct4: { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] }
>     = @make_struct{ var3 };
>   let var4:
>         [
>            `0 { str },
>            `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>            `2 { str, [ `0 { [ `0 {} ] } ] }
>         ]
>     = @make_union<1, struct4>;
>   return var4;
> }
> 
> proc lam51(captures_7: [ `0 { str } ], toNext1: [ `0 {} ]):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack7: { str } = @get_union_struct<captures_7>;
>   let s1: str = @get_struct_field<captures_stack7, 0>;
>   let struct6: { [ `0 {} ] } = @make_struct{ toNext1 };
>   let var6: [ `0 { [ `0 {} ] } ] = @make_union<0, struct6>;
>   let struct7: { str, [ `0 { [ `0 {} ] } ] } = @make_struct{ s1, var6 };
>   let var7:
>         [
>            `0 { str },
>            `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>            `2 { str, [ `0 { [ `0 {} ] } ] }
>         ]
>     = @make_union<2, struct7>;
>   return var7;
> }
> 
> proc lam41(captures_8: [ `0 { [ `0 {} ] } ], x: str):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack8: { [ `0 {} ] } = @get_union_struct<captures_8>;
>   let toNext1: [ `0 {} ] = @get_struct_field<captures_stack8, 0>;
>   let cond5: int = @get_union_id<toNext1>;
>   switch cond5 {
>   0 -> { @call_direct(lam71, toNext1, x) }
>   } in join join5;
>   return join5;
> }
> 
> proc lam61(captures_6: [ `0 {} ], s2: str): [ `0 { str } ]
> {
>   let captures_stack6: {} = @get_union_struct<captures_6>;
>   let struct5: {} = @make_struct{};
>   let var5: [ `0 {} ] = @make_union<0, struct5>;
>   let cond4: int = @get_union_id<var5>;
>   switch cond4 {
>   0 -> { @call_direct(lineOut2, var5, s2) }
>   } in join join4;
>   return join4;
> }
> 
> proc main_thunk(): [ `0 { [ `0 {} ], [ `0 {} ] } ]
> {
>   let struct9: {} = @make_struct{};
>   let var9: [ `0 {} ] = @make_union<0, struct9>;
>   let struct10: {} = @make_struct{};
>   let var10: [ `0 {} ] = @make_union<0, struct10>;
>   let cond6: int = @get_union_id<var9>;
>   switch cond6 {
>   0 -> { @call_direct(await2, var9, var10) }
>   } in join join6;
>   let struct11: {} = @make_struct{};
>   let var11: [ `0 {} ] = @make_union<0, struct11>;
>   let cond7: int = @get_union_id<join6>;
>   switch cond7 {
>   0 -> { @call_direct(lam21, join6, var11) }
>   } in join join7;
>   return join7;
> }
> 
> proc lam11(captures_1: [ `0 { [ `0 {} ], [ `0 {} ] } ], continue: [ `0 {} ]):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack1: { [ `0 {} ], [ `0 {} ] } = @get_union_struct<captures_1>;
>   let fromResult: [ `0 {} ] = @get_struct_field<captures_stack1, 0>;
>   let next: [ `0 {} ] = @get_struct_field<captures_stack1, 1>;
>   let struct1: { [ `0 {} ], [ `0 {} ] } = @make_struct{ continue, next };
>   let var1: [ `0 { [ `0 {} ], [ `0 {} ] } ] = @make_union<0, struct1>;
>   let cond: int = @get_union_id<fromResult>;
>   switch cond {
>   0 -> { @call_direct(lineIn2, fromResult, var1) }
>   } in join join;
>   return join;
> }
> 
> proc lam8(captures_2: [ `0 { [ `0 {} ], [ `0 {} ] } ], result: str):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack2: { [ `0 {} ], [ `0 {} ] } = @get_union_struct<captures_2>;
>   let continue: [ `0 {} ] = @get_struct_field<captures_stack2, 0>;
>   let next: [ `0 {} ] = @get_struct_field<captures_stack2, 1>;
>   let cond1: int = @get_union_id<next>;
>   switch cond1 {
>   0 -> { @call_direct(lam61, next, result) }
>   } in join join1;
>   let inner: [ `0 { str } ] = join1;
>   let cond2: int = @get_union_id<inner>;
>   switch cond2 {
>   0 -> { @call_direct(lam51, inner, continue) }
>   } in join join2;
>   return join2;
> }
> 
> global main1: [ `0 { [ `0 {} ], [ `0 {} ] } ] = @call_direct(main_thunk);
> 
> proc lam31(captures_4: [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ], s: str):
>   [
>      `0 { str },
>      `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>      `2 { str, [ `0 { [ `0 {} ] } ] }
>   ]
> {
>   let captures_stack4: { [ `0 { [ `0 {} ], [ `0 {} ] } ] }
>     = @get_union_struct<captures_4>;
>   let toNext: [ `0 { [ `0 {} ], [ `0 {} ] } ]
>     = @get_struct_field<captures_stack4, 0>;
>   let cond3: int = @get_union_id<toNext>;
>   switch cond3 {
>   0 -> { @call_direct(lam8, toNext, s) }
>   } in join join3;
>   return join3;
> }
> 
> proc handle11(
>   captures_handle: [ `0 {} ],
>    op1:
>      [
>         `0 { str },
>         `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>         `2 { str, [ `0 { [ `0 {} ] } ] }
>      ]):
>   str
> {
>   let captures_stack11: {} = @get_union_struct<captures_handle>;
>   let struct13: {} = @make_struct{};
>   let handle: [ `0 {} ] = @make_union<0, struct13>;
>   let discr: int = @get_union_id<op1>;
>   switch discr {
>   0 -> {
>     let payload2: { str } = @get_union_struct<op1>;
>     let x2: str = @get_struct_field<payload2, 0>;
>     x2
>   }
>   1 -> {
>     let payload: { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] }
>       = @get_union_struct<op1>;
>     let f: [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ]
>       = @get_struct_field<payload, 0>;
>     let var13: str = "hello";
>     let cond8: int = @get_union_id<f>;
>     switch cond8 {
>     0 -> { @call_direct(lam31, f, var13) }
>     } in join join8;
>     let cond9: int = @get_union_id<handle>;
>     switch cond9 {
>     0 -> { @call_direct(handle11, handle, join8) }
>     } in join join9;
>     join9
>   }
>   2 -> {
>     let payload1: { str, [ `0 { [ `0 {} ] } ] } = @get_union_struct<op1>;
>     let s3: str = @get_struct_field<payload1, 0>;
>     let f1: [ `0 { [ `0 {} ] } ] = @get_struct_field<payload1, 1>;
>     let cond10: int = @get_union_id<f1>;
>     switch cond10 {
>     0 -> { @call_direct(lam41, f1, s3) }
>     } in join join10;
>     let cond11: int = @get_union_id<handle>;
>     switch cond11 {
>     0 -> { @call_direct(handle11, handle, join10) }
>     } in join join11;
>     join11
>   }
>   } in join join12;
>   return join12;
> }
> 
> proc main_handler_thunk(): str
> {
>   let struct14: {} = @make_struct{};
>   let var14: [ `0 {} ] = @make_union<0, struct14>;
>   let cond12: int = @get_union_id<main1>;
>   switch cond12 {
>   0 -> { @call_direct(lam11, main1, var14) }
>   } in join join13;
>   let op:
>         [
>            `0 { str },
>            `1 { [ `0 { [ `0 { [ `0 {} ], [ `0 {} ] } ] } ] },
>            `2 { str, [ `0 { [ `0 {} ] } ] }
>         ]
>     = join13;
>   let struct15: {} = @make_struct{};
>   let handle: [ `0 {} ] = @make_union<0, struct15>;
>   let cond13: int = @get_union_id<handle>;
>   switch cond13 {
>   0 -> { @call_direct(handle11, handle, op) }
>   } in join join14;
>   return join14;
> }
> 
> global main_handler: str = @call_direct(main_handler_thunk);
> 
> entry main_handler;

> cor-out +eval -print
> main_handler = [104 101 108 108 111]
>              > "hello"