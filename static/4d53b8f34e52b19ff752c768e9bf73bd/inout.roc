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
> #                              ^^^^^^^ Str -[lineOut]-> Task Str %(Op 'a)
> #                         ^ Str
> #          ^^^^^ (Task Str %(Op 'a))
> #          ^^^^^   -[await]-> (Str -[lam9]-> Task Str %(Op 'a))
> #          ^^^^^                -[lam2 (Task Str %(Op 'a))]-> 
> #          ^^^^^                Task Str %(Op 'a)
> ;;
> 
> run main_handler =
> #   ^^^^^^^^^^^^ Str
>     let op = main (\x -> Done x) in
> #       ^^ %Op Str
>     let handle = \op -> when op is
> #       ^^^^^^ %(Op Str) -[handle]-> Str
>         | StdinLine f -> handle (f "hello")
>         | StdoutLine s f -> handle (f s)
>         | Done x -> x
>     end
>     in
>     handle op
> #          ^^ %Op Str
> ;;
> 

> cor-out +ir -print
> proc lam41(captures_5: box<erased>, s: str):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box4: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_5 as box<{ { *fn, box<erased> } }>);
>   let captures_stack4: { { *fn, box<erased> } } = @get_boxed<captures_box4>;
>   let toNext: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let fnptr3: *fn = @get_struct_field<toNext, 0>;
>   let captures3: box<erased> = @get_struct_field<toNext, 1>;
>   let var5:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr3, captures3, s);
>   return var5;
> }
> 
> proc lam61(captures_10: box<erased>, x: str):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box8: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_10 as box<{ { *fn, box<erased> } }>);
>   let captures_stack8: { { *fn, box<erased> } } = @get_boxed<captures_box8>;
>   let toNext1: { *fn, box<erased> } = @get_struct_field<captures_stack8, 0>;
>   let fnptr5: *fn = @get_struct_field<toNext1, 0>;
>   let captures5: box<erased> = @get_struct_field<toNext1, 1>;
>   let var11:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr5, captures5, x);
>   return var11;
> }
> 
> proc handle1(
>   captures_handle: box<erased>,
>    op1:
>      [
>         `0 { str },
>         `1 { { *fn, box<erased> } },
>         `2 { str, { *fn, box<erased> } }
>      ]):
>   str
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let discr: int = @get_union_id<op1>;
>   switch discr {
>   0 -> {
>     let payload2: { str } = @get_union_struct<op1>;
>     let x2: str = @get_struct_field<payload2, 0>;
>     x2
>   }
>   1 -> {
>     let payload: { { *fn, box<erased> } } = @get_union_struct<op1>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload, 0>;
>     let fnptr8: *fn = @get_struct_field<handle1, 0>;
>     let captures8: box<erased> = @get_struct_field<handle1, 1>;
>     let fnptr9: *fn = @get_struct_field<f, 0>;
>     let captures9: box<erased> = @get_struct_field<f, 1>;
>     let var17: str = "hello";
>     let var18:
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>       = @call_indirect(fnptr9, captures9, var17);
>     @call_indirect(fnptr8, captures8, var18)
>   }
>   2 -> {
>     let payload1: { str, { *fn, box<erased> } } = @get_union_struct<op1>;
>     let s3: str = @get_struct_field<payload1, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload1, 1>;
>     let fnptr10: *fn = @get_struct_field<handle1, 0>;
>     let captures10: box<erased> = @get_struct_field<handle1, 1>;
>     let fnptr11: *fn = @get_struct_field<f1, 0>;
>     let captures11: box<erased> = @get_struct_field<f1, 1>;
>     let var19:
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>       = @call_indirect(fnptr11, captures11, s3);
>     @call_indirect(fnptr10, captures10, var19)
>   }
>   } in join join;
>   return join;
> }
> 
> proc lam101(captures_13: box<erased>, x1: str):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box10: box<{}> = @ptr_cast(captures_13 as box<{}>);
>   let captures_stack10: {} = @get_boxed<captures_box10>;
>   let struct2: { str } = @make_struct{ x1 };
>   let var16:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct2>;
>   return var16;
> }
> 
> proc lam13(captures_2: box<erased>, result: str):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box2: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_2 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box2>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack2, 1>;
>   let fnptr1: *fn = @get_struct_field<next, 0>;
>   let captures1: box<erased> = @get_struct_field<next, 1>;
>   let inner: { *fn, box<erased> } = @call_indirect(fnptr1, captures1, result);
>   let fnptr2: *fn = @get_struct_field<inner, 0>;
>   let captures2: box<erased> = @get_struct_field<inner, 1>;
>   let var3:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr2, captures2, continue);
>   return var3;
> }
> 
> proc clos_lineIn(captures_6: box<erased>, toNext: { *fn, box<erased> }):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box5: box<{}> = @ptr_cast(captures_6 as box<{}>);
>   let captures_stack5: {} = @get_boxed<captures_box5>;
>   let captures_stack_6: { { *fn, box<erased> } } = @make_struct{ toNext };
>   let captures_box_6: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_6);
>   let captures_17: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<lam41>;
>   let var6: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_17 };
>   let struct: { { *fn, box<erased> } } = @make_struct{ var6 };
>   let var7:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct>;
>   return var7;
> }
> 
> proc lam71(captures_9: box<erased>, toNext1: { *fn, box<erased> }):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_9 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s1: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_7: { { *fn, box<erased> } } = @make_struct{ toNext1 };
>   let captures_box_7: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_7);
>   let captures_18: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<lam61>;
>   let var9: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_18 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s1, var9 };
>   let var10:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct1>;
>   return var10;
> }
> 
> proc lam12(captures_1: box<erased>, continue: { *fn, box<erased> }):
>   [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } } ]
> {
>   let captures_box1: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_1 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack1: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box1>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack1, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack1, 1>;
>   let fnptr: *fn = @get_struct_field<fromResult, 0>;
>   let captures: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_4);
>   let captures_15: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<lam13>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_15 };
>   let var2:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr, captures, var1);
>   return var2;
> }
> 
> proc lineIn_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_7: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_lineIn>;
>   let lineIn_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_7 };
>   return lineIn_closure;
> }
> 
> proc clos_lineOut(captures_11: box<erased>, s1: str): { *fn, box<erased> }
> {
>   let captures_box9: box<{}> = @ptr_cast(captures_11 as box<{}>);
>   let captures_stack9: {} = @get_boxed<captures_box9>;
>   let captures_stack_8: { str } = @make_struct{ s1 };
>   let captures_box_8: box<{ str }> = @make_box(captures_stack_8);
>   let captures_19: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<lam71>;
>   let var12: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_19 };
>   return var12;
> }
> 
> proc lam21(captures_: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_ as box<{ { *fn, box<erased> } }>);
>   let captures_stack: { { *fn, box<erased> } } = @get_boxed<captures_box>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack, 0>;
>   let captures_stack_3: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_3);
>   let captures_14: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<lam12>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_3, captures_14 };
>   return var;
> }
> 
> global lineIn1: { *fn, box<erased> } = @call_direct(lineIn_thunk);
> 
> proc lineOut_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_12: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_lineOut>;
>   let lineOut_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_12 };
>   return lineOut_closure;
> }
> 
> proc clos_await(captures_3: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box3: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack3: {} = @get_boxed<captures_box3>;
>   let captures_stack_5: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_5: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_5);
>   let captures_16: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<lam21>;
>   let var4: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_16 };
>   return var4;
> }
> 
> global lineOut1: { *fn, box<erased> } = @call_direct(lineOut_thunk);
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_4: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_await>;
>   let await_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_, captures_4 };
>   return await_closure;
> }
> 
> proc lam91(captures_8: box<erased>, s2: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_8 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let fnptr4: *fn = @get_struct_field<lineOut1, 0>;
>   let captures4: box<erased> = @get_struct_field<lineOut1, 1>;
>   let var8: { *fn, box<erased> } = @call_indirect(fnptr4, captures4, s2);
>   return var8;
> }
> 
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr6: *fn = @get_struct_field<await1, 0>;
>   let captures6: box<erased> = @get_struct_field<await1, 1>;
>   let var13: { *fn, box<erased> } = @call_indirect(fnptr6, captures6, lineIn1);
>   let fnptr7: *fn = @get_struct_field<var13, 0>;
>   let captures7: box<erased> = @get_struct_field<var13, 1>;
>   let captures_stack_9: {} = @make_struct{};
>   let captures_box_9: box<{}> = @make_box(captures_stack_9);
>   let captures_20: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<lam91>;
>   let var14: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_20 };
>   let var15: { *fn, box<erased> } = @call_indirect(fnptr7, captures7, var14);
>   return var15;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc main_handler_thunk(): str
> {
>   let fnptr12: *fn = @get_struct_field<main1, 0>;
>   let captures12: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_10: {} = @make_struct{};
>   let captures_box_10: box<{}> = @make_box(captures_stack_10);
>   let captures_21: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<lam101>;
>   let var20: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_21 };
>   let op:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr12, captures12, var20);
>   let captures_stack_11: {} = @make_struct{};
>   let captures_box_11: box<{}> = @make_box(captures_stack_11);
>   let captures_22: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_22 };
>   let fnptr13: *fn = @get_struct_field<handle1, 0>;
>   let captures13: box<erased> = @get_struct_field<handle1, 1>;
>   let var21: str = @call_indirect(fnptr13, captures13, op);
>   return var21;
> }
> 
> global main_handler: str = @call_direct(main_handler_thunk);
> 
> entry main_handler;

> cor-out +eval -print
> main_handler = [104 101 108 108 111]
>              > "hello"