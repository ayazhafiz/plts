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
> #                              ^^^^^^^ Str -> Task Str %(Op 'a)
> #                         ^ Str
> #          ^^^^^ (Task Str %(Op 'a))
> #          ^^^^^   -> (Str -> Task Str %(Op 'a))
> #          ^^^^^        -> Task Str %(Op 'a)
> ;;
> 
> run main_handler =
> #   ^^^^^^^^^^^^ Str
>     let op = main (\x -> Done x) in
> #       ^^ %Op Str
>     let handle = \op -> when op is
> #       ^^^^^^ %(Op Str) -> Str
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
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_>;
>   let await_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   return await_closure;
> }
> 
> proc clos_5(captures_11: box<erased>, result: str):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_11 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box3>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack3, 1>;
>   let fnptr1: *fn = @get_struct_field<next, 0>;
>   let captures1: box<erased> = @get_struct_field<next, 1>;
>   let inner: { *fn, box<erased> } = @call_indirect(fnptr1, captures1, result);
>   let fnptr2: *fn = @get_struct_field<inner, 0>;
>   let captures2: box<erased> = @get_struct_field<inner, 1>;
>   let var4:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr2, captures2, continue);
>   return var4;
> }
> 
> proc clos_4(captures_9: box<erased>, continue: { *fn, box<erased> }):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box2: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_9 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box2>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack2, 1>;
>   let fnptr: *fn = @get_struct_field<fromResult, 0>;
>   let captures: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_5: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_5: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_5);
>   let captures_10: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos_5>;
>   let var2: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_10 };
>   let var3:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr, captures, var2);
>   return var3;
> }
> 
> proc clos_3(captures_7: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box1: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_7 as box<{ { *fn, box<erased> } }>);
>   let captures_stack1: { { *fn, box<erased> } } = @get_boxed<captures_box1>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack1, 0>;
>   let captures_stack_4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_4);
>   let captures_8: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos_4>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_8 };
>   return var1;
> }
> 
> proc clos_(captures_1: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{}> = @ptr_cast(captures_1 as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_3: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_3: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_3);
>   let captures_6: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos_3>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_3, captures_6 };
>   return var;
> }
> 
> global lineIn1: { *fn, box<erased> } = @call_direct(lineIn_thunk);
> 
> proc lineIn_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_2: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_1>;
>   let lineIn_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_2 };
>   return lineIn_closure;
> }
> 
> proc clos_6(captures_13: box<erased>, s: str):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box5: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_13 as box<{ { *fn, box<erased> } }>);
>   let captures_stack5: { { *fn, box<erased> } } = @get_boxed<captures_box5>;
>   let toNext: { *fn, box<erased> } = @get_struct_field<captures_stack5, 0>;
>   let fnptr3: *fn = @get_struct_field<toNext, 0>;
>   let captures3: box<erased> = @get_struct_field<toNext, 1>;
>   let var7:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr3, captures3, s);
>   return var7;
> }
> 
> proc clos_1(captures_3: box<erased>, toNext: { *fn, box<erased> }):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box4: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack4: {} = @get_boxed<captures_box4>;
>   let captures_stack_6: { { *fn, box<erased> } } = @make_struct{ toNext };
>   let captures_box_6: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_6);
>   let captures_12: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos_6>;
>   let var5: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_12 };
>   let struct: { { *fn, box<erased> } } = @make_struct{ var5 };
>   let unboxed:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct>;
>   let var6:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed);
>   return var6;
> }
> 
> global lineOut1: { *fn, box<erased> } = @call_direct(lineOut_thunk);
> 
> proc lineOut_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_4: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_2>;
>   let lineOut_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_4 };
>   return lineOut_closure;
> }
> 
> proc clos_8(captures_17: box<erased>, x: str):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box8: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_17 as box<{ { *fn, box<erased> } }>);
>   let captures_stack8: { { *fn, box<erased> } } = @get_boxed<captures_box8>;
>   let toNext1: { *fn, box<erased> } = @get_struct_field<captures_stack8, 0>;
>   let fnptr4: *fn = @get_struct_field<toNext1, 0>;
>   let captures4: box<erased> = @get_struct_field<toNext1, 1>;
>   let var11:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr4, captures4, x);
>   return var11;
> }
> 
> proc clos_7(captures_15: box<erased>, toNext1: { *fn, box<erased> }):
>   box<
>     %type_2 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_15 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s1: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_8: { { *fn, box<erased> } } = @make_struct{ toNext1 };
>   let captures_box_8: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_16: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<clos_8>;
>   let var9: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_16 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s1, var9 };
>   let unboxed1:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct1>;
>   let var10:
>         box<
>           %type_2 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed1);
>   return var10;
> }
> 
> proc clos_2(captures_5: box<erased>, s1: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_5 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_7: { str } = @make_struct{ s1 };
>   let captures_box_7: box<{ str }> = @make_box(captures_stack_7);
>   let captures_14: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos_7>;
>   let var8: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_14 };
>   return var8;
> }
> 
> proc clos_9(captures_19: box<erased>, s2: str): { *fn, box<erased> }
> {
>   let captures_box9: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_19 as box<{ { *fn, box<erased> } }>);
>   let captures_stack9: { { *fn, box<erased> } } = @get_boxed<captures_box9>;
>   let lineOut1: { *fn, box<erased> } = @get_struct_field<captures_stack9, 0>;
>   let fnptr7: *fn = @get_struct_field<lineOut1, 0>;
>   let captures7: box<erased> = @get_struct_field<lineOut1, 1>;
>   let var15: { *fn, box<erased> } = @call_indirect(fnptr7, captures7, s2);
>   return var15;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr5: *fn = @get_struct_field<await1, 0>;
>   let captures5: box<erased> = @get_struct_field<await1, 1>;
>   let var12: { *fn, box<erased> } = @call_indirect(fnptr5, captures5, lineIn1);
>   let fnptr6: *fn = @get_struct_field<var12, 0>;
>   let captures6: box<erased> = @get_struct_field<var12, 1>;
>   let captures_stack_9: { { *fn, box<erased> } } = @make_struct{ lineOut1 };
>   let captures_box_9: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_9);
>   let captures_18: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<clos_9>;
>   let var13: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_18 };
>   let var14: { *fn, box<erased> } = @call_indirect(fnptr6, captures6, var13);
>   return var14;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc clos_10(captures_21: box<erased>, x1: str):
>   box<
>     %type_0 =
>     [ `0 { str }, `1 { { *fn, box<erased> } }, `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box10: box<{}> = @ptr_cast(captures_21 as box<{}>);
>   let captures_stack10: {} = @get_boxed<captures_box10>;
>   let struct2: { str } = @make_struct{ x1 };
>   let unboxed2:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct2>;
>   let var18:
>         box<
>           %type_0 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed2);
>   return var18;
> }
> 
> proc clos_handle(
>   captures_handle: box<erased>,
>    op1:
>      box<
>        %type_0 =
>        [
>           `0 { str },
>           `1 { { *fn, box<erased> } },
>           `2 { str, { *fn, box<erased> } }
>        ]>):
>   str
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let inner1:
>         [
>            `0 { str },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_boxed<op1>;
>   let discr: int = @get_union_id<inner1>;
>   switch discr {
>   0 -> {
>     let payload2: { str } = @get_union_struct<inner1>;
>     let x2: str = @get_struct_field<payload2, 0>;
>     x2
>   }
>   1 -> {
>     let payload: { { *fn, box<erased> } } = @get_union_struct<inner1>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload, 0>;
>     let fnptr10: *fn = @get_struct_field<handle, 0>;
>     let captures10: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr11: *fn = @get_struct_field<f, 0>;
>     let captures11: box<erased> = @get_struct_field<f, 1>;
>     let var19: str = "hello";
>     let var20:
>           box<
>             %type_0 =
>             [
>                `0 { str },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr11, captures11, var19);
>     @call_indirect(fnptr10, captures10, var20)
>   }
>   2 -> {
>     let payload1: { str, { *fn, box<erased> } } = @get_union_struct<inner1>;
>     let s3: str = @get_struct_field<payload1, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload1, 1>;
>     let fnptr12: *fn = @get_struct_field<handle, 0>;
>     let captures12: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr13: *fn = @get_struct_field<f1, 0>;
>     let captures13: box<erased> = @get_struct_field<f1, 1>;
>     let var21:
>           box<
>             %type_0 =
>             [
>                `0 { str },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr13, captures13, s3);
>     @call_indirect(fnptr12, captures12, var21)
>   }
>   } in join join;
>   return join;
> }
> 
> proc main_handler_thunk(): str
> {
>   let fnptr8: *fn = @get_struct_field<main1, 0>;
>   let captures8: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_10: {} = @make_struct{};
>   let captures_box_10: box<{}> = @make_box(captures_stack_10);
>   let captures_20: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<clos_10>;
>   let var16: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_20 };
>   let op:
>         box<
>           %type_0 =
>           [
>              `0 { str },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr8, captures8, var16);
>   let captures_stack_11: {} = @make_struct{};
>   let captures_box_11: box<{}> = @make_box(captures_stack_11);
>   let captures_22: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_22 };
>   let fnptr9: *fn = @get_struct_field<handle, 0>;
>   let captures9: box<erased> = @get_struct_field<handle, 1>;
>   let var17: str = @call_indirect(fnptr9, captures9, op);
>   return var17;
> }
> 
> global main_handler: str = @call_direct(main_handler_thunk);
> 
> entry main_handler;

> cor-out +eval -print
> main_handler = [104 101 108 108 111]
>              > "hello"