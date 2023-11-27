# cor +solve -elab
# cor +ir -print
# cor +eval -print

# https://github.com/roc-lang/roc/issues/5464
Op a : [
    StdoutLine Str ({} -> Op a),
    StdinLine (Str -> Op a),
    Done a,
]

Task ok err op : ([ Ok ok, Err err ] -> op) -> op

sig succeed : ok -> Task ok * *
let succeed = \ok -> \toNext -> toNext (Ok ok);;

sig fail : err -> Task * err *
let fail = \err-> \toNext -> toNext (Err err);;

sig await : Task ok1 err op -> (ok1 -> Task ok2 err op) -> Task ok2 err op
let await = \fromResult -> \next ->
    \continue -> fromResult (\result ->
        let inner = when result is
            | Ok v -> next v
            | Err e -> fail e
        end
        in
        inner continue)
;;


sig outLine : Str -> Task {} * (Op *)
let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;

sig inLine : Task Str * (Op *)
let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;

sig main : Task {} * (Op *)
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
#       ^^
    let handle = \op -> \i -> \t -> when op is
#       ^^^^^^
        | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
        | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
        | Done x -> Done x t
    end
    in
    handle op 0 EntryPoint
;;

> cor-out +solve -elab
> 
> # https://github.com/roc-lang/roc/issues/5464
> Op a : [
>     StdoutLine Str ({} -> Op a),
>     StdinLine (Str -> Op a),
>     Done a,
> ]
> 
> Task ok err op : ([ Ok ok, Err err ] -> op) -> op
> 
> sig succeed : ok -> Task ok * *
> let succeed = \ok -> \toNext -> toNext (Ok ok);;
> 
> sig fail : err -> Task * err *
> let fail = \err-> \toNext -> toNext (Err err);;
> 
> sig await : Task ok1 err op -> (ok1 -> Task ok2 err op) -> Task ok2 err op
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
> sig outLine : Str -> Task {} * (Op *)
> let outLine = \s -> (\toNext -> StdoutLine s (\x -> toNext (Ok x)));;
> 
> sig inLine : Task Str * (Op *)
> let inLine = \toNext -> StdinLine (\s -> toNext (Ok s));;
> 
> sig main : Task {} * (Op *)
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
> #   ^^^^^^^^^^^^   Done [Err ?err, Ok {}]
> #   ^^^^^^^^^^^^     [
> #   ^^^^^^^^^^^^       EntryPoint,
> #   ^^^^^^^^^^^^       Stdin
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]?*>,
> #   ^^^^^^^^^^^^       Stdout Str
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]?*>
> #   ^^^^^^^^^^^^       ]?*
> #   ^^^^^^^^^^^^   ]?*
>     let op = main (\x -> Done x) in
> #       ^^ %Op [Err ?err, Ok {}]
>     let handle = \op -> \i -> \t -> when op is
> #       ^^^^^^ %(Op [Err ?err, Ok {}])
> #       ^^^^^^   -[handle]-> Int
> #       ^^^^^^                 -[lam19
> #       ^^^^^^                     <..(Op ..)
> #       ^^^^^^                          -[handle]-> Int
> #       ^^^^^^                                        -[
> #       ^^^^^^                                         lam19 ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]-> 
> #       ^^^^^^                                        [
> #       ^^^^^^                                         EntryPoint,
> #       ^^^^^^                                         Stdin ..,
> #       ^^^^^^                                         Stdout ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]?a
> #       ^^^^^^                                         -[
> #       ^^^^^^                                         lam18 ..
> #       ^^^^^^                                         .. ..
> #       ^^^^^^                                         ]-> 
> #       ^^^^^^                                         [
> #       ^^^^^^                                         Done ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]?*>
> #       ^^^^^^                     %(Op [Err ?err, Ok {}])]-> 
> #       ^^^^^^                 [
> #       ^^^^^^                   EntryPoint,
> #       ^^^^^^                   Stdin
> #       ^^^^^^                     <..[
> #       ^^^^^^                          EntryPoint,
> #       ^^^^^^                          Stdin ..,
> #       ^^^^^^                          Stdout .. ..
> #       ^^^^^^                          ]?a>,
> #       ^^^^^^                   Stdout Str
> #       ^^^^^^                     <..[
> #       ^^^^^^                          EntryPoint,
> #       ^^^^^^                          Stdin ..,
> #       ^^^^^^                          Stdout .. ..
> #       ^^^^^^                          ]?a>
> #       ^^^^^^                   ]?a
> #       ^^^^^^                   -[lam18
> #       ^^^^^^                       <..(Op ..)
> #       ^^^^^^                            -[handle]-> Int
> #       ^^^^^^                                         -[
> #       ^^^^^^                                         lam19 ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]-> 
> #       ^^^^^^                                         [
> #       ^^^^^^                                         EntryPoint,
> #       ^^^^^^                                         Stdin ..,
> #       ^^^^^^                                         Stdout ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]?a
> #       ^^^^^^                                         -[
> #       ^^^^^^                                         lam18 ..
> #       ^^^^^^                                         .. ..
> #       ^^^^^^                                         ]-> 
> #       ^^^^^^                                         [
> #       ^^^^^^                                         Done ..
> #       ^^^^^^                                         ..
> #       ^^^^^^                                         ]?*> Int
> #       ^^^^^^                       %(Op [Err ?err, Ok {}])]-> 
> #       ^^^^^^                   [
> #       ^^^^^^                     Done [Err ?err, Ok {}]
> #       ^^^^^^                       [
> #       ^^^^^^                         EntryPoint,
> #       ^^^^^^                         Stdin
> #       ^^^^^^                           <..[
> #       ^^^^^^                                EntryPoint,
> #       ^^^^^^                                Stdin ..,
> #       ^^^^^^                                Stdout .. ..
> #       ^^^^^^                                ]?a>,
> #       ^^^^^^                         Stdout Str
> #       ^^^^^^                           <..[
> #       ^^^^^^                                EntryPoint,
> #       ^^^^^^                                Stdin ..,
> #       ^^^^^^                                Stdout .. ..
> #       ^^^^^^                                ]?a>
> #       ^^^^^^                         ]?a
> #       ^^^^^^                     ]?*
>         | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
>         | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
>         | Done x -> Done x t
>     end
>     in
>     handle op 0 EntryPoint
> ;;
> 

> cor-out +ir -print
> global fail1: { *fn, box<erased> } = @call_direct(fail_thunk);
> 
> proc fail_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_>;
>   let fail_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   return fail_closure;
> }
> 
> proc clos_5(
>   captures_11: box<erased>,
>    toNext1: box<%type_8 = { *fn, box<erased> }>):
>   box<
>     %type_9 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box1: box<{ [] }> = @ptr_cast(captures_11 as box<{ [] }>);
>   let captures_stack1: { [] } = @get_boxed<captures_box1>;
>   let err: [] = @get_struct_field<captures_stack1, 0>;
>   let inner1: { *fn, box<erased> } = @get_boxed<toNext1>;
>   let fnptr: *fn = @get_struct_field<inner1, 0>;
>   let captures: box<erased> = @get_struct_field<inner1, 1>;
>   let struct: { [] } = @make_struct{ err };
>   let var1: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct>;
>   let var2:
>         box<
>           %type_9 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr, captures, var1);
>   return var2;
> }
> 
> proc clos_(captures_1: box<erased>, err: []):
>   box<%type_7 = { *fn, box<erased> }>
> {
>   let captures_box: box<{}> = @ptr_cast(captures_1 as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_5: { [] } = @make_struct{ err };
>   let captures_box_5: box<{ [] }> = @make_box(captures_stack_5);
>   let captures_10: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos_5>;
>   let unboxed: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_10 };
>   let var: box<%type_7 = { *fn, box<erased> }> = @make_box(unboxed);
>   return var;
> }
> 
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_2: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_1>;
>   let await_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_2 };
>   return await_closure;
> }
> 
> proc clos_8(captures_17: box<erased>, result: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box5:
>         box<{ box<%type_10 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_17 as
>         box<{ box<%type_10 = { *fn, box<erased> }>, { *fn, box<erased> } }>);
>   let captures_stack5:
>         { box<%type_10 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @get_boxed<captures_box5>;
>   let continue: box<%type_10 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack5, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack5, 1>;
>   let discr: int = @get_union_id<result>;
>   switch discr {
>   0 -> {
>     let payload1: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload1, 0>;
>     let fnptr3: *fn = @get_struct_field<fail1, 0>;
>     let captures3: box<erased> = @get_struct_field<fail1, 1>;
>     @call_indirect(fnptr3, captures3, e)
>   }
>   1 -> {
>     let payload: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload, 0>;
>     let fnptr2: *fn = @get_struct_field<next, 0>;
>     let captures2: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr2, captures2, v)
>   }
>   } in join join;
>   let inner: box<%type_4 = { *fn, box<erased> }> = join;
>   let inner2: { *fn, box<erased> } = @get_boxed<inner>;
>   let fnptr4: *fn = @get_struct_field<inner2, 0>;
>   let captures4: box<erased> = @get_struct_field<inner2, 1>;
>   let var7:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr4, captures4, continue);
>   return var7;
> }
> 
> proc clos_7(
>   captures_15: box<erased>,
>    continue: box<%type_10 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_15 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box4>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack4, 1>;
>   let fnptr1: *fn = @get_struct_field<fromResult, 0>;
>   let captures1: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_8:
>         { box<%type_10 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_8:
>         box<{ box<%type_10 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_16: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<clos_8>;
>   let unboxed1: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_16 };
>   let var5: box<%type_12 = { *fn, box<erased> }> = @make_box(unboxed1);
>   let var6:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr1, captures1, var5);
>   return var6;
> }
> 
> proc clos_6(captures_13: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box3: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_13 as box<{ { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> } } = @get_boxed<captures_box3>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let captures_stack_7: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_7: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_7);
>   let captures_14: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos_7>;
>   let var4: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_14 };
>   return var4;
> }
> 
> proc clos_1(captures_3: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box2: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack2: {} = @get_boxed<captures_box2>;
>   let captures_stack_6: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_6: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_6);
>   let captures_12: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos_6>;
>   let var3: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_12 };
>   return var3;
> }
> 
> global outLine1: { *fn, box<erased> } = @call_direct(outLine_thunk);
> 
> proc outLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_4: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_2>;
>   let outLine_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_4 };
>   return outLine_closure;
> }
> 
> proc clos_10(captures_21: box<erased>, x: {}):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box8: box<{ box<%type_12 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_21 as box<{ box<%type_12 = { *fn, box<erased> }> }>);
>   let captures_stack8: { box<%type_12 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box8>;
>   let toNext2: box<%type_12 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack8, 0>;
>   let inner3: { *fn, box<erased> } = @get_boxed<toNext2>;
>   let fnptr5: *fn = @get_struct_field<inner3, 0>;
>   let captures5: box<erased> = @get_struct_field<inner3, 1>;
>   let struct2: { {} } = @make_struct{ x };
>   let var11: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct2>;
>   let var12:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr5, captures5, var11);
>   return var12;
> }
> 
> proc clos_9(
>   captures_19: box<erased>,
>    toNext2: box<%type_12 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_19 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_10: { box<%type_12 = { *fn, box<erased> }> }
>     = @make_struct{ toNext2 };
>   let captures_box_10: box<{ box<%type_12 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_10);
>   let captures_20: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<clos_10>;
>   let var9: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_20 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s, var9 };
>   let unboxed2:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct1>;
>   let var10:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed2);
>   return var10;
> }
> 
> proc clos_2(captures_5: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_5 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_9: { str } = @make_struct{ s };
>   let captures_box_9: box<{ str }> = @make_box(captures_stack_9);
>   let captures_18: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<clos_9>;
>   let var8: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_18 };
>   return var8;
> }
> 
> global inLine1: { *fn, box<erased> } = @call_direct(inLine_thunk);
> 
> proc inLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_3: {} = @make_struct{};
>   let captures_box_3: box<{}> = @make_box(captures_stack_3);
>   let captures_6: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos_3>;
>   let inLine_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_3, captures_6 };
>   return inLine_closure;
> }
> 
> proc clos_11(captures_23: box<erased>, s1: str):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box10: box<{ box<%type_5 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_23 as box<{ box<%type_5 = { *fn, box<erased> }> }>);
>   let captures_stack10: { box<%type_5 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box10>;
>   let toNext3: box<%type_5 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack10, 0>;
>   let inner4: { *fn, box<erased> } = @get_boxed<toNext3>;
>   let fnptr6: *fn = @get_struct_field<inner4, 0>;
>   let captures6: box<erased> = @get_struct_field<inner4, 1>;
>   let struct4: { str } = @make_struct{ s1 };
>   let var15: [ `0 { [] }, `1 { str } ] = @make_union<1, struct4>;
>   let var16:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr6, captures6, var15);
>   return var16;
> }
> 
> proc clos_3(
>   captures_7: box<erased>,
>    toNext3: box<%type_5 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box9: box<{}> = @ptr_cast(captures_7 as box<{}>);
>   let captures_stack9: {} = @get_boxed<captures_box9>;
>   let captures_stack_11: { box<%type_5 = { *fn, box<erased> }> }
>     = @make_struct{ toNext3 };
>   let captures_box_11: box<{ box<%type_5 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_11);
>   let captures_22: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<clos_11>;
>   let var13: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_22 };
>   let struct3: { { *fn, box<erased> } } = @make_struct{ var13 };
>   let unboxed3:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct3>;
>   let var14:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed3);
>   return var14;
> }
> 
> global outLine2: { *fn, box<erased> } = @call_direct(outLine_thunk1);
> 
> proc outLine_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_4: {} = @make_struct{};
>   let captures_box_4: box<{}> = @make_box(captures_stack_4);
>   let captures_8: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos_4>;
>   let outLine_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_4, captures_8 };
>   return outLine_closure1;
> }
> 
> proc clos_13(captures_27: box<erased>, x: {}):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box13: box<{ box<%type_10 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_27 as box<{ box<%type_10 = { *fn, box<erased> }> }>);
>   let captures_stack13: { box<%type_10 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box13>;
>   let toNext2: box<%type_10 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack13, 0>;
>   let inner5: { *fn, box<erased> } = @get_boxed<toNext2>;
>   let fnptr7: *fn = @get_struct_field<inner5, 0>;
>   let captures7: box<erased> = @get_struct_field<inner5, 1>;
>   let struct6: { {} } = @make_struct{ x };
>   let var20: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct6>;
>   let var21:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr7, captures7, var20);
>   return var21;
> }
> 
> proc clos_12(
>   captures_25: box<erased>,
>    toNext2: box<%type_10 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box12: box<{ str }> = @ptr_cast(captures_25 as box<{ str }>);
>   let captures_stack12: { str } = @get_boxed<captures_box12>;
>   let s: str = @get_struct_field<captures_stack12, 0>;
>   let captures_stack_13: { box<%type_10 = { *fn, box<erased> }> }
>     = @make_struct{ toNext2 };
>   let captures_box_13: box<{ box<%type_10 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_13);
>   let captures_26: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<clos_13>;
>   let var18: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_26 };
>   let struct5: { str, { *fn, box<erased> } } = @make_struct{ s, var18 };
>   let unboxed5:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct5>;
>   let var19:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed5);
>   return var19;
> }
> 
> proc clos_4(captures_9: box<erased>, s: str):
>   box<%type_13 = { *fn, box<erased> }>
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_9 as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let captures_stack_12: { str } = @make_struct{ s };
>   let captures_box_12: box<{ str }> = @make_box(captures_stack_12);
>   let captures_24: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<clos_12>;
>   let unboxed4: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_24 };
>   let var17: box<%type_13 = { *fn, box<erased> }> = @make_box(unboxed4);
>   return var17;
> }
> 
> proc clos_17(captures_35: box<erased>, lastName: str):
>   box<%type_3 = { *fn, box<erased> }>
> {
>   let captures_box17: box<{ str }> = @ptr_cast(captures_35 as box<{ str }>);
>   let captures_stack17: { str } = @get_boxed<captures_box17>;
>   let firstName: str = @get_struct_field<captures_stack17, 0>;
>   let fnptr18: *fn = @get_struct_field<outLine2, 0>;
>   let captures18: box<erased> = @get_struct_field<outLine2, 1>;
>   let var38: str = "Hello ";
>   let var39: str = " ";
>   let var40: str = "!";
>   let var41: str
>     = @call_kfn(str_concat, var38, firstName, var39, lastName, var40);
>   let var42: box<%type_3 = { *fn, box<erased> }>
>     = @call_indirect(fnptr18, captures18, var41);
>   return var42;
> }
> 
> proc clos_16(captures_33: box<erased>, y: {}):
>   box<%type_14 = { *fn, box<erased> }>
> {
>   let captures_box16: box<{ str }> = @ptr_cast(captures_33 as box<{ str }>);
>   let captures_stack16: { str } = @get_boxed<captures_box16>;
>   let firstName: str = @get_struct_field<captures_stack16, 0>;
>   let fnptr16: *fn = @get_struct_field<await1, 0>;
>   let captures16: box<erased> = @get_struct_field<await1, 1>;
>   let var35: { *fn, box<erased> }
>     = @call_indirect(fnptr16, captures16, inLine1);
>   let fnptr17: *fn = @get_struct_field<var35, 0>;
>   let captures17: box<erased> = @get_struct_field<var35, 1>;
>   let captures_stack_17: { str } = @make_struct{ firstName };
>   let captures_box_17: box<{ str }> = @make_box(captures_stack_17);
>   let captures_34: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<clos_17>;
>   let var36: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_34 };
>   let var37: box<%type_14 = { *fn, box<erased> }>
>     = @call_indirect(fnptr17, captures17, var36);
>   return var37;
> }
> 
> proc clos_15(captures_31: box<erased>, firstName: str):
>   box<%type_3 = { *fn, box<erased> }>
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_31 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let fnptr13: *fn = @get_struct_field<await1, 0>;
>   let captures13: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr14: *fn = @get_struct_field<outLine1, 0>;
>   let captures14: box<erased> = @get_struct_field<outLine1, 1>;
>   let var30: str = "What's your last name?";
>   let var31: { *fn, box<erased> } = @call_indirect(fnptr14, captures14, var30);
>   let var32: { *fn, box<erased> } = @call_indirect(fnptr13, captures13, var31);
>   let fnptr15: *fn = @get_struct_field<var32, 0>;
>   let captures15: box<erased> = @get_struct_field<var32, 1>;
>   let captures_stack_16: { str } = @make_struct{ firstName };
>   let captures_box_16: box<{ str }> = @make_box(captures_stack_16);
>   let captures_32: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<clos_16>;
>   let var33: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_32 };
>   let var34: box<%type_3 = { *fn, box<erased> }>
>     = @call_indirect(fnptr15, captures15, var33);
>   return var34;
> }
> 
> proc clos_14(captures_29: box<erased>, x1: {}):
>   box<%type_14 = { *fn, box<erased> }>
> {
>   let captures_box14: box<{}> = @ptr_cast(captures_29 as box<{}>);
>   let captures_stack14: {} = @get_boxed<captures_box14>;
>   let fnptr11: *fn = @get_struct_field<await1, 0>;
>   let captures11: box<erased> = @get_struct_field<await1, 1>;
>   let var27: { *fn, box<erased> }
>     = @call_indirect(fnptr11, captures11, inLine1);
>   let fnptr12: *fn = @get_struct_field<var27, 0>;
>   let captures12: box<erased> = @get_struct_field<var27, 1>;
>   let captures_stack_15: {} = @make_struct{};
>   let captures_box_15: box<{}> = @make_box(captures_stack_15);
>   let captures_30: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<clos_15>;
>   let var28: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_30 };
>   let var29: box<%type_14 = { *fn, box<erased> }>
>     = @call_indirect(fnptr12, captures12, var28);
>   return var29;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr8: *fn = @get_struct_field<await1, 0>;
>   let captures8: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr9: *fn = @get_struct_field<outLine1, 0>;
>   let captures9: box<erased> = @get_struct_field<outLine1, 1>;
>   let var22: str = "What's your first name?";
>   let var23: { *fn, box<erased> } = @call_indirect(fnptr9, captures9, var22);
>   let var24: { *fn, box<erased> } = @call_indirect(fnptr8, captures8, var23);
>   let fnptr10: *fn = @get_struct_field<var24, 0>;
>   let captures10: box<erased> = @get_struct_field<var24, 1>;
>   let captures_stack_14: {} = @make_struct{};
>   let captures_box_14: box<{}> = @make_box(captures_stack_14);
>   let captures_28: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<clos_14>;
>   let var25: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_28 };
>   let var26: { *fn, box<erased> } = @call_indirect(fnptr10, captures10, var25);
>   return var26;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc clos_18(captures_37: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box18: box<{}> = @ptr_cast(captures_37 as box<{}>);
>   let captures_stack18: {} = @get_boxed<captures_box18>;
>   let struct8: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let unboxed8:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct8>;
>   let var49:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed8);
>   return var49;
> }
> 
> proc clos_20(
>   captures_42: box<erased>,
>    t:
>      box<%type_15 = [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>):
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_15 =
>             [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>          ,
>         }
>   ]
> {
>   let captures_box21:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_42 as
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack21:
>         {
>          { *fn, box<erased> },
>           int,
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box21>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack21, 0>;
>   let i: int = @get_struct_field<captures_stack21, 1>;
>   let op1:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack21, 2>;
>   let inner6:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_boxed<op1>;
>   let discr1: int = @get_union_id<inner6>;
>   switch discr1 {
>   0 -> {
>     let payload4: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<inner6>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload4, 0>;
>     let struct11:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_15 =
>               [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct11>
>   }
>   1 -> {
>     let payload2: { { *fn, box<erased> } } = @get_union_struct<inner6>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload2, 0>;
>     let fnptr23: *fn = @get_struct_field<handle, 0>;
>     let captures23: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr24: *fn = @get_struct_field<f, 0>;
>     let captures24: box<erased> = @get_struct_field<f, 1>;
>     let var52: str = "stdin";
>     let var53: str = @call_kfn(itos, i);
>     let var54: str = @call_kfn(str_concat, var52, var53);
>     let var55:
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr24, captures24, var54);
>     let var56: { *fn, box<erased> }
>       = @call_indirect(fnptr23, captures23, var55);
>     let fnptr25: *fn = @get_struct_field<var56, 0>;
>     let captures25: box<erased> = @get_struct_field<var56, 1>;
>     let var57: int = 1;
>     let var58: int = @call_kfn(add, i, var57);
>     let var59: { *fn, box<erased> }
>       = @call_indirect(fnptr25, captures25, var58);
>     let fnptr26: *fn = @get_struct_field<var59, 0>;
>     let captures26: box<erased> = @get_struct_field<var59, 1>;
>     let struct9:
>           {
>            box<
>              %type_15 =
>              [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let unboxed9:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_15 =
>                    [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_15> }
>           ]
>       = @make_union<1, struct9>;
>     let var60:
>           box<
>             %type_15 =
>             [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>       = @make_box(unboxed9);
>     @call_indirect(fnptr26, captures26, var60)
>   }
>   2 -> {
>     let payload3: { str, { *fn, box<erased> } } = @get_union_struct<inner6>;
>     let s2: str = @get_struct_field<payload3, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload3, 1>;
>     let fnptr27: *fn = @get_struct_field<handle, 0>;
>     let captures27: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr28: *fn = @get_struct_field<f1, 0>;
>     let captures28: box<erased> = @get_struct_field<f1, 1>;
>     let var61: {} = @make_struct{};
>     let var62:
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr28, captures28, var61);
>     let var63: { *fn, box<erased> }
>       = @call_indirect(fnptr27, captures27, var62);
>     let fnptr29: *fn = @get_struct_field<var63, 0>;
>     let captures29: box<erased> = @get_struct_field<var63, 1>;
>     let var64: int = 1;
>     let var65: int = @call_kfn(add, i, var64);
>     let var66: { *fn, box<erased> }
>       = @call_indirect(fnptr29, captures29, var65);
>     let fnptr30: *fn = @get_struct_field<var66, 0>;
>     let captures30: box<erased> = @get_struct_field<var66, 1>;
>     let struct10:
>           {
>            str,
>             box<
>               %type_15 =
>               [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let unboxed10:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_15 =
>                    [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_15> }
>           ]
>       = @make_union<2, struct10>;
>     let var67:
>           box<
>             %type_15 =
>             [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>       = @make_box(unboxed10);
>     @call_indirect(fnptr30, captures30, var67)
>   }
>   } in join join1;
>   return join1;
> }
> 
> proc clos_19(captures_40: box<erased>, i: int): { *fn, box<erased> }
> {
>   let captures_box20:
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_40 as
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack20:
>         {
>          { *fn, box<erased> },
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box20>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack20, 0>;
>   let op1:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack20, 1>;
>   let captures_stack_21:
>         {
>          { *fn, box<erased> },
>           int,
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle, i, op1 };
>   let captures_box_21:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_21);
>   let captures_41: box<erased> = @ptr_cast(captures_box_21 as box<erased>);
>   let fn_ptr_21: *fn = @make_fn_ptr<clos_20>;
>   let var51: { *fn, box<erased> } = @make_struct{ fn_ptr_21, captures_41 };
>   return var51;
> }
> 
> proc clos_handle(
>   captures_handle: box<erased>,
>    op1:
>      box<
>        %type_1 =
>        [
>           `0 { [ `0 { [] }, `1 { {} } ] },
>           `1 { { *fn, box<erased> } },
>           `2 { str, { *fn, box<erased> } }
>        ]>):
>   { *fn, box<erased> }
> {
>   let captures_box19: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack19: {} = @get_boxed<captures_box19>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let captures_stack_20:
>         {
>          { *fn, box<erased> },
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle, op1 };
>   let captures_box_20:
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_1 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_20);
>   let captures_39: box<erased> = @ptr_cast(captures_box_20 as box<erased>);
>   let fn_ptr_20: *fn = @make_fn_ptr<clos_19>;
>   let var50: { *fn, box<erased> } = @make_struct{ fn_ptr_20, captures_39 };
>   return var50;
> }
> 
> proc main_handler_thunk():
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_15 =
>             [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>          ,
>         }
>   ]
> {
>   let fnptr19: *fn = @get_struct_field<main1, 0>;
>   let captures19: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_18: {} = @make_struct{};
>   let captures_box_18: box<{}> = @make_box(captures_stack_18);
>   let captures_36: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<clos_18>;
>   let unboxed6: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_36 };
>   let var43: box<%type_0 = { *fn, box<erased> }> = @make_box(unboxed6);
>   let op:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr19, captures19, var43);
>   let captures_stack_19: {} = @make_struct{};
>   let captures_box_19: box<{}> = @make_box(captures_stack_19);
>   let captures_38: box<erased> = @ptr_cast(captures_box_19 as box<erased>);
>   let fn_ptr_19: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> } = @make_struct{ fn_ptr_19, captures_38 };
>   let fnptr20: *fn = @get_struct_field<handle, 0>;
>   let captures20: box<erased> = @get_struct_field<handle, 1>;
>   let var44: { *fn, box<erased> } = @call_indirect(fnptr20, captures20, op);
>   let fnptr21: *fn = @get_struct_field<var44, 0>;
>   let captures21: box<erased> = @get_struct_field<var44, 1>;
>   let var45: int = 0;
>   let var46: { *fn, box<erased> } = @call_indirect(fnptr21, captures21, var45);
>   let fnptr22: *fn = @get_struct_field<var46, 0>;
>   let captures22: box<erased> = @get_struct_field<var46, 1>;
>   let struct7: {} = @make_struct{};
>   let unboxed7:
>         [
>            `0 {},
>            `1 {
>                box<
>                  %type_15 =
>                  [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>                ,
>               },
>            `2 { str, box<%type_15> }
>         ]
>     = @make_union<0, struct7>;
>   let var47:
>         box<
>           %type_15 =
>           [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>     = @make_box(unboxed7);
>   let var48:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_15 =
>                   [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr22, captures22, var47);
>   return var48;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_6 =
>             [ `0 {}, `1 { box<%type_6> }, `2 { str, box<%type_6> } ]>
>          ,
>         }
>   ]
>   = @call_direct(main_handler_thunk);
> 
> entry main_handler;

> cor-out +eval -print
> main_handler = [0 [1 []]
>                [2
>                [72 101 108 108 111 32 115 116 100
>                105 110 49 32 115 116 100 105 110
>                51 33]
>                [1
>                [2
>                [87 104 97 116 39 115 32 121 111
>                117 114 32 108 97 115 116 32 110
>                97 109 101 63]
>                [1
>                [2
>                [87 104 97 116 39 115 32 121 111
>                117 114 32 102 105 114 115 116 32
>                110 97 109 101 63] [0]]]]]]]
>              > Done (Ok {})
>                  (Stdout "Hello stdin1 stdin3!"
>                     (Stdin
>                        (Stdout
>                           "What's your last name?"
>                           (Stdin
>                              (Stdout
>                                 "What's your first name?"
>                                 (EntryPoint ))))))