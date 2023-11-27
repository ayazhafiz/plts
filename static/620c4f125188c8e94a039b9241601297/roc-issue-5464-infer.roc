# cor +solve -elab
# cor +ir -print
# cor +eval -print

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
> proc clos_4(
>   captures_9: box<erased>,
>    toNext1: box<%type_6 = { *fn, box<erased> }>):
>   box<
>     %type_8 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_7 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box1: box<{ [] }> = @ptr_cast(captures_9 as box<{ [] }>);
>   let captures_stack1: { [] } = @get_boxed<captures_box1>;
>   let err: [] = @get_struct_field<captures_stack1, 0>;
>   let inner1: { *fn, box<erased> } = @get_boxed<toNext1>;
>   let fnptr: *fn = @get_struct_field<inner1, 0>;
>   let captures: box<erased> = @get_struct_field<inner1, 1>;
>   let struct: { [] } = @make_struct{ err };
>   let var1: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct>;
>   let var2:
>         box<
>           %type_8 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_7 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr, captures, var1);
>   return var2;
> }
> 
> proc clos_(captures_1: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box: box<{}> = @ptr_cast(captures_1 as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_4: { [] } = @make_struct{ err };
>   let captures_box_4: box<{ [] }> = @make_box(captures_stack_4);
>   let captures_8: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos_4>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_8 };
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
> proc clos_7(captures_15: box<erased>, result: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_12 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box5:
>         box<{ box<%type_9 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_15 as
>         box<{ box<%type_9 = { *fn, box<erased> }>, { *fn, box<erased> } }>);
>   let captures_stack5:
>         { box<%type_9 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @get_boxed<captures_box5>;
>   let continue: box<%type_9 = { *fn, box<erased> }>
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
>   let inner: { *fn, box<erased> } = join;
>   let fnptr4: *fn = @get_struct_field<inner, 0>;
>   let captures4: box<erased> = @get_struct_field<inner, 1>;
>   let var7:
>         box<
>           %type_12 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr4, captures4, continue);
>   return var7;
> }
> 
> proc clos_6(
>   captures_13: box<erased>,
>    continue: box<%type_9 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_13 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box4>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack4, 1>;
>   let fnptr1: *fn = @get_struct_field<fromResult, 0>;
>   let captures1: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_7:
>         { box<%type_9 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_7:
>         box<{ box<%type_9 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @make_box(captures_stack_7);
>   let captures_14: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos_7>;
>   let unboxed: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_14 };
>   let var5: box<%type_9 = { *fn, box<erased> }> = @make_box(unboxed);
>   let var6:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr1, captures1, var5);
>   return var6;
> }
> 
> proc clos_5(captures_11: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box3: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_11 as box<{ { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> } } = @get_boxed<captures_box3>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let captures_stack_6: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_6: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_6);
>   let captures_12: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos_6>;
>   let var4: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_12 };
>   return var4;
> }
> 
> proc clos_1(captures_3: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box2: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack2: {} = @get_boxed<captures_box2>;
>   let captures_stack_5: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_5: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_5);
>   let captures_10: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos_5>;
>   let var3: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_10 };
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
> proc clos_9(captures_19: box<erased>, x: {}):
>   box<
>     %type_12 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box8: box<{ box<%type_9 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_19 as box<{ box<%type_9 = { *fn, box<erased> }> }>);
>   let captures_stack8: { box<%type_9 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box8>;
>   let toNext2: box<%type_9 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack8, 0>;
>   let inner2: { *fn, box<erased> } = @get_boxed<toNext2>;
>   let fnptr5: *fn = @get_struct_field<inner2, 0>;
>   let captures5: box<erased> = @get_struct_field<inner2, 1>;
>   let struct2: { {} } = @make_struct{ x };
>   let var11: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct2>;
>   let var12:
>         box<
>           %type_12 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr5, captures5, var11);
>   return var12;
> }
> 
> proc clos_8(
>   captures_17: box<erased>,
>    toNext2: box<%type_9 = { *fn, box<erased> }>):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_17 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_9: { box<%type_9 = { *fn, box<erased> }> }
>     = @make_struct{ toNext2 };
>   let captures_box_9: box<{ box<%type_9 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_9);
>   let captures_18: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<clos_9>;
>   let unboxed1: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_18 };
>   let var9: box<%type_10 = { *fn, box<erased> }> = @make_box(unboxed1);
>   let struct1: { str, box<%type_10 = { *fn, box<erased> }> }
>     = @make_struct{ s, var9 };
>   let unboxed2:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, box<%type_10 = { *fn, box<erased> }> }
>         ]
>     = @make_union<2, struct1>;
>   let var10:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @make_box(unboxed2);
>   return var10;
> }
> 
> proc clos_2(captures_5: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_5 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_8: { str } = @make_struct{ s };
>   let captures_box_8: box<{ str }> = @make_box(captures_stack_8);
>   let captures_16: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<clos_8>;
>   let var8: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_16 };
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
> proc clos_10(captures_21: box<erased>, s1: str):
>   box<
>     %type_12 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box10: box<{ box<%type_4 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_21 as box<{ box<%type_4 = { *fn, box<erased> }> }>);
>   let captures_stack10: { box<%type_4 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box10>;
>   let toNext3: box<%type_4 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack10, 0>;
>   let inner3: { *fn, box<erased> } = @get_boxed<toNext3>;
>   let fnptr6: *fn = @get_struct_field<inner3, 0>;
>   let captures6: box<erased> = @get_struct_field<inner3, 1>;
>   let struct4: { str } = @make_struct{ s1 };
>   let var15: [ `0 { [] }, `1 { str } ] = @make_union<1, struct4>;
>   let var16:
>         box<
>           %type_12 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr6, captures6, var15);
>   return var16;
> }
> 
> proc clos_3(
>   captures_7: box<erased>,
>    toNext3: box<%type_4 = { *fn, box<erased> }>):
>   box<
>     %type_12 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_10 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box9: box<{}> = @ptr_cast(captures_7 as box<{}>);
>   let captures_stack9: {} = @get_boxed<captures_box9>;
>   let captures_stack_10: { box<%type_4 = { *fn, box<erased> }> }
>     = @make_struct{ toNext3 };
>   let captures_box_10: box<{ box<%type_4 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_10);
>   let captures_20: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<clos_10>;
>   let var13: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_20 };
>   let struct3: { { *fn, box<erased> } } = @make_struct{ var13 };
>   let unboxed3:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, box<%type_10 = { *fn, box<erased> }> }
>         ]
>     = @make_union<1, struct3>;
>   let var14:
>         box<
>           %type_12 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_10 = { *fn, box<erased> }> }
>           ]>
>     = @make_box(unboxed3);
>   return var14;
> }
> 
> proc clos_14(captures_29: box<erased>, lastName: str): { *fn, box<erased> }
> {
>   let captures_box14: box<{ str }> = @ptr_cast(captures_29 as box<{ str }>);
>   let captures_stack14: { str } = @get_boxed<captures_box14>;
>   let firstName: str = @get_struct_field<captures_stack14, 0>;
>   let fnptr17: *fn = @get_struct_field<outLine1, 0>;
>   let captures17: box<erased> = @get_struct_field<outLine1, 1>;
>   let var33: str = "Hello ";
>   let var34: str = " ";
>   let var35: str = "!";
>   let var36: str
>     = @call_kfn(str_concat, var33, firstName, var34, lastName, var35);
>   let var37: { *fn, box<erased> } = @call_indirect(fnptr17, captures17, var36);
>   return var37;
> }
> 
> proc clos_13(captures_27: box<erased>, y: {}): { *fn, box<erased> }
> {
>   let captures_box13: box<{ str }> = @ptr_cast(captures_27 as box<{ str }>);
>   let captures_stack13: { str } = @get_boxed<captures_box13>;
>   let firstName: str = @get_struct_field<captures_stack13, 0>;
>   let fnptr15: *fn = @get_struct_field<await1, 0>;
>   let captures15: box<erased> = @get_struct_field<await1, 1>;
>   let var30: { *fn, box<erased> }
>     = @call_indirect(fnptr15, captures15, inLine1);
>   let fnptr16: *fn = @get_struct_field<var30, 0>;
>   let captures16: box<erased> = @get_struct_field<var30, 1>;
>   let captures_stack_14: { str } = @make_struct{ firstName };
>   let captures_box_14: box<{ str }> = @make_box(captures_stack_14);
>   let captures_28: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<clos_14>;
>   let unboxed5: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_28 };
>   let var31: box<%type_13 = { *fn, box<erased> }> = @make_box(unboxed5);
>   let var32: { *fn, box<erased> } = @call_indirect(fnptr16, captures16, var31);
>   return var32;
> }
> 
> proc clos_12(captures_25: box<erased>, firstName: str): { *fn, box<erased> }
> {
>   let captures_box12: box<{}> = @ptr_cast(captures_25 as box<{}>);
>   let captures_stack12: {} = @get_boxed<captures_box12>;
>   let fnptr12: *fn = @get_struct_field<await1, 0>;
>   let captures12: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr13: *fn = @get_struct_field<outLine1, 0>;
>   let captures13: box<erased> = @get_struct_field<outLine1, 1>;
>   let var25: str = "What's your last name?";
>   let var26: { *fn, box<erased> } = @call_indirect(fnptr13, captures13, var25);
>   let var27: { *fn, box<erased> } = @call_indirect(fnptr12, captures12, var26);
>   let fnptr14: *fn = @get_struct_field<var27, 0>;
>   let captures14: box<erased> = @get_struct_field<var27, 1>;
>   let captures_stack_13: { str } = @make_struct{ firstName };
>   let captures_box_13: box<{ str }> = @make_box(captures_stack_13);
>   let captures_26: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<clos_13>;
>   let var28: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_26 };
>   let var29: { *fn, box<erased> } = @call_indirect(fnptr14, captures14, var28);
>   return var29;
> }
> 
> proc clos_11(captures_23: box<erased>, x1: {}): { *fn, box<erased> }
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_23 as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let fnptr10: *fn = @get_struct_field<await1, 0>;
>   let captures10: box<erased> = @get_struct_field<await1, 1>;
>   let var22: { *fn, box<erased> }
>     = @call_indirect(fnptr10, captures10, inLine1);
>   let fnptr11: *fn = @get_struct_field<var22, 0>;
>   let captures11: box<erased> = @get_struct_field<var22, 1>;
>   let captures_stack_12: {} = @make_struct{};
>   let captures_box_12: box<{}> = @make_box(captures_stack_12);
>   let captures_24: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<clos_12>;
>   let unboxed4: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_24 };
>   let var23: box<%type_13 = { *fn, box<erased> }> = @make_box(unboxed4);
>   let var24: { *fn, box<erased> } = @call_indirect(fnptr11, captures11, var23);
>   return var24;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr7: *fn = @get_struct_field<await1, 0>;
>   let captures7: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr8: *fn = @get_struct_field<outLine1, 0>;
>   let captures8: box<erased> = @get_struct_field<outLine1, 1>;
>   let var17: str = "What's your first name?";
>   let var18: { *fn, box<erased> } = @call_indirect(fnptr8, captures8, var17);
>   let var19: { *fn, box<erased> } = @call_indirect(fnptr7, captures7, var18);
>   let fnptr9: *fn = @get_struct_field<var19, 0>;
>   let captures9: box<erased> = @get_struct_field<var19, 1>;
>   let captures_stack_11: {} = @make_struct{};
>   let captures_box_11: box<{}> = @make_box(captures_stack_11);
>   let captures_22: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<clos_11>;
>   let var20: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_22 };
>   let var21: { *fn, box<erased> } = @call_indirect(fnptr9, captures9, var20);
>   return var21;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc clos_15(captures_31: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_15 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_31 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let struct6: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let unboxed7:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct6>;
>   let var44:
>         box<
>           %type_15 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed7);
>   return var44;
> }
> 
> proc clos_17(
>   captures_36: box<erased>,
>    t:
>      box<%type_14 = [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>):
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>          ,
>         }
>   ]
> {
>   let captures_box18:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_36 as
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack18:
>         {
>          { *fn, box<erased> },
>           int,
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box18>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack18, 0>;
>   let i: int = @get_struct_field<captures_stack18, 1>;
>   let op1:
>         box<
>           %type_0 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack18, 2>;
>   let inner4:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_boxed<op1>;
>   let discr1: int = @get_union_id<inner4>;
>   switch discr1 {
>   0 -> {
>     let payload4: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<inner4>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload4, 0>;
>     let struct9:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_14 =
>               [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct9>
>   }
>   1 -> {
>     let payload2: { { *fn, box<erased> } } = @get_union_struct<inner4>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload2, 0>;
>     let fnptr22: *fn = @get_struct_field<handle, 0>;
>     let captures22: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr23: *fn = @get_struct_field<f, 0>;
>     let captures23: box<erased> = @get_struct_field<f, 1>;
>     let var47: str = "stdin";
>     let var48: str = @call_kfn(itos, i);
>     let var49: str = @call_kfn(str_concat, var47, var48);
>     let var50:
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr23, captures23, var49);
>     let var51: { *fn, box<erased> }
>       = @call_indirect(fnptr22, captures22, var50);
>     let fnptr24: *fn = @get_struct_field<var51, 0>;
>     let captures24: box<erased> = @get_struct_field<var51, 1>;
>     let var52: int = 1;
>     let var53: int = @call_kfn(add, i, var52);
>     let var54: { *fn, box<erased> }
>       = @call_indirect(fnptr24, captures24, var53);
>     let fnptr25: *fn = @get_struct_field<var54, 0>;
>     let captures25: box<erased> = @get_struct_field<var54, 1>;
>     let struct7:
>           {
>            box<
>              %type_14 =
>              [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let unboxed8:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_14 =
>                    [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_14> }
>           ]
>       = @make_union<1, struct7>;
>     let var55:
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>       = @make_box(unboxed8);
>     @call_indirect(fnptr25, captures25, var55)
>   }
>   2 -> {
>     let payload3: { str, { *fn, box<erased> } } = @get_union_struct<inner4>;
>     let s2: str = @get_struct_field<payload3, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload3, 1>;
>     let fnptr26: *fn = @get_struct_field<handle, 0>;
>     let captures26: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr27: *fn = @get_struct_field<f1, 0>;
>     let captures27: box<erased> = @get_struct_field<f1, 1>;
>     let var56: {} = @make_struct{};
>     let var57:
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr27, captures27, var56);
>     let var58: { *fn, box<erased> }
>       = @call_indirect(fnptr26, captures26, var57);
>     let fnptr28: *fn = @get_struct_field<var58, 0>;
>     let captures28: box<erased> = @get_struct_field<var58, 1>;
>     let var59: int = 1;
>     let var60: int = @call_kfn(add, i, var59);
>     let var61: { *fn, box<erased> }
>       = @call_indirect(fnptr28, captures28, var60);
>     let fnptr29: *fn = @get_struct_field<var61, 0>;
>     let captures29: box<erased> = @get_struct_field<var61, 1>;
>     let struct8:
>           {
>            str,
>             box<
>               %type_14 =
>               [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let unboxed9:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_14 =
>                    [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_14> }
>           ]
>       = @make_union<2, struct8>;
>     let var62:
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>       = @make_box(unboxed9);
>     @call_indirect(fnptr29, captures29, var62)
>   }
>   } in join join1;
>   return join1;
> }
> 
> proc clos_16(captures_34: box<erased>, i: int): { *fn, box<erased> }
> {
>   let captures_box17:
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_34 as
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack17:
>         {
>          { *fn, box<erased> },
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box17>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack17, 0>;
>   let op1:
>         box<
>           %type_0 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack17, 1>;
>   let captures_stack_18:
>         {
>          { *fn, box<erased> },
>           int,
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle, i, op1 };
>   let captures_box_18:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_18);
>   let captures_35: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<clos_17>;
>   let var46: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_35 };
>   return var46;
> }
> 
> proc clos_handle(
>   captures_handle: box<erased>,
>    op1:
>      box<
>        %type_0 =
>        [
>           `0 { [ `0 { [] }, `1 { {} } ] },
>           `1 { { *fn, box<erased> } },
>           `2 { str, { *fn, box<erased> } }
>        ]>):
>   { *fn, box<erased> }
> {
>   let captures_box16: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack16: {} = @get_boxed<captures_box16>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let captures_stack_17:
>         {
>          { *fn, box<erased> },
>           box<
>             %type_0 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle, op1 };
>   let captures_box_17:
>         box<
>           {
>            { *fn, box<erased> },
>             box<
>               %type_0 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_17);
>   let captures_33: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<clos_16>;
>   let var45: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_33 };
>   return var45;
> }
> 
> proc main_handler_thunk():
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>          ,
>         }
>   ]
> {
>   let fnptr18: *fn = @get_struct_field<main1, 0>;
>   let captures18: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_15: {} = @make_struct{};
>   let captures_box_15: box<{}> = @make_box(captures_stack_15);
>   let captures_30: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<clos_15>;
>   let var38: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_30 };
>   let op:
>         box<
>           %type_0 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr18, captures18, var38);
>   let captures_stack_16: {} = @make_struct{};
>   let captures_box_16: box<{}> = @make_box(captures_stack_16);
>   let captures_32: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<clos_handle>;
>   let handle: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_32 };
>   let fnptr19: *fn = @get_struct_field<handle, 0>;
>   let captures19: box<erased> = @get_struct_field<handle, 1>;
>   let var39: { *fn, box<erased> } = @call_indirect(fnptr19, captures19, op);
>   let fnptr20: *fn = @get_struct_field<var39, 0>;
>   let captures20: box<erased> = @get_struct_field<var39, 1>;
>   let var40: int = 0;
>   let var41: { *fn, box<erased> } = @call_indirect(fnptr20, captures20, var40);
>   let fnptr21: *fn = @get_struct_field<var41, 0>;
>   let captures21: box<erased> = @get_struct_field<var41, 1>;
>   let struct5: {} = @make_struct{};
>   let unboxed6:
>         [
>            `0 {},
>            `1 {
>                box<
>                  %type_14 =
>                  [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>                ,
>               },
>            `2 { str, box<%type_14> }
>         ]
>     = @make_union<0, struct5>;
>   let var42:
>         box<
>           %type_14 =
>           [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>     = @make_box(unboxed6);
>   let var43:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_14 =
>                   [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr21, captures21, var42);
>   return var43;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_5 =
>             [ `0 {}, `1 { box<%type_5> }, `2 { str, box<%type_5> } ]>
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