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

sig succeed : ok -> Task ok * (Op *)
let succeed = \ok -> \toNext -> toNext (Ok ok);;

sig fail : err -> Task * err (Op *)
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
                    (\lastName -> outLine (~str_concat firstName (~str_concat " " lastName))))))
;;

run main_handler =
#   ^^^^^^^^^^^^
    let op = main (\x -> Done x) in
#       ^^
    let rec handle = \op -> \i -> \t -> when op is
#           ^^^^^^
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
> sig succeed : ok -> Task ok * (Op *)
> let succeed = \ok -> \toNext -> toNext (Ok ok);;
> 
> sig fail : err -> Task * err (Op *)
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
>                     (\lastName -> outLine (~str_concat firstName (~str_concat " " lastName))))))
> ;;
> 
> run main_handler =
> #   ^^^^^^^^^^^^ [
> #   ^^^^^^^^^^^^   Done [Err 'err, Ok {}]
> #   ^^^^^^^^^^^^     [
> #   ^^^^^^^^^^^^       EntryPoint,
> #   ^^^^^^^^^^^^       Stdin
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]'*>,
> #   ^^^^^^^^^^^^       Stdout Str
> #   ^^^^^^^^^^^^         <..[EntryPoint, Stdin .., Stdout .. ..]'*>
> #   ^^^^^^^^^^^^       ]'*
> #   ^^^^^^^^^^^^   ]'*
>     let op = main (\x -> Done x) in
> #       ^^ %Op [Err 'err, Ok {}]
>     let rec handle = \op -> \i -> \t -> when op is
> #           ^^^^^^ %(Op [Err 'err, Ok {}])
> #           ^^^^^^   -> Int
> #           ^^^^^^        -> [
> #           ^^^^^^             EntryPoint,
> #           ^^^^^^             Stdin
> #           ^^^^^^               <..[
> #           ^^^^^^                    EntryPoint,
> #           ^^^^^^                    Stdin ..,
> #           ^^^^^^                    Stdout .. ..
> #           ^^^^^^                    ]'a>,
> #           ^^^^^^             Stdout Str
> #           ^^^^^^               <..[
> #           ^^^^^^                    EntryPoint,
> #           ^^^^^^                    Stdin ..,
> #           ^^^^^^                    Stdout .. ..
> #           ^^^^^^                    ]'a>
> #           ^^^^^^             ]'a
> #           ^^^^^^             -> [
> #           ^^^^^^                  Done [Err 'err, Ok {}]
> #           ^^^^^^                    [
> #           ^^^^^^                      EntryPoint,
> #           ^^^^^^                      Stdin
> #           ^^^^^^                        <..[
> #           ^^^^^^                             EntryPoint,
> #           ^^^^^^                             Stdin ..,
> #           ^^^^^^                             Stdout .. ..
> #           ^^^^^^                             ]'a>,
> #           ^^^^^^                      Stdout Str
> #           ^^^^^^                        <..[
> #           ^^^^^^                             EntryPoint,
> #           ^^^^^^                             Stdin ..,
> #           ^^^^^^                             Stdout .. ..
> #           ^^^^^^                             ]'a>
> #           ^^^^^^                      ]'a
> #           ^^^^^^                  ]'*
>         | StdinLine f -> handle (f (~str_concat "stdin" (~itos i))) (~add i 1) (Stdin t)
>         | StdoutLine s f -> handle (f {}) (~add i 1) (Stdout s t)
>         | Done x -> Done x t
>     end
>     in
>     handle op 0 EntryPoint
> ;;
> 

> cor-out +ir -print
> proc clos1(captures_1: box<erased>, toNext1: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box1: box<{ [] }> = @ptr_cast(captures_1 as box<{ [] }>);
>   let captures_stack1: { [] } = @get_boxed<captures_box1>;
>   let err: [] = @get_struct_field<captures_stack1, 0>;
>   let fnptr: *fn = @get_struct_field<toNext1, 0>;
>   let captures: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct: { [] } = @make_struct{ err };
>   let var2: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct>;
>   let var3:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr, captures, var2);
>   return var3;
> }
> 
> proc clos(captures_: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box: box<{}> = @ptr_cast(captures_ as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_1: { [] } = @make_struct{ err };
>   let captures_box_1: box<{ [] }> = @make_box(captures_stack_1);
>   let captures_1: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos1>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_1, captures_1 };
>   return var1;
> }
> 
> proc fail_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   return var;
> }
> 
> global fail1: { *fn, box<erased> } = @call_direct(fail_thunk);
> 
> proc clos5(captures_5: box<erased>, result: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box5:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_5 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack5:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box5>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack5, 0>;
>   let fail1: { *fn, box<erased> } = @get_struct_field<captures_stack5, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack5, 2>;
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
>   let var9:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr4, captures4, continue);
>   return var9;
> }
> 
> proc clos4(captures_4: box<erased>, continue: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box4:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_4 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack4:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box4>;
>   let fail1: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack4, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack4, 2>;
>   let fnptr1: *fn = @get_struct_field<fromResult, 0>;
>   let captures1: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_5:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, fail1, next };
>   let captures_box_5:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_5);
>   let captures_5: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos5>;
>   let var7: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_5 };
>   let var8:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr1, captures1, var7);
>   return var8;
> }
> 
> proc clos3(captures_3: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_3 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box3>;
>   let fail1: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack3, 1>;
>   let captures_stack_4:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail1, fromResult, next };
>   let captures_box_4:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_4);
>   let captures_4: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos4>;
>   let var6: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_4 };
>   return var6;
> }
> 
> proc clos2(captures_2: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box2: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_2 as box<{ { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> } } = @get_boxed<captures_box2>;
>   let fail1: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let captures_stack_3: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail1, fromResult };
>   let captures_box_3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_3);
>   let captures_3: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos3>;
>   let var5: { *fn, box<erased> } = @make_struct{ fn_ptr_3, captures_3 };
>   return var5;
> }
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: { { *fn, box<erased> } } = @make_struct{ fail1 };
>   let captures_box_2: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_2);
>   let captures_2: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos2>;
>   let var4: { *fn, box<erased> } = @make_struct{ fn_ptr_2, captures_2 };
>   return var4;
> }
> 
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc clos8(captures_8: box<erased>, x: {}):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box8: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_8 as box<{ { *fn, box<erased> } }>);
>   let captures_stack8: { { *fn, box<erased> } } = @get_boxed<captures_box8>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack8, 0>;
>   let fnptr5: *fn = @get_struct_field<toNext2, 0>;
>   let captures5: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct2: { {} } = @make_struct{ x };
>   let var14: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct2>;
>   let var15:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr5, captures5, var14);
>   return var15;
> }
> 
> proc clos7(captures_7: box<erased>, toNext2: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_7 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_8: { { *fn, box<erased> } } = @make_struct{ toNext2 };
>   let captures_box_8: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_8: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<clos8>;
>   let var12: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_8 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s, var12 };
>   let union:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct1>;
>   let var13:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union);
>   return var13;
> }
> 
> proc clos6(captures_6: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_6 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_7: { str } = @make_struct{ s };
>   let captures_box_7: box<{ str }> = @make_box(captures_stack_7);
>   let captures_7: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos7>;
>   let var11: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_7 };
>   return var11;
> }
> 
> proc outLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_6: {} = @make_struct{};
>   let captures_box_6: box<{}> = @make_box(captures_stack_6);
>   let captures_6: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos6>;
>   let var10: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_6 };
>   return var10;
> }
> 
> global outLine1: { *fn, box<erased> } = @call_direct(outLine_thunk);
> 
> proc clos10(captures_10: box<erased>, toNext1: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box10: box<{ [] }> = @ptr_cast(captures_10 as box<{ [] }>);
>   let captures_stack10: { [] } = @get_boxed<captures_box10>;
>   let err: [] = @get_struct_field<captures_stack10, 0>;
>   let fnptr6: *fn = @get_struct_field<toNext1, 0>;
>   let captures6: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct3: { [] } = @make_struct{ err };
>   let var18: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct3>;
>   let var19:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr6, captures6, var18);
>   return var19;
> }
> 
> proc clos9(captures_9: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box9: box<{}> = @ptr_cast(captures_9 as box<{}>);
>   let captures_stack9: {} = @get_boxed<captures_box9>;
>   let captures_stack_10: { [] } = @make_struct{ err };
>   let captures_box_10: box<{ [] }> = @make_box(captures_stack_10);
>   let captures_10: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<clos10>;
>   let var17: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_10 };
>   return var17;
> }
> 
> proc fail_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_9: {} = @make_struct{};
>   let captures_box_9: box<{}> = @make_box(captures_stack_9);
>   let captures_9: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<clos9>;
>   let var16: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_9 };
>   return var16;
> }
> 
> global fail2: { *fn, box<erased> } = @call_direct(fail_thunk1);
> 
> proc clos14(captures_14: box<erased>, result: [ `0 { [] }, `1 { str } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box14:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_14 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack14:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box14>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack14, 0>;
>   let fail2: { *fn, box<erased> } = @get_struct_field<captures_stack14, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack14, 2>;
>   let discr1: int = @get_union_id<result>;
>   switch discr1 {
>   0 -> {
>     let payload3: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload3, 0>;
>     let fnptr9: *fn = @get_struct_field<fail2, 0>;
>     let captures9: box<erased> = @get_struct_field<fail2, 1>;
>     @call_indirect(fnptr9, captures9, e)
>   }
>   1 -> {
>     let payload2: { str } = @get_union_struct<result>;
>     let v: str = @get_struct_field<payload2, 0>;
>     let fnptr8: *fn = @get_struct_field<next, 0>;
>     let captures8: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr8, captures8, v)
>   }
>   } in join join1;
>   let inner: { *fn, box<erased> } = join1;
>   let fnptr10: *fn = @get_struct_field<inner, 0>;
>   let captures10: box<erased> = @get_struct_field<inner, 1>;
>   let var25:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr10, captures10, continue);
>   return var25;
> }
> 
> proc clos13(captures_13: box<erased>, continue: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box13:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_13 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack13:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box13>;
>   let fail2: { *fn, box<erased> } = @get_struct_field<captures_stack13, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack13, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack13, 2>;
>   let fnptr7: *fn = @get_struct_field<fromResult, 0>;
>   let captures7: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_14:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, fail2, next };
>   let captures_box_14:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_14);
>   let captures_14: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<clos14>;
>   let var23: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_14 };
>   let var24:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr7, captures7, var23);
>   return var24;
> }
> 
> proc clos12(captures_12: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box12: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_12 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack12: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box12>;
>   let fail2: { *fn, box<erased> } = @get_struct_field<captures_stack12, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack12, 1>;
>   let captures_stack_13:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail2, fromResult, next };
>   let captures_box_13:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_13);
>   let captures_13: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<clos13>;
>   let var22: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_13 };
>   return var22;
> }
> 
> proc clos11(captures_11: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box11: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_11 as box<{ { *fn, box<erased> } }>);
>   let captures_stack11: { { *fn, box<erased> } } = @get_boxed<captures_box11>;
>   let fail2: { *fn, box<erased> } = @get_struct_field<captures_stack11, 0>;
>   let captures_stack_12: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail2, fromResult };
>   let captures_box_12: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_12);
>   let captures_12: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<clos12>;
>   let var21: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_12 };
>   return var21;
> }
> 
> proc await_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_11: { { *fn, box<erased> } } = @make_struct{ fail2 };
>   let captures_box_11: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_11);
>   let captures_11: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<clos11>;
>   let var20: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_11 };
>   return var20;
> }
> 
> global await2: { *fn, box<erased> } = @call_direct(await_thunk1);
> 
> proc clos16(captures_16: box<erased>, s1: str):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box16: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_16 as box<{ { *fn, box<erased> } }>);
>   let captures_stack16: { { *fn, box<erased> } } = @get_boxed<captures_box16>;
>   let toNext3: { *fn, box<erased> } = @get_struct_field<captures_stack16, 0>;
>   let fnptr11: *fn = @get_struct_field<toNext3, 0>;
>   let captures11: box<erased> = @get_struct_field<toNext3, 1>;
>   let struct5: { str } = @make_struct{ s1 };
>   let var29: [ `0 { [] }, `1 { str } ] = @make_union<1, struct5>;
>   let var30:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr11, captures11, var29);
>   return var30;
> }
> 
> proc clos15(captures_15: box<erased>, toNext3: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_15 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let captures_stack_16: { { *fn, box<erased> } } = @make_struct{ toNext3 };
>   let captures_box_16: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_16);
>   let captures_16: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<clos16>;
>   let var27: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_16 };
>   let struct4: { { *fn, box<erased> } } = @make_struct{ var27 };
>   let union1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct4>;
>   let var28:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union1);
>   return var28;
> }
> 
> proc inLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_15: {} = @make_struct{};
>   let captures_box_15: box<{}> = @make_box(captures_stack_15);
>   let captures_15: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<clos15>;
>   let var26: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_15 };
>   return var26;
> }
> 
> global inLine1: { *fn, box<erased> } = @call_direct(inLine_thunk);
> 
> proc clos18(captures_18: box<erased>, toNext1: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box18: box<{ [] }> = @ptr_cast(captures_18 as box<{ [] }>);
>   let captures_stack18: { [] } = @get_boxed<captures_box18>;
>   let err: [] = @get_struct_field<captures_stack18, 0>;
>   let fnptr12: *fn = @get_struct_field<toNext1, 0>;
>   let captures12: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct6: { [] } = @make_struct{ err };
>   let var33: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct6>;
>   let var34:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr12, captures12, var33);
>   return var34;
> }
> 
> proc clos17(captures_17: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box17: box<{}> = @ptr_cast(captures_17 as box<{}>);
>   let captures_stack17: {} = @get_boxed<captures_box17>;
>   let captures_stack_18: { [] } = @make_struct{ err };
>   let captures_box_18: box<{ [] }> = @make_box(captures_stack_18);
>   let captures_18: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<clos18>;
>   let var32: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_18 };
>   return var32;
> }
> 
> proc fail_thunk2(): { *fn, box<erased> }
> {
>   let captures_stack_17: {} = @make_struct{};
>   let captures_box_17: box<{}> = @make_box(captures_stack_17);
>   let captures_17: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<clos17>;
>   let var31: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_17 };
>   return var31;
> }
> 
> global fail3: { *fn, box<erased> } = @call_direct(fail_thunk2);
> 
> proc clos22(captures_22: box<erased>, result: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box22:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_22 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack22:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box22>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack22, 0>;
>   let fail3: { *fn, box<erased> } = @get_struct_field<captures_stack22, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack22, 2>;
>   let discr2: int = @get_union_id<result>;
>   switch discr2 {
>   0 -> {
>     let payload5: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload5, 0>;
>     let fnptr15: *fn = @get_struct_field<fail3, 0>;
>     let captures15: box<erased> = @get_struct_field<fail3, 1>;
>     @call_indirect(fnptr15, captures15, e)
>   }
>   1 -> {
>     let payload4: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload4, 0>;
>     let fnptr14: *fn = @get_struct_field<next, 0>;
>     let captures14: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr14, captures14, v)
>   }
>   } in join join2;
>   let inner: { *fn, box<erased> } = join2;
>   let fnptr16: *fn = @get_struct_field<inner, 0>;
>   let captures16: box<erased> = @get_struct_field<inner, 1>;
>   let var40:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr16, captures16, continue);
>   return var40;
> }
> 
> proc clos21(captures_21: box<erased>, continue: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box21:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_21 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack21:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box21>;
>   let fail3: { *fn, box<erased> } = @get_struct_field<captures_stack21, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack21, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack21, 2>;
>   let fnptr13: *fn = @get_struct_field<fromResult, 0>;
>   let captures13: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_22:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, fail3, next };
>   let captures_box_22:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_22);
>   let captures_22: box<erased> = @ptr_cast(captures_box_22 as box<erased>);
>   let fn_ptr_22: *fn = @make_fn_ptr<clos22>;
>   let var38: { *fn, box<erased> } = @make_struct{ fn_ptr_22, captures_22 };
>   let var39:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr13, captures13, var38);
>   return var39;
> }
> 
> proc clos20(captures_20: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box20: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_20 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack20: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box20>;
>   let fail3: { *fn, box<erased> } = @get_struct_field<captures_stack20, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack20, 1>;
>   let captures_stack_21:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail3, fromResult, next };
>   let captures_box_21:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_21);
>   let captures_21: box<erased> = @ptr_cast(captures_box_21 as box<erased>);
>   let fn_ptr_21: *fn = @make_fn_ptr<clos21>;
>   let var37: { *fn, box<erased> } = @make_struct{ fn_ptr_21, captures_21 };
>   return var37;
> }
> 
> proc clos19(captures_19: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box19: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_19 as box<{ { *fn, box<erased> } }>);
>   let captures_stack19: { { *fn, box<erased> } } = @get_boxed<captures_box19>;
>   let fail3: { *fn, box<erased> } = @get_struct_field<captures_stack19, 0>;
>   let captures_stack_20: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail3, fromResult };
>   let captures_box_20: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_20);
>   let captures_20: box<erased> = @ptr_cast(captures_box_20 as box<erased>);
>   let fn_ptr_20: *fn = @make_fn_ptr<clos20>;
>   let var36: { *fn, box<erased> } = @make_struct{ fn_ptr_20, captures_20 };
>   return var36;
> }
> 
> proc await_thunk2(): { *fn, box<erased> }
> {
>   let captures_stack_19: { { *fn, box<erased> } } = @make_struct{ fail3 };
>   let captures_box_19: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_19);
>   let captures_19: box<erased> = @ptr_cast(captures_box_19 as box<erased>);
>   let fn_ptr_19: *fn = @make_fn_ptr<clos19>;
>   let var35: { *fn, box<erased> } = @make_struct{ fn_ptr_19, captures_19 };
>   return var35;
> }
> 
> global await3: { *fn, box<erased> } = @call_direct(await_thunk2);
> 
> proc clos25(captures_25: box<erased>, x: {}):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box25: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_25 as box<{ { *fn, box<erased> } }>);
>   let captures_stack25: { { *fn, box<erased> } } = @get_boxed<captures_box25>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack25, 0>;
>   let fnptr17: *fn = @get_struct_field<toNext2, 0>;
>   let captures17: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct8: { {} } = @make_struct{ x };
>   let var45: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct8>;
>   let var46:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr17, captures17, var45);
>   return var46;
> }
> 
> proc clos24(captures_24: box<erased>, toNext2: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box24: box<{ str }> = @ptr_cast(captures_24 as box<{ str }>);
>   let captures_stack24: { str } = @get_boxed<captures_box24>;
>   let s: str = @get_struct_field<captures_stack24, 0>;
>   let captures_stack_25: { { *fn, box<erased> } } = @make_struct{ toNext2 };
>   let captures_box_25: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_25);
>   let captures_25: box<erased> = @ptr_cast(captures_box_25 as box<erased>);
>   let fn_ptr_25: *fn = @make_fn_ptr<clos25>;
>   let var43: { *fn, box<erased> } = @make_struct{ fn_ptr_25, captures_25 };
>   let struct7: { str, { *fn, box<erased> } } = @make_struct{ s, var43 };
>   let union2:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct7>;
>   let var44:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union2);
>   return var44;
> }
> 
> proc clos23(captures_23: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box23: box<{}> = @ptr_cast(captures_23 as box<{}>);
>   let captures_stack23: {} = @get_boxed<captures_box23>;
>   let captures_stack_24: { str } = @make_struct{ s };
>   let captures_box_24: box<{ str }> = @make_box(captures_stack_24);
>   let captures_24: box<erased> = @ptr_cast(captures_box_24 as box<erased>);
>   let fn_ptr_24: *fn = @make_fn_ptr<clos24>;
>   let var42: { *fn, box<erased> } = @make_struct{ fn_ptr_24, captures_24 };
>   return var42;
> }
> 
> proc outLine_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_23: {} = @make_struct{};
>   let captures_box_23: box<{}> = @make_box(captures_stack_23);
>   let captures_23: box<erased> = @ptr_cast(captures_box_23 as box<erased>);
>   let fn_ptr_23: *fn = @make_fn_ptr<clos23>;
>   let var41: { *fn, box<erased> } = @make_struct{ fn_ptr_23, captures_23 };
>   return var41;
> }
> 
> global outLine2: { *fn, box<erased> } = @call_direct(outLine_thunk1);
> 
> proc clos27(captures_27: box<erased>, toNext1: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box27: box<{ [] }> = @ptr_cast(captures_27 as box<{ [] }>);
>   let captures_stack27: { [] } = @get_boxed<captures_box27>;
>   let err: [] = @get_struct_field<captures_stack27, 0>;
>   let fnptr18: *fn = @get_struct_field<toNext1, 0>;
>   let captures18: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct9: { [] } = @make_struct{ err };
>   let var49: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct9>;
>   let var50:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr18, captures18, var49);
>   return var50;
> }
> 
> proc clos26(captures_26: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box26: box<{}> = @ptr_cast(captures_26 as box<{}>);
>   let captures_stack26: {} = @get_boxed<captures_box26>;
>   let captures_stack_27: { [] } = @make_struct{ err };
>   let captures_box_27: box<{ [] }> = @make_box(captures_stack_27);
>   let captures_27: box<erased> = @ptr_cast(captures_box_27 as box<erased>);
>   let fn_ptr_27: *fn = @make_fn_ptr<clos27>;
>   let var48: { *fn, box<erased> } = @make_struct{ fn_ptr_27, captures_27 };
>   return var48;
> }
> 
> proc fail_thunk3(): { *fn, box<erased> }
> {
>   let captures_stack_26: {} = @make_struct{};
>   let captures_box_26: box<{}> = @make_box(captures_stack_26);
>   let captures_26: box<erased> = @ptr_cast(captures_box_26 as box<erased>);
>   let fn_ptr_26: *fn = @make_fn_ptr<clos26>;
>   let var47: { *fn, box<erased> } = @make_struct{ fn_ptr_26, captures_26 };
>   return var47;
> }
> 
> global fail4: { *fn, box<erased> } = @call_direct(fail_thunk3);
> 
> proc clos31(captures_31: box<erased>, result: [ `0 { [] }, `1 { str } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box31:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_31 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack31:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box31>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack31, 0>;
>   let fail4: { *fn, box<erased> } = @get_struct_field<captures_stack31, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack31, 2>;
>   let discr3: int = @get_union_id<result>;
>   switch discr3 {
>   0 -> {
>     let payload7: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload7, 0>;
>     let fnptr21: *fn = @get_struct_field<fail4, 0>;
>     let captures21: box<erased> = @get_struct_field<fail4, 1>;
>     @call_indirect(fnptr21, captures21, e)
>   }
>   1 -> {
>     let payload6: { str } = @get_union_struct<result>;
>     let v: str = @get_struct_field<payload6, 0>;
>     let fnptr20: *fn = @get_struct_field<next, 0>;
>     let captures20: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr20, captures20, v)
>   }
>   } in join join3;
>   let inner: { *fn, box<erased> } = join3;
>   let fnptr22: *fn = @get_struct_field<inner, 0>;
>   let captures22: box<erased> = @get_struct_field<inner, 1>;
>   let var56:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr22, captures22, continue);
>   return var56;
> }
> 
> proc clos30(captures_30: box<erased>, continue: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box30:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_30 as
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack30:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box30>;
>   let fail4: { *fn, box<erased> } = @get_struct_field<captures_stack30, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack30, 1>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack30, 2>;
>   let fnptr19: *fn = @get_struct_field<fromResult, 0>;
>   let captures19: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_31:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, fail4, next };
>   let captures_box_31:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_31);
>   let captures_31: box<erased> = @ptr_cast(captures_box_31 as box<erased>);
>   let fn_ptr_31: *fn = @make_fn_ptr<clos31>;
>   let var54: { *fn, box<erased> } = @make_struct{ fn_ptr_31, captures_31 };
>   let var55:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr19, captures19, var54);
>   return var55;
> }
> 
> proc clos29(captures_29: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box29: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_29 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack29: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box29>;
>   let fail4: { *fn, box<erased> } = @get_struct_field<captures_stack29, 0>;
>   let fromResult: { *fn, box<erased> }
>     = @get_struct_field<captures_stack29, 1>;
>   let captures_stack_30:
>         { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail4, fromResult, next };
>   let captures_box_30:
>         box<
>           { { *fn, box<erased> }, { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_30);
>   let captures_30: box<erased> = @ptr_cast(captures_box_30 as box<erased>);
>   let fn_ptr_30: *fn = @make_fn_ptr<clos30>;
>   let var53: { *fn, box<erased> } = @make_struct{ fn_ptr_30, captures_30 };
>   return var53;
> }
> 
> proc clos28(captures_28: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box28: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_28 as box<{ { *fn, box<erased> } }>);
>   let captures_stack28: { { *fn, box<erased> } } = @get_boxed<captures_box28>;
>   let fail4: { *fn, box<erased> } = @get_struct_field<captures_stack28, 0>;
>   let captures_stack_29: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fail4, fromResult };
>   let captures_box_29: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_29);
>   let captures_29: box<erased> = @ptr_cast(captures_box_29 as box<erased>);
>   let fn_ptr_29: *fn = @make_fn_ptr<clos29>;
>   let var52: { *fn, box<erased> } = @make_struct{ fn_ptr_29, captures_29 };
>   return var52;
> }
> 
> proc await_thunk3(): { *fn, box<erased> }
> {
>   let captures_stack_28: { { *fn, box<erased> } } = @make_struct{ fail4 };
>   let captures_box_28: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_28);
>   let captures_28: box<erased> = @ptr_cast(captures_box_28 as box<erased>);
>   let fn_ptr_28: *fn = @make_fn_ptr<clos28>;
>   let var51: { *fn, box<erased> } = @make_struct{ fn_ptr_28, captures_28 };
>   return var51;
> }
> 
> global await4: { *fn, box<erased> } = @call_direct(await_thunk3);
> 
> proc clos33(captures_33: box<erased>, s1: str):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box33: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_33 as box<{ { *fn, box<erased> } }>);
>   let captures_stack33: { { *fn, box<erased> } } = @get_boxed<captures_box33>;
>   let toNext3: { *fn, box<erased> } = @get_struct_field<captures_stack33, 0>;
>   let fnptr23: *fn = @get_struct_field<toNext3, 0>;
>   let captures23: box<erased> = @get_struct_field<toNext3, 1>;
>   let struct11: { str } = @make_struct{ s1 };
>   let var60: [ `0 { [] }, `1 { str } ] = @make_union<1, struct11>;
>   let var61:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr23, captures23, var60);
>   return var61;
> }
> 
> proc clos32(captures_32: box<erased>, toNext3: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box32: box<{}> = @ptr_cast(captures_32 as box<{}>);
>   let captures_stack32: {} = @get_boxed<captures_box32>;
>   let captures_stack_33: { { *fn, box<erased> } } = @make_struct{ toNext3 };
>   let captures_box_33: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_33);
>   let captures_33: box<erased> = @ptr_cast(captures_box_33 as box<erased>);
>   let fn_ptr_33: *fn = @make_fn_ptr<clos33>;
>   let var58: { *fn, box<erased> } = @make_struct{ fn_ptr_33, captures_33 };
>   let struct10: { { *fn, box<erased> } } = @make_struct{ var58 };
>   let union3:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct10>;
>   let var59:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union3);
>   return var59;
> }
> 
> proc inLine_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_32: {} = @make_struct{};
>   let captures_box_32: box<{}> = @make_box(captures_stack_32);
>   let captures_32: box<erased> = @ptr_cast(captures_box_32 as box<erased>);
>   let fn_ptr_32: *fn = @make_fn_ptr<clos32>;
>   let var57: { *fn, box<erased> } = @make_struct{ fn_ptr_32, captures_32 };
>   return var57;
> }
> 
> global inLine2: { *fn, box<erased> } = @call_direct(inLine_thunk1);
> 
> proc clos36(captures_36: box<erased>, x: {}):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box36: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_36 as box<{ { *fn, box<erased> } }>);
>   let captures_stack36: { { *fn, box<erased> } } = @get_boxed<captures_box36>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack36, 0>;
>   let fnptr24: *fn = @get_struct_field<toNext2, 0>;
>   let captures24: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct13: { {} } = @make_struct{ x };
>   let var66: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct13>;
>   let var67:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr24, captures24, var66);
>   return var67;
> }
> 
> proc clos35(captures_35: box<erased>, toNext2: { *fn, box<erased> }):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box35: box<{ str }> = @ptr_cast(captures_35 as box<{ str }>);
>   let captures_stack35: { str } = @get_boxed<captures_box35>;
>   let s: str = @get_struct_field<captures_stack35, 0>;
>   let captures_stack_36: { { *fn, box<erased> } } = @make_struct{ toNext2 };
>   let captures_box_36: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_36);
>   let captures_36: box<erased> = @ptr_cast(captures_box_36 as box<erased>);
>   let fn_ptr_36: *fn = @make_fn_ptr<clos36>;
>   let var64: { *fn, box<erased> } = @make_struct{ fn_ptr_36, captures_36 };
>   let struct12: { str, { *fn, box<erased> } } = @make_struct{ s, var64 };
>   let union4:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct12>;
>   let var65:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union4);
>   return var65;
> }
> 
> proc clos34(captures_34: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box34: box<{}> = @ptr_cast(captures_34 as box<{}>);
>   let captures_stack34: {} = @get_boxed<captures_box34>;
>   let captures_stack_35: { str } = @make_struct{ s };
>   let captures_box_35: box<{ str }> = @make_box(captures_stack_35);
>   let captures_35: box<erased> = @ptr_cast(captures_box_35 as box<erased>);
>   let fn_ptr_35: *fn = @make_fn_ptr<clos35>;
>   let var63: { *fn, box<erased> } = @make_struct{ fn_ptr_35, captures_35 };
>   return var63;
> }
> 
> proc outLine_thunk2(): { *fn, box<erased> }
> {
>   let captures_stack_34: {} = @make_struct{};
>   let captures_box_34: box<{}> = @make_box(captures_stack_34);
>   let captures_34: box<erased> = @ptr_cast(captures_box_34 as box<erased>);
>   let fn_ptr_34: *fn = @make_fn_ptr<clos34>;
>   let var62: { *fn, box<erased> } = @make_struct{ fn_ptr_34, captures_34 };
>   return var62;
> }
> 
> global outLine3: { *fn, box<erased> } = @call_direct(outLine_thunk2);
> 
> proc clos40(captures_40: box<erased>, lastName: str): { *fn, box<erased> }
> {
>   let captures_box40: box<{ str, { *fn, box<erased> } }>
>     = @ptr_cast(captures_40 as box<{ str, { *fn, box<erased> } }>);
>   let captures_stack40: { str, { *fn, box<erased> } }
>     = @get_boxed<captures_box40>;
>   let firstName: str = @get_struct_field<captures_stack40, 0>;
>   let outLine3: { *fn, box<erased> } = @get_struct_field<captures_stack40, 1>;
>   let fnptr35: *fn = @get_struct_field<outLine3, 0>;
>   let captures35: box<erased> = @get_struct_field<outLine3, 1>;
>   let var84: str = " ";
>   let var85: str = @call_kfn(str_concat, var84, lastName);
>   let var86: str = @call_kfn(str_concat, firstName, var85);
>   let var87: { *fn, box<erased> } = @call_indirect(fnptr35, captures35, var86);
>   return var87;
> }
> 
> proc clos39(captures_39: box<erased>, y: {}): { *fn, box<erased> }
> {
>   let captures_box39:
>         box<
>           {
>            { *fn, box<erased> },
>             str,
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @ptr_cast(
>         captures_39 as
>         box<
>           {
>            { *fn, box<erased> },
>             str,
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>);
>   let captures_stack39:
>         { { *fn, box<erased> }, str, { *fn, box<erased> }, { *fn, box<erased> } ,
>         }
>     = @get_boxed<captures_box39>;
>   let await4: { *fn, box<erased> } = @get_struct_field<captures_stack39, 0>;
>   let firstName: str = @get_struct_field<captures_stack39, 1>;
>   let inLine2: { *fn, box<erased> } = @get_struct_field<captures_stack39, 2>;
>   let outLine3: { *fn, box<erased> } = @get_struct_field<captures_stack39, 3>;
>   let fnptr33: *fn = @get_struct_field<await4, 0>;
>   let captures33: box<erased> = @get_struct_field<await4, 1>;
>   let var81: { *fn, box<erased> }
>     = @call_indirect(fnptr33, captures33, inLine2);
>   let fnptr34: *fn = @get_struct_field<var81, 0>;
>   let captures34: box<erased> = @get_struct_field<var81, 1>;
>   let captures_stack_40: { str, { *fn, box<erased> } }
>     = @make_struct{ firstName, outLine3 };
>   let captures_box_40: box<{ str, { *fn, box<erased> } }>
>     = @make_box(captures_stack_40);
>   let captures_40: box<erased> = @ptr_cast(captures_box_40 as box<erased>);
>   let fn_ptr_40: *fn = @make_fn_ptr<clos40>;
>   let var82: { *fn, box<erased> } = @make_struct{ fn_ptr_40, captures_40 };
>   let var83: { *fn, box<erased> } = @call_indirect(fnptr34, captures34, var82);
>   return var83;
> }
> 
> proc clos38(captures_38: box<erased>, firstName: str): { *fn, box<erased> }
> {
>   let captures_box38:
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @ptr_cast(
>         captures_38 as
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>);
>   let captures_stack38:
>         {
>          { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> }
>          ,
>         }
>     = @get_boxed<captures_box38>;
>   let await3: { *fn, box<erased> } = @get_struct_field<captures_stack38, 0>;
>   let await4: { *fn, box<erased> } = @get_struct_field<captures_stack38, 1>;
>   let inLine2: { *fn, box<erased> } = @get_struct_field<captures_stack38, 2>;
>   let outLine2: { *fn, box<erased> } = @get_struct_field<captures_stack38, 3>;
>   let outLine3: { *fn, box<erased> } = @get_struct_field<captures_stack38, 4>;
>   let fnptr30: *fn = @get_struct_field<await3, 0>;
>   let captures30: box<erased> = @get_struct_field<await3, 1>;
>   let fnptr31: *fn = @get_struct_field<outLine2, 0>;
>   let captures31: box<erased> = @get_struct_field<outLine2, 1>;
>   let var76: str = "What's your last name?";
>   let var77: { *fn, box<erased> } = @call_indirect(fnptr31, captures31, var76);
>   let var78: { *fn, box<erased> } = @call_indirect(fnptr30, captures30, var77);
>   let fnptr32: *fn = @get_struct_field<var78, 0>;
>   let captures32: box<erased> = @get_struct_field<var78, 1>;
>   let captures_stack_39:
>         { { *fn, box<erased> }, str, { *fn, box<erased> }, { *fn, box<erased> } ,
>         }
>     = @make_struct{ await4, firstName, inLine2, outLine3 };
>   let captures_box_39:
>         box<
>           {
>            { *fn, box<erased> },
>             str,
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @make_box(captures_stack_39);
>   let captures_39: box<erased> = @ptr_cast(captures_box_39 as box<erased>);
>   let fn_ptr_39: *fn = @make_fn_ptr<clos39>;
>   let var79: { *fn, box<erased> } = @make_struct{ fn_ptr_39, captures_39 };
>   let var80: { *fn, box<erased> } = @call_indirect(fnptr32, captures32, var79);
>   return var80;
> }
> 
> proc clos37(captures_37: box<erased>, x1: {}): { *fn, box<erased> }
> {
>   let captures_box37:
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @ptr_cast(
>         captures_37 as
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>);
>   let captures_stack37:
>         {
>          { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> }
>          ,
>         }
>     = @get_boxed<captures_box37>;
>   let await2: { *fn, box<erased> } = @get_struct_field<captures_stack37, 0>;
>   let await3: { *fn, box<erased> } = @get_struct_field<captures_stack37, 1>;
>   let await4: { *fn, box<erased> } = @get_struct_field<captures_stack37, 2>;
>   let inLine1: { *fn, box<erased> } = @get_struct_field<captures_stack37, 3>;
>   let inLine2: { *fn, box<erased> } = @get_struct_field<captures_stack37, 4>;
>   let outLine2: { *fn, box<erased> } = @get_struct_field<captures_stack37, 5>;
>   let outLine3: { *fn, box<erased> } = @get_struct_field<captures_stack37, 6>;
>   let fnptr28: *fn = @get_struct_field<await2, 0>;
>   let captures28: box<erased> = @get_struct_field<await2, 1>;
>   let var73: { *fn, box<erased> }
>     = @call_indirect(fnptr28, captures28, inLine1);
>   let fnptr29: *fn = @get_struct_field<var73, 0>;
>   let captures29: box<erased> = @get_struct_field<var73, 1>;
>   let captures_stack_38:
>         {
>          { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> }
>          ,
>         }
>     = @make_struct{ await3, await4, inLine2, outLine2, outLine3 };
>   let captures_box_38:
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @make_box(captures_stack_38);
>   let captures_38: box<erased> = @ptr_cast(captures_box_38 as box<erased>);
>   let fn_ptr_38: *fn = @make_fn_ptr<clos38>;
>   let var74: { *fn, box<erased> } = @make_struct{ fn_ptr_38, captures_38 };
>   let var75: { *fn, box<erased> } = @call_indirect(fnptr29, captures29, var74);
>   return var75;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr25: *fn = @get_struct_field<await1, 0>;
>   let captures25: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr26: *fn = @get_struct_field<outLine1, 0>;
>   let captures26: box<erased> = @get_struct_field<outLine1, 1>;
>   let var68: str = "What's your first name?";
>   let var69: { *fn, box<erased> } = @call_indirect(fnptr26, captures26, var68);
>   let var70: { *fn, box<erased> } = @call_indirect(fnptr25, captures25, var69);
>   let fnptr27: *fn = @get_struct_field<var70, 0>;
>   let captures27: box<erased> = @get_struct_field<var70, 1>;
>   let captures_stack_37:
>         {
>          { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> },
>           { *fn, box<erased> }
>          ,
>         }
>     = @make_struct{ await2, await3, await4, inLine1, inLine2, outLine2,
>        outLine3 };
>   let captures_box_37:
>         box<
>           {
>            { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> },
>             { *fn, box<erased> }
>            ,
>           }>
>     = @make_box(captures_stack_37);
>   let captures_37: box<erased> = @ptr_cast(captures_box_37 as box<erased>);
>   let fn_ptr_37: *fn = @make_fn_ptr<clos37>;
>   let var71: { *fn, box<erased> } = @make_struct{ fn_ptr_37, captures_37 };
>   let var72: { *fn, box<erased> } = @call_indirect(fnptr27, captures27, var71);
>   return var72;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc clos41(captures_41: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_1 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box41: box<{}> = @ptr_cast(captures_41 as box<{}>);
>   let captures_stack41: {} = @get_boxed<captures_box41>;
>   let struct15: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let union6:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct15>;
>   let var94:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(union6);
>   return var94;
> }
> 
> proc clos43(
>   captures_43: box<erased>,
>    t: box<%type_0 = [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>):
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_0 =
>             [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>          ,
>         }
>   ]
> {
>   let captures_box44:
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
>         captures_43 as
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
>   let captures_stack44:
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
>     = @get_boxed<captures_box44>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack44, 0>;
>   let i: int = @get_struct_field<captures_stack44, 1>;
>   let op1:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack44, 2>;
>   let inner1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_boxed<op1>;
>   let discr4: int = @get_union_id<inner1>;
>   switch discr4 {
>   0 -> {
>     let payload10: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<inner1>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload10, 0>;
>     let struct18:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_0 =
>               [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct18>
>   }
>   1 -> {
>     let payload8: { { *fn, box<erased> } } = @get_union_struct<inner1>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload8, 0>;
>     let fnptr40: *fn = @get_struct_field<handle, 0>;
>     let captures40: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr41: *fn = @get_struct_field<f, 0>;
>     let captures41: box<erased> = @get_struct_field<f, 1>;
>     let var97: str = "stdin";
>     let var98: str = @call_kfn(itos, i);
>     let var99: str = @call_kfn(str_concat, var97, var98);
>     let var100:
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr41, captures41, var99);
>     let var101: { *fn, box<erased> }
>       = @call_indirect(fnptr40, captures40, var100);
>     let fnptr42: *fn = @get_struct_field<var101, 0>;
>     let captures42: box<erased> = @get_struct_field<var101, 1>;
>     let var102: int = 1;
>     let var103: int = @call_kfn(add, i, var102);
>     let var104: { *fn, box<erased> }
>       = @call_indirect(fnptr42, captures42, var103);
>     let fnptr43: *fn = @get_struct_field<var104, 0>;
>     let captures43: box<erased> = @get_struct_field<var104, 1>;
>     let struct16:
>           {
>            box<
>              %type_0 =
>              [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let union7:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_0 =
>                    [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_0> }
>           ]
>       = @make_union<1, struct16>;
>     let var105:
>           box<
>             %type_0 =
>             [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>       = @make_box(union7);
>     @call_indirect(fnptr43, captures43, var105)
>   }
>   2 -> {
>     let payload9: { str, { *fn, box<erased> } } = @get_union_struct<inner1>;
>     let s2: str = @get_struct_field<payload9, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload9, 1>;
>     let fnptr44: *fn = @get_struct_field<handle, 0>;
>     let captures44: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr45: *fn = @get_struct_field<f1, 0>;
>     let captures45: box<erased> = @get_struct_field<f1, 1>;
>     let var106: {} = @make_struct{};
>     let var107:
>           box<
>             %type_1 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr45, captures45, var106);
>     let var108: { *fn, box<erased> }
>       = @call_indirect(fnptr44, captures44, var107);
>     let fnptr46: *fn = @get_struct_field<var108, 0>;
>     let captures46: box<erased> = @get_struct_field<var108, 1>;
>     let var109: int = 1;
>     let var110: int = @call_kfn(add, i, var109);
>     let var111: { *fn, box<erased> }
>       = @call_indirect(fnptr46, captures46, var110);
>     let fnptr47: *fn = @get_struct_field<var111, 0>;
>     let captures47: box<erased> = @get_struct_field<var111, 1>;
>     let struct17:
>           {
>            str,
>             box<
>               %type_0 =
>               [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let union8:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_0 =
>                    [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_0> }
>           ]
>       = @make_union<2, struct17>;
>     let var112:
>           box<
>             %type_0 =
>             [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>       = @make_box(union8);
>     @call_indirect(fnptr47, captures47, var112)
>   }
>   } in join join4;
>   return join4;
> }
> 
> proc clos42(captures_42: box<erased>, i: int): { *fn, box<erased> }
> {
>   let captures_box43:
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
>         captures_42 as
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
>   let captures_stack43:
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
>     = @get_boxed<captures_box43>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack43, 0>;
>   let op1:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack43, 1>;
>   let captures_stack_43:
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
>   let captures_box_43:
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
>     = @make_box(captures_stack_43);
>   let captures_43: box<erased> = @ptr_cast(captures_box_43 as box<erased>);
>   let fn_ptr_43: *fn = @make_fn_ptr<clos43>;
>   let var96: { *fn, box<erased> } = @make_struct{ fn_ptr_43, captures_43 };
>   return var96;
> }
> 
> proc handle1(
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
>   let captures_box42: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack42: {} = @get_boxed<captures_box42>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let captures_stack_42:
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
>   let captures_box_42:
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
>     = @make_box(captures_stack_42);
>   let captures_42: box<erased> = @ptr_cast(captures_box_42 as box<erased>);
>   let fn_ptr_42: *fn = @make_fn_ptr<clos42>;
>   let var95: { *fn, box<erased> } = @make_struct{ fn_ptr_42, captures_42 };
>   return var95;
> }
> 
> proc main_handler_thunk():
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_0 =
>             [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>          ,
>         }
>   ]
> {
>   let fnptr36: *fn = @get_struct_field<main1, 0>;
>   let captures36: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_41: {} = @make_struct{};
>   let captures_box_41: box<{}> = @make_box(captures_stack_41);
>   let captures_41: box<erased> = @ptr_cast(captures_box_41 as box<erased>);
>   let fn_ptr_41: *fn = @make_fn_ptr<clos41>;
>   let var88: { *fn, box<erased> } = @make_struct{ fn_ptr_41, captures_41 };
>   let op:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr36, captures36, var88);
>   let captures_stack_handle: {} = @make_struct{};
>   let captures_box_handle: box<{}> = @make_box(captures_stack_handle);
>   let captures_handle: box<erased>
>     = @ptr_cast(captures_box_handle as box<erased>);
>   let fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_handle, captures_handle };
>   let fnptr37: *fn = @get_struct_field<handle, 0>;
>   let captures37: box<erased> = @get_struct_field<handle, 1>;
>   let var89: { *fn, box<erased> } = @call_indirect(fnptr37, captures37, op);
>   let fnptr38: *fn = @get_struct_field<var89, 0>;
>   let captures38: box<erased> = @get_struct_field<var89, 1>;
>   let var90: int = 0;
>   let var91: { *fn, box<erased> } = @call_indirect(fnptr38, captures38, var90);
>   let fnptr39: *fn = @get_struct_field<var91, 0>;
>   let captures39: box<erased> = @get_struct_field<var91, 1>;
>   let struct14: {} = @make_struct{};
>   let union5:
>         [
>            `0 {},
>            `1 {
>                box<
>                  %type_0 =
>                  [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>                ,
>               },
>            `2 { str, box<%type_0> }
>         ]
>     = @make_union<0, struct14>;
>   let var92:
>         box<%type_0 = [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>     = @make_box(union5);
>   let var93:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_0 =
>                   [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr39, captures39, var92);
>   return var93;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_0 =
>             [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>          ,
>         }
>   ]
>   = @call_direct(main_handler_thunk);
> 
> entry main_handler;

> cor-out +eval -print
> main_handler = [0 [1 []]
>                [2
>                [115 116 100 105 110 49 32 115 116
>                100 105 110 51]
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
>                  (Stdout "stdin1 stdin3"
>                     (Stdin
>                        (Stdout
>                           "What's your last name?"
>                           (Stdin
>                              (Stdout
>                                 "What's your first name?"
>                                 (EntryPoint ))))))
