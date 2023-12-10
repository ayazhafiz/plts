# cor +solve -elab
# cor +mono -print
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

> cor-out +mono -print
> specializations:
>   let lam41 = \next -[lam41 fromResult]->
>     \continue -[lam31 fromResult next]->
>       (fromResult
>          \result -[lam21 continue next]->
>            (let inner =
>               when result is
>                 | Okv -> next v
>                 | Erre -> fail2 e
>               end
>            in
>            inner continue))
>   
>   let lam31 = \continue -[lam31 fromResult
>                         next]->
>     fromResult
>       \result -[lam21 continue next]->
>         (let inner =
>            when result is
>              | Okv -> next v
>              | Erre -> fail2 e
>            end
>         in
>         inner continue)
>   
>   let lam21 = \result -[lam21 continue next]->
>     let inner =
>       when result is
>         | Okv -> next v
>         | Erre -> fail2 e
>       end
>     in
>     inner continue
>   
>   let lam15 = \toNext1 -[lam15 err]->
>     toNext1 (Err err)
>   
>   let fail2 = \err -[fail2]->
>     \toNext1 -[lam15 err]-> (toNext1 (Err err))
>   
>   let await2 = \fromResult -[await2]->
>     \next -[lam41 fromResult]->
>       \continue -[lam31 fromResult next]->
>         (fromResult
>            \result -[lam21 continue next]->
>              (let inner =
>                 when result is
>                   | Okv -> next v
>                   | Erre -> fail2 e
>                 end
>              in
>              inner continue))
>   
>   let lam61 = \toNext2 -[lam61 s]->
>     StdoutLine s
>       \x -[lam51 toNext2]-> (toNext2 (Ok x))
>   
>   let lam51 = \x -[lam51 toNext2]->
>     toNext2 (Ok x)
>   
>   let outLine2 = \s -[outLine2]->
>     \toNext2 -[lam61 s]->
>       (StdoutLine s
>          \x -[lam51 toNext2]-> (toNext2 (
>                                         Ok x)))
>   
>   let lam111 = \x1 -[lam111]->
>     (await2 inLine2)
>       \firstName -[lam101]->
>         ((await2
>             (outLine2 "What's your last name?"))
>            \y -[lam91 firstName]->
>              ((await2 inLine2)
>                 \lastName -[lam81 firstName]->
>                   (outLine2
>                      str_concat "Hello "
>                        firstName " " lastName "!")))
>   
>   let lam71 = \s1 -[lam71 toNext3]->
>     toNext3 (Ok s1)
>   
>   let inLine2 = \toNext3 -[inLine2]->
>     StdinLine
>       \s1 -[lam71 toNext3]-> (toNext3 (Ok s1))
>   
>   let lam101 = \firstName -[lam101]->
>     (await2 (outLine2 "What's your last name?"))
>       \y -[lam91 firstName]->
>         ((await2 inLine2)
>            \lastName -[lam81 firstName]->
>              (outLine2
>                 str_concat "Hello " firstName " "
>                   lastName "!"))
>   
>   let lam91 = \y -[lam91 firstName]->
>     (await2 inLine2)
>       \lastName -[lam81 firstName]->
>         (outLine2
>            str_concat "Hello " firstName " "
>              lastName "!")
>   
>   let lam81 = \lastName -[lam81 firstName]->
>     outLine2
>       str_concat "Hello " firstName " " lastName
>         "!"
>   
>   let main1 =
>     (await2 (outLine2 "What's your first name?"))
>       \x1 -[lam111]->
>         ((await2 inLine2)
>            \firstName -[lam101]->
>              ((await2
>                  (outLine2
>                     "What's your last name?"))
>                 \y -[lam91 firstName]->
>                   ((await2 inLine2)
>                      \lastName -[lam81 firstName]->
>                        (outLine2
>                           str_concat "Hello "
>                             firstName " "
>                             lastName "!"))))
>   
>   let lam121 = \x2 -[lam121]-> Done x2
>   
>   let handle11 = \op1 -[handle11]->
>     \i -[lam141 handle op1]->
>       \t -[lam131 handle i op1]->
>         when op1 is
>           | StdinLinef ->
>             ((handle
>                 (f str_concat "stdin" itos i))
>                add i 1) (Stdin t)
>           | StdoutLines2 f1 ->
>             ((handle (f1 {})) add i 1)
>               (Stdout s2 t)
>           | Donex3 -> Done x3 t
>         end
>   
>   let lam141 = \i -[lam141 handle op1]->
>     \t -[lam131 handle i op1]->
>       when op1 is
>         | StdinLinef ->
>           ((handle (f str_concat "stdin" itos i))
>              add i 1) (Stdin t)
>         | StdoutLines2 f1 ->
>           ((handle (f1 {})) add i 1)
>             (Stdout s2 t)
>         | Donex3 -> Done x3 t
>       end
>   
>   let lam131 = \t -[lam131 handle i op1]->
>     when op1 is
>       | StdinLinef ->
>         ((handle (f str_concat "stdin" itos i))
>            add i 1) (Stdin t)
>       | StdoutLines2 f1 ->
>         ((handle (f1 {})) add i 1) (Stdout s2 t)
>       | Donex3 -> Done x3 t
>     end
>   
>   let main_handler =
>     let op = main1 \x2 -[lam121]-> (Done x2) in
>     let handle =
>       \op1 -[handle11]->
>         \i -[lam141 handle op1]->
>           \t -[lam131 handle i op1]->
>             when op1 is
>               | StdinLinef ->
>                 ((handle
>                     (f str_concat "stdin" itos i))
>                    add i 1) (Stdin t)
>               | StdoutLines2 f1 ->
>                 ((handle (f1 {})) add i 1)
>                   (Stdout s2 t)
>               | Donex3 -> Done x3 t
>             end
>     in
>     ((handle op) 0) (EntryPoint )
>   
>   
> entry_points:
>   main_handler

> cor-out +ir -print
> proc lam71(captures_13: box<erased>, s1: str):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box10: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_13 as box<{ { *fn, box<erased> } }>);
>   let captures_stack10: { { *fn, box<erased> } } = @get_boxed<captures_box10>;
>   let toNext3: { *fn, box<erased> } = @get_struct_field<captures_stack10, 0>;
>   let fnptr8: *fn = @get_struct_field<toNext3, 0>;
>   let captures8: box<erased> = @get_struct_field<toNext3, 1>;
>   let struct3: { str } = @make_struct{ s1 };
>   let var16: [ `0 { [] }, `1 { str } ] = @make_union<1, struct3>;
>   let var17:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr8, captures8, var16);
>   return var17;
> }
> 
> proc lam15(captures_3: box<erased>, toNext1: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box3: box<{ [] }> = @ptr_cast(captures_3 as box<{ [] }>);
>   let captures_stack3: { [] } = @get_boxed<captures_box3>;
>   let err: [] = @get_struct_field<captures_stack3, 0>;
>   let fnptr4: *fn = @get_struct_field<toNext1, 0>;
>   let captures4: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct: { [] } = @make_struct{ err };
>   let var4: [ `0 { [] }, `1 { {} } ] = @make_union<0, struct>;
>   let var5:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr4, captures4, var4);
>   return var5;
> }
> 
> proc lam131(
>   captures_21: box<erased>,
>    t: box<%type_1 = [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>):
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_1 =
>             [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>          ,
>         }
>   ]
> {
>   let captures_box18:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>
>     = @ptr_cast(
>         captures_21 as
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>);
>   let captures_stack18:
>         {
>          { *fn, box<erased> },
>           int,
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @get_boxed<captures_box18>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack18, 0>;
>   let i: int = @get_struct_field<captures_stack18, 1>;
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_struct_field<captures_stack18, 2>;
>   let discr1: int = @get_union_id<op1>;
>   switch discr1 {
>   0 -> {
>     let payload4: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<op1>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload4, 0>;
>     let struct8:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_1 =
>               [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct8>
>   }
>   1 -> {
>     let payload2: { { *fn, box<erased> } } = @get_union_struct<op1>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload2, 0>;
>     let fnptr18: *fn = @get_struct_field<handle, 0>;
>     let captures18: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr19: *fn = @get_struct_field<f, 0>;
>     let captures19: box<erased> = @get_struct_field<f, 1>;
>     let var41: str = "stdin";
>     let var42: str = @call_kfn(itos, i);
>     let var43: str = @call_kfn(str_concat, var41, var42);
>     let var44:
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>       = @call_indirect(fnptr19, captures19, var43);
>     let var45: { *fn, box<erased> }
>       = @call_indirect(fnptr18, captures18, var44);
>     let fnptr20: *fn = @get_struct_field<var45, 0>;
>     let captures20: box<erased> = @get_struct_field<var45, 1>;
>     let var46: int = 1;
>     let var47: int = @call_kfn(add, i, var46);
>     let var48: { *fn, box<erased> }
>       = @call_indirect(fnptr20, captures20, var47);
>     let fnptr21: *fn = @get_struct_field<var48, 0>;
>     let captures21: box<erased> = @get_struct_field<var48, 1>;
>     let struct6:
>           {
>            box<
>              %type_1 =
>              [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let unboxed:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_1 =
>                    [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_1> }
>           ]
>       = @make_union<1, struct6>;
>     let var49:
>           box<
>             %type_1 =
>             [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>       = @make_box(unboxed);
>     @call_indirect(fnptr21, captures21, var49)
>   }
>   2 -> {
>     let payload3: { str, { *fn, box<erased> } } = @get_union_struct<op1>;
>     let s2: str = @get_struct_field<payload3, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload3, 1>;
>     let fnptr22: *fn = @get_struct_field<handle, 0>;
>     let captures22: box<erased> = @get_struct_field<handle, 1>;
>     let fnptr23: *fn = @get_struct_field<f1, 0>;
>     let captures23: box<erased> = @get_struct_field<f1, 1>;
>     let var50: {} = @make_struct{};
>     let var51:
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>       = @call_indirect(fnptr23, captures23, var50);
>     let var52: { *fn, box<erased> }
>       = @call_indirect(fnptr22, captures22, var51);
>     let fnptr24: *fn = @get_struct_field<var52, 0>;
>     let captures24: box<erased> = @get_struct_field<var52, 1>;
>     let var53: int = 1;
>     let var54: int = @call_kfn(add, i, var53);
>     let var55: { *fn, box<erased> }
>       = @call_indirect(fnptr24, captures24, var54);
>     let fnptr25: *fn = @get_struct_field<var55, 0>;
>     let captures25: box<erased> = @get_struct_field<var55, 1>;
>     let struct7:
>           {
>            str,
>             box<
>               %type_1 =
>               [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let unboxed1:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_1 =
>                    [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_1> }
>           ]
>       = @make_union<2, struct7>;
>     let var56:
>           box<
>             %type_1 =
>             [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>       = @make_box(unboxed1);
>     @call_indirect(fnptr25, captures25, var56)
>   }
>   } in join join1;
>   return join1;
> }
> 
> proc lam121(captures_19: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_19 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let struct5: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let var38:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct5>;
>   return var38;
> }
> 
> proc lam51(captures_9: box<erased>, x: {}):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box7: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_9 as box<{ { *fn, box<erased> } }>);
>   let captures_stack7: { { *fn, box<erased> } } = @get_boxed<captures_box7>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack7, 0>;
>   let fnptr5: *fn = @get_struct_field<toNext2, 0>;
>   let captures5: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct2: { {} } = @make_struct{ x };
>   let var10: [ `0 { [] }, `1 { {} } ] = @make_union<1, struct2>;
>   let var11:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr5, captures5, var10);
>   return var11;
> }
> 
> proc clos_inLine2(captures_14: box<erased>, toNext3: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_14 as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let captures_stack_11: { { *fn, box<erased> } } = @make_struct{ toNext3 };
>   let captures_box_11: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_11);
>   let captures_29: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<lam71>;
>   let var18: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_29 };
>   let struct4: { { *fn, box<erased> } } = @make_struct{ var18 };
>   let var19:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct4>;
>   return var19;
> }
> 
> proc clos_fail2(captures_4: box<erased>, err: []): { *fn, box<erased> }
> {
>   let captures_box4: box<{}> = @ptr_cast(captures_4 as box<{}>);
>   let captures_stack4: {} = @get_boxed<captures_box4>;
>   let captures_stack_6: { [] } = @make_struct{ err };
>   let captures_box_6: box<{ [] }> = @make_box(captures_stack_6);
>   let captures_24: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<lam15>;
>   let var6: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_24 };
>   return var6;
> }
> 
> proc lam141(captures_20: box<erased>, i: int): { *fn, box<erased> }
> {
>   let captures_box17:
>         box<
>           {
>            { *fn, box<erased> },
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>
>     = @ptr_cast(
>         captures_20 as
>         box<
>           {
>            { *fn, box<erased> },
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>);
>   let captures_stack17:
>         {
>          { *fn, box<erased> },
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @get_boxed<captures_box17>;
>   let handle: { *fn, box<erased> } = @get_struct_field<captures_stack17, 0>;
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_struct_field<captures_stack17, 1>;
>   let captures_stack_16:
>         {
>          { *fn, box<erased> },
>           int,
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @make_struct{ handle, i, op1 };
>   let captures_box_16:
>         box<
>           {
>            { *fn, box<erased> },
>             int,
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>
>     = @make_box(captures_stack_16);
>   let captures_34: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<lam131>;
>   let var40: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_34 };
>   return var40;
> }
> 
> proc lam61(captures_8: box<erased>, toNext2: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box6: box<{ str }> = @ptr_cast(captures_8 as box<{ str }>);
>   let captures_stack6: { str } = @get_boxed<captures_box6>;
>   let s: str = @get_struct_field<captures_stack6, 0>;
>   let captures_stack_8: { { *fn, box<erased> } } = @make_struct{ toNext2 };
>   let captures_box_8: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_26: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<lam51>;
>   let var8: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_26 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s, var8 };
>   let var9:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct1>;
>   return var9;
> }
> 
> proc inLine2_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_3: {} = @make_struct{};
>   let captures_box_3: box<{}> = @make_box(captures_stack_3);
>   let captures_15: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos_inLine2>;
>   let inLine2_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_3, captures_15 };
>   return inLine2_closure;
> }
> 
> proc fail2_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_5: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_fail2>;
>   let fail2_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_, captures_5 };
>   return fail2_closure;
> }
> 
> proc handle11(
>   captures_handle: box<erased>,
>    op1:
>      [
>         `0 { [ `0 { [] }, `1 { {} } ] },
>         `1 { { *fn, box<erased> } },
>         `2 { str, { *fn, box<erased> } }
>      ]):
>   { *fn, box<erased> }
> {
>   let captures_box16: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack16: {} = @get_boxed<captures_box16>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle11>;
>   let handle: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let captures_stack_15:
>         {
>          { *fn, box<erased> },
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @make_struct{ handle, op1 };
>   let captures_box_15:
>         box<
>           {
>            { *fn, box<erased> },
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]
>            ,
>           }>
>     = @make_box(captures_stack_15);
>   let captures_33: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<lam141>;
>   let var39: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_33 };
>   return var39;
> }
> 
> proc clos_outLine2(captures_10: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box8: box<{}> = @ptr_cast(captures_10 as box<{}>);
>   let captures_stack8: {} = @get_boxed<captures_box8>;
>   let captures_stack_9: { str } = @make_struct{ s };
>   let captures_box_9: box<{ str }> = @make_box(captures_stack_9);
>   let captures_27: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<lam61>;
>   let var12: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_27 };
>   return var12;
> }
> 
> global inLine2: { *fn, box<erased> } = @call_direct(inLine2_thunk);
> 
> global fail2: { *fn, box<erased> } = @call_direct(fail2_thunk);
> 
> proc outLine2_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_11: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_outLine2>;
>   let outLine2_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_11 };
>   return outLine2_closure;
> }
> 
> proc lam21(captures_2: box<erased>, result: [ `0 { [] }, `1 { {} } ]):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box2: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_2 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box2>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack2, 1>;
>   let discr: int = @get_union_id<result>;
>   switch discr {
>   0 -> {
>     let payload1: { [] } = @get_union_struct<result>;
>     let e: [] = @get_struct_field<payload1, 0>;
>     let fnptr2: *fn = @get_struct_field<fail2, 0>;
>     let captures2: box<erased> = @get_struct_field<fail2, 1>;
>     @call_indirect(fnptr2, captures2, e)
>   }
>   1 -> {
>     let payload: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload, 0>;
>     let fnptr1: *fn = @get_struct_field<next, 0>;
>     let captures1: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr1, captures1, v)
>   }
>   } in join join;
>   let inner: { *fn, box<erased> } = join;
>   let fnptr3: *fn = @get_struct_field<inner, 0>;
>   let captures3: box<erased> = @get_struct_field<inner, 1>;
>   let var3:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr3, captures3, continue);
>   return var3;
> }
> 
> global outLine2: { *fn, box<erased> } = @call_direct(outLine2_thunk);
> 
> proc lam31(captures_1: box<erased>, continue: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
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
>   let captures_stack_5: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_5: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_5);
>   let captures_23: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<lam21>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_23 };
>   let var2:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr, captures, var1);
>   return var2;
> }
> 
> proc lam81(captures_18: box<erased>, lastName: str): { *fn, box<erased> }
> {
>   let captures_box14: box<{ str }> = @ptr_cast(captures_18 as box<{ str }>);
>   let captures_stack14: { str } = @get_boxed<captures_box14>;
>   let firstName: str = @get_struct_field<captures_stack14, 0>;
>   let fnptr14: *fn = @get_struct_field<outLine2, 0>;
>   let captures14: box<erased> = @get_struct_field<outLine2, 1>;
>   let var28: str = "Hello ";
>   let var29: str = " ";
>   let var30: str = "!";
>   let var31: str
>     = @call_kfn(str_concat, var28, firstName, var29, lastName, var30);
>   let var32: { *fn, box<erased> } = @call_indirect(fnptr14, captures14, var31);
>   return var32;
> }
> 
> proc lam41(captures_: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_ as box<{ { *fn, box<erased> } }>);
>   let captures_stack: { { *fn, box<erased> } } = @get_boxed<captures_box>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack, 0>;
>   let captures_stack_4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_4);
>   let captures_22: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<lam31>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_22 };
>   return var;
> }
> 
> proc clos_await2(captures_6: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box5: box<{}> = @ptr_cast(captures_6 as box<{}>);
>   let captures_stack5: {} = @get_boxed<captures_box5>;
>   let captures_stack_7: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_7: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_7);
>   let captures_25: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<lam41>;
>   let var7: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_25 };
>   return var7;
> }
> 
> proc await2_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_7: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_await2>;
>   let await2_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_7 };
>   return await2_closure;
> }
> 
> global await2: { *fn, box<erased> } = @call_direct(await2_thunk);
> 
> proc lam91(captures_17: box<erased>, y: {}): { *fn, box<erased> }
> {
>   let captures_box13: box<{ str }> = @ptr_cast(captures_17 as box<{ str }>);
>   let captures_stack13: { str } = @get_boxed<captures_box13>;
>   let firstName: str = @get_struct_field<captures_stack13, 0>;
>   let fnptr12: *fn = @get_struct_field<await2, 0>;
>   let captures12: box<erased> = @get_struct_field<await2, 1>;
>   let var25: { *fn, box<erased> }
>     = @call_indirect(fnptr12, captures12, inLine2);
>   let fnptr13: *fn = @get_struct_field<var25, 0>;
>   let captures13: box<erased> = @get_struct_field<var25, 1>;
>   let captures_stack_13: { str } = @make_struct{ firstName };
>   let captures_box_13: box<{ str }> = @make_box(captures_stack_13);
>   let captures_31: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<lam81>;
>   let var26: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_31 };
>   let var27: { *fn, box<erased> } = @call_indirect(fnptr13, captures13, var26);
>   return var27;
> }
> 
> proc lam101(captures_16: box<erased>, firstName: str): { *fn, box<erased> }
> {
>   let captures_box12: box<{}> = @ptr_cast(captures_16 as box<{}>);
>   let captures_stack12: {} = @get_boxed<captures_box12>;
>   let fnptr9: *fn = @get_struct_field<await2, 0>;
>   let captures9: box<erased> = @get_struct_field<await2, 1>;
>   let fnptr10: *fn = @get_struct_field<outLine2, 0>;
>   let captures10: box<erased> = @get_struct_field<outLine2, 1>;
>   let var20: str = "What's your last name?";
>   let var21: { *fn, box<erased> } = @call_indirect(fnptr10, captures10, var20);
>   let var22: { *fn, box<erased> } = @call_indirect(fnptr9, captures9, var21);
>   let fnptr11: *fn = @get_struct_field<var22, 0>;
>   let captures11: box<erased> = @get_struct_field<var22, 1>;
>   let captures_stack_12: { str } = @make_struct{ firstName };
>   let captures_box_12: box<{ str }> = @make_box(captures_stack_12);
>   let captures_30: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<lam91>;
>   let var23: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_30 };
>   let var24: { *fn, box<erased> } = @call_indirect(fnptr11, captures11, var23);
>   return var24;
> }
> 
> proc lam111(captures_12: box<erased>, x1: {}): { *fn, box<erased> }
> {
>   let captures_box9: box<{}> = @ptr_cast(captures_12 as box<{}>);
>   let captures_stack9: {} = @get_boxed<captures_box9>;
>   let fnptr6: *fn = @get_struct_field<await2, 0>;
>   let captures6: box<erased> = @get_struct_field<await2, 1>;
>   let var13: { *fn, box<erased> } = @call_indirect(fnptr6, captures6, inLine2);
>   let fnptr7: *fn = @get_struct_field<var13, 0>;
>   let captures7: box<erased> = @get_struct_field<var13, 1>;
>   let captures_stack_10: {} = @make_struct{};
>   let captures_box_10: box<{}> = @make_box(captures_stack_10);
>   let captures_28: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<lam101>;
>   let var14: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_28 };
>   let var15: { *fn, box<erased> } = @call_indirect(fnptr7, captures7, var14);
>   return var15;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr15: *fn = @get_struct_field<await2, 0>;
>   let captures15: box<erased> = @get_struct_field<await2, 1>;
>   let fnptr16: *fn = @get_struct_field<outLine2, 0>;
>   let captures16: box<erased> = @get_struct_field<outLine2, 1>;
>   let var33: str = "What's your first name?";
>   let var34: { *fn, box<erased> } = @call_indirect(fnptr16, captures16, var33);
>   let var35: { *fn, box<erased> } = @call_indirect(fnptr15, captures15, var34);
>   let fnptr17: *fn = @get_struct_field<var35, 0>;
>   let captures17: box<erased> = @get_struct_field<var35, 1>;
>   let captures_stack_14: {} = @make_struct{};
>   let captures_box_14: box<{}> = @make_box(captures_stack_14);
>   let captures_32: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<lam111>;
>   let var36: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_32 };
>   let var37: { *fn, box<erased> } = @call_indirect(fnptr17, captures17, var36);
>   return var37;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc main_handler_thunk():
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_1 =
>             [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>          ,
>         }
>   ]
> {
>   let fnptr26: *fn = @get_struct_field<main1, 0>;
>   let captures26: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_17: {} = @make_struct{};
>   let captures_box_17: box<{}> = @make_box(captures_stack_17);
>   let captures_35: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<lam121>;
>   let var57: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_35 };
>   let op:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr26, captures26, var57);
>   let captures_stack_18: {} = @make_struct{};
>   let captures_box_18: box<{}> = @make_box(captures_stack_18);
>   let captures_36: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<handle11>;
>   let handle: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_36 };
>   let fnptr27: *fn = @get_struct_field<handle, 0>;
>   let captures27: box<erased> = @get_struct_field<handle, 1>;
>   let var58: { *fn, box<erased> } = @call_indirect(fnptr27, captures27, op);
>   let fnptr28: *fn = @get_struct_field<var58, 0>;
>   let captures28: box<erased> = @get_struct_field<var58, 1>;
>   let var59: int = 0;
>   let var60: { *fn, box<erased> } = @call_indirect(fnptr28, captures28, var59);
>   let fnptr29: *fn = @get_struct_field<var60, 0>;
>   let captures29: box<erased> = @get_struct_field<var60, 1>;
>   let struct9: {} = @make_struct{};
>   let unboxed2:
>         [
>            `0 {},
>            `1 {
>                box<
>                  %type_1 =
>                  [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>                ,
>               },
>            `2 { str, box<%type_1> }
>         ]
>     = @make_union<0, struct9>;
>   let var61:
>         box<%type_1 = [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>     = @make_box(unboxed2);
>   let var62:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_1 =
>                   [ `0 {}, `1 { box<%type_1> }, `2 { str, box<%type_1> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr29, captures29, var61);
>   return var62;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_2 =
>             [ `0 {}, `1 { box<%type_2> }, `2 { str, box<%type_2> } ]>
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