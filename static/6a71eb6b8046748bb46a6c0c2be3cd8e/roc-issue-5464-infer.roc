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
>   let handle1 = \-[handle1]-> op1
>     \i -[lam191 handle1 op1]->
>       \t -[lam181 handle1 i op1]->
>         when op1 is
>           | StdinLinef ->
>             ((handle1
>                 (f str_concat "stdin" itos i))
>                add i 1) (Stdin t)
>           | StdoutLines2 f1 ->
>             ((handle1 (f1 {})) add i 1)
>               (Stdout s2 t)
>           | Donex3 -> Done x3 t
>         end
>   
>   let lam191 = \-[lam191 handle1 op1]-> i
>     \t -[lam181 handle1 i op1]->
>       when op1 is
>         | StdinLinef ->
>           ((handle1 (f str_concat "stdin" itos i))
>              add i 1) (Stdin t)
>         | StdoutLines2 f1 ->
>           ((handle1 (f1 {})) add i 1)
>             (Stdout s2 t)
>         | Donex3 -> Done x3 t
>       end
>   
>   let lam181 = \-[lam181 handle1 i op1]-> t
>     when op1 is
>       | StdinLinef ->
>         ((handle1 (f str_concat "stdin" itos i))
>            add i 1) (Stdin t)
>       | StdoutLines2 f1 ->
>         ((handle1 (f1 {})) add i 1) (Stdout s2 t)
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

> cor-out +ir -print
> proc lam111(captures_19: box<erased>, s1: str):
>   [
>      `0 { [ `0 { box<%type_17 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box14: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_19 as box<{ { *fn, box<erased> } }>);
>   let captures_stack14: { { *fn, box<erased> } } = @get_boxed<captures_box14>;
>   let toNext3: { *fn, box<erased> } = @get_struct_field<captures_stack14, 0>;
>   let fnptr12: *fn = @get_struct_field<toNext3, 0>;
>   let captures12: box<erased> = @get_struct_field<toNext3, 1>;
>   let struct5: { str } = @make_struct{ s1 };
>   let var22: [ `0 { box<%type_17 = []> }, `1 { str } ]
>     = @make_union<1, struct5>;
>   let var23:
>         [
>            `0 { [ `0 { box<%type_17 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr12, captures12, var22);
>   return var23;
> }
> 
> proc lam21(captures_6: box<erased>, toNext1: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { box<%type_13 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box5: box<{ box<%type_13 = []> }>
>     = @ptr_cast(captures_6 as box<{ box<%type_13 = []> }>);
>   let captures_stack5: { box<%type_13 = []> } = @get_boxed<captures_box5>;
>   let err: box<%type_13 = []> = @get_struct_field<captures_stack5, 0>;
>   let fnptr7: *fn = @get_struct_field<toNext1, 0>;
>   let captures7: box<erased> = @get_struct_field<toNext1, 1>;
>   let struct: { box<%type_13 = []> } = @make_struct{ err };
>   let var6: [ `0 { box<%type_13 = []> }, `1 { {} } ] = @make_union<0, struct>;
>   let var7:
>         [
>            `0 { [ `0 { box<%type_13 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr7, captures7, var6);
>   return var7;
> }
> 
> proc lam181(
>   captures_27: box<erased>,
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
>   let captures_box22:
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
>         captures_27 as
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
>   let captures_stack22:
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
>     = @get_boxed<captures_box22>;
>   let handle1: { *fn, box<erased> } = @get_struct_field<captures_stack22, 0>;
>   let i: int = @get_struct_field<captures_stack22, 1>;
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_struct_field<captures_stack22, 2>;
>   let discr2: int = @get_union_id<op1>;
>   switch discr2 {
>   0 -> {
>     let payload6: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<op1>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload6, 0>;
>     let struct10:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_14 =
>               [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct10>
>   }
>   1 -> {
>     let payload4: { { *fn, box<erased> } } = @get_union_struct<op1>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload4, 0>;
>     let fnptr22: *fn = @get_struct_field<handle1, 0>;
>     let captures22: box<erased> = @get_struct_field<handle1, 1>;
>     let fnptr23: *fn = @get_struct_field<f, 0>;
>     let captures23: box<erased> = @get_struct_field<f, 1>;
>     let var47: str = "stdin";
>     let var48: str = @call_kfn(itos, i);
>     let var49: str = @call_kfn(str_concat, var47, var48);
>     let var50:
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
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
>     let struct8:
>           {
>            box<
>              %type_14 =
>              [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let unboxed:
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
>       = @make_union<1, struct8>;
>     let var55:
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>       = @make_box(unboxed);
>     @call_indirect(fnptr25, captures25, var55)
>   }
>   2 -> {
>     let payload5: { str, { *fn, box<erased> } } = @get_union_struct<op1>;
>     let s2: str = @get_struct_field<payload5, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload5, 1>;
>     let fnptr26: *fn = @get_struct_field<handle1, 0>;
>     let captures26: box<erased> = @get_struct_field<handle1, 1>;
>     let fnptr27: *fn = @get_struct_field<f1, 0>;
>     let captures27: box<erased> = @get_struct_field<f1, 1>;
>     let var56: {} = @make_struct{};
>     let var57:
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
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
>     let struct9:
>           {
>            str,
>             box<
>               %type_14 =
>               [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let unboxed1:
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
>       = @make_union<2, struct9>;
>     let var62:
>           box<
>             %type_14 =
>             [ `0 {}, `1 { box<%type_14> }, `2 { str, box<%type_14> } ]>
>       = @make_box(unboxed1);
>     @call_indirect(fnptr29, captures29, var62)
>   }
>   } in join join2;
>   return join2;
> }
> 
> proc handle1(
>   captures_handle: box<erased>,
>    op1:
>      [
>         `0 { [ `0 { [] }, `1 { {} } ] },
>         `1 { { *fn, box<erased> } },
>         `2 { str, { *fn, box<erased> } }
>      ]):
>   { *fn, box<erased> }
> {
>   let captures_box20: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack20: {} = @get_boxed<captures_box20>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let captures_stack_19:
>         {
>          { *fn, box<erased> },
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @make_struct{ handle1, op1 };
>   let captures_box_19:
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
>     = @make_box(captures_stack_19);
>   let captures_41: box<erased> = @ptr_cast(captures_box_19 as box<erased>);
>   let fn_ptr_19: *fn = @make_fn_ptr<lam191>;
>   let var45: { *fn, box<erased> } = @make_struct{ fn_ptr_19, captures_41 };
>   return var45;
> }
> 
> proc lam191(captures_26: box<erased>, i: int): { *fn, box<erased> }
> {
>   let captures_box21:
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
>         captures_26 as
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
>   let captures_stack21:
>         {
>          { *fn, box<erased> },
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]
>          ,
>         }
>     = @get_boxed<captures_box21>;
>   let handle1: { *fn, box<erased> } = @get_struct_field<captures_stack21, 0>;
>   let op1:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_struct_field<captures_stack21, 1>;
>   let captures_stack_20:
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
>     = @make_struct{ handle1, i, op1 };
>   let captures_box_20:
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
>     = @make_box(captures_stack_20);
>   let captures_42: box<erased> = @ptr_cast(captures_box_20 as box<erased>);
>   let fn_ptr_20: *fn = @make_fn_ptr<lam181>;
>   let var46: { *fn, box<erased> } = @make_struct{ fn_ptr_20, captures_42 };
>   return var46;
> }
> 
> proc lam81(captures_13: box<erased>, x: {}):
>   [
>      `0 { [ `0 { box<%type_7 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box10: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_13 as box<{ { *fn, box<erased> } }>);
>   let captures_stack10: { { *fn, box<erased> } } = @get_boxed<captures_box10>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack10, 0>;
>   let fnptr9: *fn = @get_struct_field<toNext2, 0>;
>   let captures9: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct3: { {} } = @make_struct{ x };
>   let var14: [ `0 { box<%type_7 = []> }, `1 { {} } ] = @make_union<1, struct3>;
>   let var15:
>         [
>            `0 { [ `0 { box<%type_7 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr9, captures9, var14);
>   return var15;
> }
> 
> proc lam171(captures_25: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   [
>      `0 { [ `0 { [] }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box19: box<{}> = @ptr_cast(captures_25 as box<{}>);
>   let captures_stack19: {} = @get_boxed<captures_box19>;
>   let struct7: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let var44:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct7>;
>   return var44;
> }
> 
> proc lam82(captures_11: box<erased>, x: {}):
>   [
>      `0 { [ `0 { box<%type_16 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box8: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_11 as box<{ { *fn, box<erased> } }>);
>   let captures_stack8: { { *fn, box<erased> } } = @get_boxed<captures_box8>;
>   let toNext2: { *fn, box<erased> } = @get_struct_field<captures_stack8, 0>;
>   let fnptr8: *fn = @get_struct_field<toNext2, 0>;
>   let captures8: box<erased> = @get_struct_field<toNext2, 1>;
>   let struct1: { {} } = @make_struct{ x };
>   let var10: [ `0 { box<%type_16 = []> }, `1 { {} } ]
>     = @make_union<1, struct1>;
>   let var11:
>         [
>            `0 { [ `0 { box<%type_16 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr8, captures8, var10);
>   return var11;
> }
> 
> proc clos_inLine1(captures_20: box<erased>, toNext3: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { box<%type_18 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_20 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let captures_stack_15: { { *fn, box<erased> } } = @make_struct{ toNext3 };
>   let captures_box_15: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_15);
>   let captures_37: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<lam111>;
>   let var24: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_37 };
>   let struct6: { { *fn, box<erased> } } = @make_struct{ var24 };
>   let var25:
>         [
>            `0 { [ `0 { box<%type_18 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct6>;
>   return var25;
> }
> 
> proc clos_inLine(captures_16: box<erased>, toNext3: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { box<%type_6 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box12: box<{}> = @ptr_cast(captures_16 as box<{}>);
>   let captures_stack12: {} = @get_boxed<captures_box12>;
>   let captures_stack_13: { { *fn, box<erased> } } = @make_struct{ toNext3 };
>   let captures_box_13: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_13);
>   let captures_35: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<lam111>;
>   let var17: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_35 };
>   let struct4: { { *fn, box<erased> } } = @make_struct{ var17 };
>   let var18:
>         [
>            `0 { [ `0 { box<%type_6 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct4>;
>   return var18;
> }
> 
> proc clos_fail1(captures_7: box<erased>, err: box<%type_10 = []>):
>   { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_7 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_9: { box<%type_10 = []> } = @make_struct{ err };
>   let captures_box_9: box<{ box<%type_10 = []> }>
>     = @make_box(captures_stack_9);
>   let captures_31: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<lam21>;
>   let var8: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_31 };
>   return var8;
> }
> 
> proc clos_fail(captures_2: box<erased>, err: box<%type_12 = []>):
>   { *fn, box<erased> }
> {
>   let captures_box2: box<{}> = @ptr_cast(captures_2 as box<{}>);
>   let captures_stack2: {} = @get_boxed<captures_box2>;
>   let captures_stack_7: { box<%type_12 = []> } = @make_struct{ err };
>   let captures_box_7: box<{ box<%type_12 = []> }>
>     = @make_box(captures_stack_7);
>   let captures_29: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<lam21>;
>   let var2: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_29 };
>   return var2;
> }
> 
> proc lam91(captures_12: box<erased>, toNext2: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { box<%type_7 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box9: box<{ str }> = @ptr_cast(captures_12 as box<{ str }>);
>   let captures_stack9: { str } = @get_boxed<captures_box9>;
>   let s: str = @get_struct_field<captures_stack9, 0>;
>   let captures_stack_11: { { *fn, box<erased> } } = @make_struct{ toNext2 };
>   let captures_box_11: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_11);
>   let captures_33: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<lam82>;
>   let var12: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_33 };
>   let struct2: { str, { *fn, box<erased> } } = @make_struct{ s, var12 };
>   let var13:
>         [
>            `0 { [ `0 { box<%type_7 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<2, struct2>;
>   return var13;
> }
> 
> proc inLine_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_5: {} = @make_struct{};
>   let captures_box_5: box<{}> = @make_box(captures_stack_5);
>   let captures_21: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos_inLine1>;
>   let inLine_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_5, captures_21 };
>   return inLine_closure1;
> }
> 
> proc inLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_4: {} = @make_struct{};
>   let captures_box_4: box<{}> = @make_box(captures_stack_4);
>   let captures_17: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos_inLine>;
>   let inLine_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_4, captures_17 };
>   return inLine_closure;
> }
> 
> proc fail_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_8: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_fail1>;
>   let fail_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_8 };
>   return fail_closure1;
> }
> 
> proc fail_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_3: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_fail>;
>   let fail_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_3 };
>   return fail_closure;
> }
> 
> proc clos_outLine(captures_14: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_14 as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let captures_stack_12: { str } = @make_struct{ s };
>   let captures_box_12: box<{ str }> = @make_box(captures_stack_12);
>   let captures_34: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<lam91>;
>   let var16: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_34 };
>   return var16;
> }
> 
> global inLine1: { *fn, box<erased> } = @call_direct(inLine_thunk1);
> 
> global inLine2: { *fn, box<erased> } = @call_direct(inLine_thunk);
> 
> global fail1: { *fn, box<erased> } = @call_direct(fail_thunk1);
> 
> global fail2: { *fn, box<erased> } = @call_direct(fail_thunk);
> 
> proc outLine_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_3: {} = @make_struct{};
>   let captures_box_3: box<{}> = @make_box(captures_stack_3);
>   let captures_15: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos_outLine>;
>   let outLine_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_3, captures_15 };
>   return outLine_closure;
> }
> 
> proc lam42(
>   captures_1: box<erased>,
>    result: [ `0 { box<%type_12 = []> }, `1 { {} } ]):
>   [
>      `0 { [ `0 { box<%type_12 = []> }, `1 { {} } ] },
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
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack1, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack1, 1>;
>   let discr: int = @get_union_id<result>;
>   switch discr {
>   0 -> {
>     let payload1: { box<%type_12 = []> } = @get_union_struct<result>;
>     let e: box<%type_12 = []> = @get_struct_field<payload1, 0>;
>     let fnptr1: *fn = @get_struct_field<fail2, 0>;
>     let captures1: box<erased> = @get_struct_field<fail2, 1>;
>     @call_indirect(fnptr1, captures1, e)
>   }
>   1 -> {
>     let payload: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload, 0>;
>     let fnptr: *fn = @get_struct_field<next, 0>;
>     let captures: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr, captures, v)
>   }
>   } in join join;
>   let inner: { *fn, box<erased> } = join;
>   let fnptr2: *fn = @get_struct_field<inner, 0>;
>   let captures2: box<erased> = @get_struct_field<inner, 1>;
>   let var1:
>         [
>            `0 { [ `0 { box<%type_12 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr2, captures2, continue);
>   return var1;
> }
> 
> proc lam41(
>   captures_5: box<erased>,
>    result: [ `0 { box<%type_10 = []> }, `1 { {} } ]):
>   [
>      `0 { [ `0 { box<%type_10 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_5 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box4>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack4, 1>;
>   let discr1: int = @get_union_id<result>;
>   switch discr1 {
>   0 -> {
>     let payload3: { box<%type_10 = []> } = @get_union_struct<result>;
>     let e: box<%type_10 = []> = @get_struct_field<payload3, 0>;
>     let fnptr5: *fn = @get_struct_field<fail2, 0>;
>     let captures5: box<erased> = @get_struct_field<fail2, 1>;
>     @call_indirect(fnptr5, captures5, e)
>   }
>   1 -> {
>     let payload2: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload2, 0>;
>     let fnptr4: *fn = @get_struct_field<next, 0>;
>     let captures4: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr4, captures4, v)
>   }
>   } in join join1;
>   let inner: { *fn, box<erased> } = join1;
>   let fnptr6: *fn = @get_struct_field<inner, 0>;
>   let captures6: box<erased> = @get_struct_field<inner, 1>;
>   let var5:
>         [
>            `0 { [ `0 { box<%type_10 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr6, captures6, continue);
>   return var5;
> }
> 
> global outLine1: { *fn, box<erased> } = @call_direct(outLine_thunk);
> 
> proc lam51(captures_4: box<erased>, continue: { *fn, box<erased> }):
>   [
>      `0 { [ `0 { box<%type_10 = []> }, `1 { {} } ] },
>      `1 { { *fn, box<erased> } },
>      `2 { str, { *fn, box<erased> } }
>   ]
> {
>   let captures_box3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_4 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box3>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack3, 1>;
>   let fnptr3: *fn = @get_struct_field<fromResult, 0>;
>   let captures3: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_8: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_8: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_30: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<lam42>;
>   let var3: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_30 };
>   let var4:
>         [
>            `0 { [ `0 { box<%type_10 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr3, captures3, var3);
>   return var4;
> }
> 
> proc lam131(captures_24: box<erased>, lastName: str): { *fn, box<erased> }
> {
>   let captures_box18: box<{ str }> = @ptr_cast(captures_24 as box<{ str }>);
>   let captures_stack18: { str } = @get_boxed<captures_box18>;
>   let firstName: str = @get_struct_field<captures_stack18, 0>;
>   let fnptr18: *fn = @get_struct_field<outLine1, 0>;
>   let captures18: box<erased> = @get_struct_field<outLine1, 1>;
>   let var34: str = "Hello ";
>   let var35: str = " ";
>   let var36: str = "!";
>   let var37: str
>     = @call_kfn(str_concat, var34, firstName, var35, lastName, var36);
>   let var38: { *fn, box<erased> } = @call_indirect(fnptr18, captures18, var37);
>   return var38;
> }
> 
> proc lam61(captures_: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_ as box<{ { *fn, box<erased> } }>);
>   let captures_stack: { { *fn, box<erased> } } = @get_boxed<captures_box>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack, 0>;
>   let captures_stack_6: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_6: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_6);
>   let captures_28: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<lam51>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_28 };
>   return var;
> }
> 
> proc clos_await(captures_9: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box7: box<{}> = @ptr_cast(captures_9 as box<{}>);
>   let captures_stack7: {} = @get_boxed<captures_box7>;
>   let captures_stack_10: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_10: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_10);
>   let captures_32: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<lam61>;
>   let var9: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_32 };
>   return var9;
> }
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_10: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_await>;
>   let await_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_10 };
>   return await_closure;
> }
> 
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc lam141(captures_23: box<erased>, y: {}): { *fn, box<erased> }
> {
>   let captures_box17: box<{ str }> = @ptr_cast(captures_23 as box<{ str }>);
>   let captures_stack17: { str } = @get_boxed<captures_box17>;
>   let firstName: str = @get_struct_field<captures_stack17, 0>;
>   let fnptr16: *fn = @get_struct_field<await1, 0>;
>   let captures16: box<erased> = @get_struct_field<await1, 1>;
>   let var31: { *fn, box<erased> }
>     = @call_indirect(fnptr16, captures16, inLine2);
>   let fnptr17: *fn = @get_struct_field<var31, 0>;
>   let captures17: box<erased> = @get_struct_field<var31, 1>;
>   let captures_stack_17: { str } = @make_struct{ firstName };
>   let captures_box_17: box<{ str }> = @make_box(captures_stack_17);
>   let captures_39: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<lam131>;
>   let var32: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_39 };
>   let var33: { *fn, box<erased> } = @call_indirect(fnptr17, captures17, var32);
>   return var33;
> }
> 
> proc lam151(captures_22: box<erased>, firstName: str): { *fn, box<erased> }
> {
>   let captures_box16: box<{}> = @ptr_cast(captures_22 as box<{}>);
>   let captures_stack16: {} = @get_boxed<captures_box16>;
>   let fnptr13: *fn = @get_struct_field<await1, 0>;
>   let captures13: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr14: *fn = @get_struct_field<outLine1, 0>;
>   let captures14: box<erased> = @get_struct_field<outLine1, 1>;
>   let var26: str = "What's your last name?";
>   let var27: { *fn, box<erased> } = @call_indirect(fnptr14, captures14, var26);
>   let var28: { *fn, box<erased> } = @call_indirect(fnptr13, captures13, var27);
>   let fnptr15: *fn = @get_struct_field<var28, 0>;
>   let captures15: box<erased> = @get_struct_field<var28, 1>;
>   let captures_stack_16: { str } = @make_struct{ firstName };
>   let captures_box_16: box<{ str }> = @make_box(captures_stack_16);
>   let captures_38: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<lam141>;
>   let var29: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_38 };
>   let var30: { *fn, box<erased> } = @call_indirect(fnptr15, captures15, var29);
>   return var30;
> }
> 
> proc lam161(captures_18: box<erased>, x1: {}): { *fn, box<erased> }
> {
>   let captures_box13: box<{}> = @ptr_cast(captures_18 as box<{}>);
>   let captures_stack13: {} = @get_boxed<captures_box13>;
>   let fnptr10: *fn = @get_struct_field<await1, 0>;
>   let captures10: box<erased> = @get_struct_field<await1, 1>;
>   let var19: { *fn, box<erased> }
>     = @call_indirect(fnptr10, captures10, inLine2);
>   let fnptr11: *fn = @get_struct_field<var19, 0>;
>   let captures11: box<erased> = @get_struct_field<var19, 1>;
>   let captures_stack_14: {} = @make_struct{};
>   let captures_box_14: box<{}> = @make_box(captures_stack_14);
>   let captures_36: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<lam151>;
>   let var20: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_36 };
>   let var21: { *fn, box<erased> } = @call_indirect(fnptr11, captures11, var20);
>   return var21;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr19: *fn = @get_struct_field<await1, 0>;
>   let captures19: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr20: *fn = @get_struct_field<outLine1, 0>;
>   let captures20: box<erased> = @get_struct_field<outLine1, 1>;
>   let var39: str = "What's your first name?";
>   let var40: { *fn, box<erased> } = @call_indirect(fnptr20, captures20, var39);
>   let var41: { *fn, box<erased> } = @call_indirect(fnptr19, captures19, var40);
>   let fnptr21: *fn = @get_struct_field<var41, 0>;
>   let captures21: box<erased> = @get_struct_field<var41, 1>;
>   let captures_stack_18: {} = @make_struct{};
>   let captures_box_18: box<{}> = @make_box(captures_stack_18);
>   let captures_40: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<lam161>;
>   let var42: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_40 };
>   let var43: { *fn, box<erased> } = @call_indirect(fnptr21, captures21, var42);
>   return var43;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
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
>   let fnptr30: *fn = @get_struct_field<main1, 0>;
>   let captures30: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_21: {} = @make_struct{};
>   let captures_box_21: box<{}> = @make_box(captures_stack_21);
>   let captures_43: box<erased> = @ptr_cast(captures_box_21 as box<erased>);
>   let fn_ptr_21: *fn = @make_fn_ptr<lam171>;
>   let var63: { *fn, box<erased> } = @make_struct{ fn_ptr_21, captures_43 };
>   let op:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @call_indirect(fnptr30, captures30, var63);
>   let captures_stack_22: {} = @make_struct{};
>   let captures_box_22: box<{}> = @make_box(captures_stack_22);
>   let captures_44: box<erased> = @ptr_cast(captures_box_22 as box<erased>);
>   let fn_ptr_22: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> } = @make_struct{ fn_ptr_22, captures_44 };
>   let fnptr31: *fn = @get_struct_field<handle1, 0>;
>   let captures31: box<erased> = @get_struct_field<handle1, 1>;
>   let var64: { *fn, box<erased> } = @call_indirect(fnptr31, captures31, op);
>   let fnptr32: *fn = @get_struct_field<var64, 0>;
>   let captures32: box<erased> = @get_struct_field<var64, 1>;
>   let var65: int = 0;
>   let var66: { *fn, box<erased> } = @call_indirect(fnptr32, captures32, var65);
>   let fnptr33: *fn = @get_struct_field<var66, 0>;
>   let captures33: box<erased> = @get_struct_field<var66, 1>;
>   let struct11: {} = @make_struct{};
>   let unboxed2:
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
>     = @make_union<0, struct11>;
>   let var67:
>         box<%type_0 = [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>     = @make_box(unboxed2);
>   let var68:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_0 =
>                   [ `0 {}, `1 { box<%type_0> }, `2 { str, box<%type_0> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr33, captures33, var67);
>   return var68;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_15 =
>             [ `0 {}, `1 { box<%type_15> }, `2 { str, box<%type_15> } ]>
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