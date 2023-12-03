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
> proc lam111(captures_19: box<erased>, s1: str):
>   box<
>     %type_93 =
>     [
>        `0 { [ `0 { box<%type_92 = []> }, `1 { {} } ] },
>        `1 { box<%type_73 = { *fn, box<erased> }> },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box14: box<{ box<%type_74 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_19 as box<{ box<%type_74 = { *fn, box<erased> }> }>);
>   let captures_stack14: { box<%type_74 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box14>;
>   let toNext3: box<%type_74 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack14, 0>;
>   let inner6: { *fn, box<erased> } = @get_boxed<toNext3>;
>   let fnptr12: *fn = @get_struct_field<inner6, 0>;
>   let captures12: box<erased> = @get_struct_field<inner6, 1>;
>   let struct5: { str } = @make_struct{ s1 };
>   let var22: [ `0 { box<%type_92 = []> }, `1 { str } ]
>     = @make_union<1, struct5>;
>   let var23:
>         box<
>           %type_93 =
>           [
>              `0 { [ `0 { box<%type_92 = []> }, `1 { {} } ] },
>              `1 { box<%type_73 = { *fn, box<erased> }> },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr12, captures12, var22);
>   return var23;
> }
> 
> proc lam21(
>   captures_7: box<erased>,
>    toNext1: box<%type_68 = { *fn, box<erased> }>):
>   box<
>     %type_88 =
>     [
>        `0 { [ `0 { box<%type_67 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box6: box<{ box<%type_67 = []> }>
>     = @ptr_cast(captures_7 as box<{ box<%type_67 = []> }>);
>   let captures_stack6: { box<%type_67 = []> } = @get_boxed<captures_box6>;
>   let err: box<%type_67 = []> = @get_struct_field<captures_stack6, 0>;
>   let inner4: { *fn, box<erased> } = @get_boxed<toNext1>;
>   let fnptr8: *fn = @get_struct_field<inner4, 0>;
>   let captures8: box<erased> = @get_struct_field<inner4, 1>;
>   let struct1: { box<%type_67 = []> } = @make_struct{ err };
>   let var8: [ `0 { box<%type_67 = []> }, `1 { {} } ] = @make_union<0, struct1>;
>   let var9:
>         box<
>           %type_88 =
>           [
>              `0 { [ `0 { box<%type_67 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr8, captures8, var8);
>   return var9;
> }
> 
> proc handle2(
>   captures_handle1: box<erased>,
>    op1:
>      box<
>        %type_14 =
>        [
>           `0 { [ `0 { [] }, `1 { {} } ] },
>           `1 { { *fn, box<erased> } },
>           `2 { str, { *fn, box<erased> } }
>        ]>):
>   box<%type_15 = { *fn, box<erased> }>
> {
>   let captures_box23: box<{}> = @ptr_cast(captures_handle1 as box<{}>);
>   let captures_stack23: {} = @get_boxed<captures_box23>;
>   let rec_fn_ptr_handle1: *fn = @make_fn_ptr<handle2>;
>   let unboxed15: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle1, captures_handle1 };
>   let handle2: box<%type_81 = { *fn, box<erased> }> = @make_box(unboxed15);
>   let captures_stack_24:
>         {
>          box<%type_81 = { *fn, box<erased> }>,
>           box<
>             %type_14 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle2, op1 };
>   let captures_box_24:
>         box<
>           {
>            box<%type_81 = { *fn, box<erased> }>,
>             box<
>               %type_14 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_24);
>   let captures_48: box<erased> = @ptr_cast(captures_box_24 as box<erased>);
>   let fn_ptr_24: *fn = @make_fn_ptr<lam191>;
>   let unboxed16: { *fn, box<erased> } = @make_struct{ fn_ptr_24, captures_48 };
>   let var49: box<%type_15 = { *fn, box<erased> }> = @make_box(unboxed16);
>   return var49;
> }
> 
> proc lam181(
>   captures_31: box<erased>,
>    t:
>      box<%type_83 = [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>):
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_83 =
>             [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>          ,
>         }
>   ]
> {
>   let captures_box25:
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             int,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_31 as
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             int,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack25:
>         {
>          box<%type_80 = { *fn, box<erased> }>,
>           int,
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box25>;
>   let handle2: box<%type_80 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack25, 0>;
>   let i: int = @get_struct_field<captures_stack25, 1>;
>   let op1:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack25, 2>;
>   let inner7:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @get_boxed<op1>;
>   let discr2: int = @get_union_id<inner7>;
>   switch discr2 {
>   0 -> {
>     let payload6: { [ `0 { [] }, `1 { {} } ] } = @get_union_struct<inner7>;
>     let x3: [ `0 { [] }, `1 { {} } ] = @get_struct_field<payload6, 0>;
>     let struct11:
>           {
>            [ `0 { [] }, `1 { {} } ],
>             box<
>               %type_83 =
>               [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>            ,
>           }
>       = @make_struct{ x3, t };
>     @make_union<0, struct11>
>   }
>   1 -> {
>     let payload4: { { *fn, box<erased> } } = @get_union_struct<inner7>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload4, 0>;
>     let inner8: { *fn, box<erased> } = @get_boxed<handle2>;
>     let fnptr22: *fn = @get_struct_field<inner8, 0>;
>     let captures22: box<erased> = @get_struct_field<inner8, 1>;
>     let fnptr23: *fn = @get_struct_field<f, 0>;
>     let captures23: box<erased> = @get_struct_field<f, 1>;
>     let var51: str = "stdin";
>     let var52: str = @call_kfn(itos, i);
>     let var53: str = @call_kfn(str_concat, var51, var52);
>     let var54:
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr23, captures23, var53);
>     let var55: box<%type_12 = { *fn, box<erased> }>
>       = @call_indirect(fnptr22, captures22, var54);
>     let inner9: { *fn, box<erased> } = @get_boxed<var55>;
>     let fnptr24: *fn = @get_struct_field<inner9, 0>;
>     let captures24: box<erased> = @get_struct_field<inner9, 1>;
>     let var56: int = 1;
>     let var57: int = @call_kfn(add, i, var56);
>     let var58: box<%type_82 = { *fn, box<erased> }>
>       = @call_indirect(fnptr24, captures24, var57);
>     let inner10: { *fn, box<erased> } = @get_boxed<var58>;
>     let fnptr25: *fn = @get_struct_field<inner10, 0>;
>     let captures25: box<erased> = @get_struct_field<inner10, 1>;
>     let struct9:
>           {
>            box<
>              %type_83 =
>              [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>            ,
>           }
>       = @make_struct{ t };
>     let unboxed18:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_83 =
>                    [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_83> }
>           ]
>       = @make_union<1, struct9>;
>     let var59:
>           box<
>             %type_83 =
>             [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>       = @make_box(unboxed18);
>     @call_indirect(fnptr25, captures25, var59)
>   }
>   2 -> {
>     let payload5: { str, { *fn, box<erased> } } = @get_union_struct<inner7>;
>     let s2: str = @get_struct_field<payload5, 0>;
>     let f1: { *fn, box<erased> } = @get_struct_field<payload5, 1>;
>     let inner11: { *fn, box<erased> } = @get_boxed<handle2>;
>     let fnptr26: *fn = @get_struct_field<inner11, 0>;
>     let captures26: box<erased> = @get_struct_field<inner11, 1>;
>     let fnptr27: *fn = @get_struct_field<f1, 0>;
>     let captures27: box<erased> = @get_struct_field<f1, 1>;
>     let var60: {} = @make_struct{};
>     let var61:
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>       = @call_indirect(fnptr27, captures27, var60);
>     let var62: box<%type_12 = { *fn, box<erased> }>
>       = @call_indirect(fnptr26, captures26, var61);
>     let inner12: { *fn, box<erased> } = @get_boxed<var62>;
>     let fnptr28: *fn = @get_struct_field<inner12, 0>;
>     let captures28: box<erased> = @get_struct_field<inner12, 1>;
>     let var63: int = 1;
>     let var64: int = @call_kfn(add, i, var63);
>     let var65: box<%type_82 = { *fn, box<erased> }>
>       = @call_indirect(fnptr28, captures28, var64);
>     let inner13: { *fn, box<erased> } = @get_boxed<var65>;
>     let fnptr29: *fn = @get_struct_field<inner13, 0>;
>     let captures29: box<erased> = @get_struct_field<inner13, 1>;
>     let struct10:
>           {
>            str,
>             box<
>               %type_83 =
>               [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>            ,
>           }
>       = @make_struct{ s2, t };
>     let unboxed19:
>           [
>              `0 {},
>              `1 {
>                  box<
>                    %type_83 =
>                    [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>                  ,
>                 },
>              `2 { str, box<%type_83> }
>           ]
>       = @make_union<2, struct10>;
>     let var66:
>           box<
>             %type_83 =
>             [ `0 {}, `1 { box<%type_83> }, `2 { str, box<%type_83> } ]>
>       = @make_box(unboxed19);
>     @call_indirect(fnptr29, captures29, var66)
>   }
>   } in join join2;
>   return join2;
> }
> 
> proc lam191(captures_30: box<erased>, i: int):
>   box<%type_82 = { *fn, box<erased> }>
> {
>   let captures_box24:
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @ptr_cast(
>         captures_30 as
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>);
>   let captures_stack24:
>         {
>          box<%type_80 = { *fn, box<erased> }>,
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @get_boxed<captures_box24>;
>   let handle2: box<%type_80 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack24, 0>;
>   let op1:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @get_struct_field<captures_stack24, 1>;
>   let captures_stack_25:
>         {
>          box<%type_80 = { *fn, box<erased> }>,
>           int,
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle2, i, op1 };
>   let captures_box_25:
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             int,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_25);
>   let captures_49: box<erased> = @ptr_cast(captures_box_25 as box<erased>);
>   let fn_ptr_25: *fn = @make_fn_ptr<lam181>;
>   let unboxed17: { *fn, box<erased> } = @make_struct{ fn_ptr_25, captures_49 };
>   let var50: box<%type_82 = { *fn, box<erased> }> = @make_box(unboxed17);
>   return var50;
> }
> 
> proc lam81(captures_13: box<erased>, x: {}):
>   box<
>     %type_90 =
>     [
>        `0 { [ `0 { box<%type_89 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_70 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box10: box<{ box<%type_69 = { *fn, box<erased> }> }>
>     = @ptr_cast(captures_13 as box<{ box<%type_69 = { *fn, box<erased> }> }>);
>   let captures_stack10: { box<%type_69 = { *fn, box<erased> }> }
>     = @get_boxed<captures_box10>;
>   let toNext2: box<%type_69 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack10, 0>;
>   let inner5: { *fn, box<erased> } = @get_boxed<toNext2>;
>   let fnptr9: *fn = @get_struct_field<inner5, 0>;
>   let captures9: box<erased> = @get_struct_field<inner5, 1>;
>   let struct3: { {} } = @make_struct{ x };
>   let var14: [ `0 { box<%type_89 = []> }, `1 { {} } ]
>     = @make_union<1, struct3>;
>   let var15:
>         box<
>           %type_90 =
>           [
>              `0 { [ `0 { box<%type_89 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_70 = { *fn, box<erased> }> }
>           ]>
>     = @call_indirect(fnptr9, captures9, var14);
>   return var15;
> }
> 
> proc lam171(captures_29: box<erased>, x2: [ `0 { [] }, `1 { {} } ]):
>   box<
>     %type_11 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box21: box<{}> = @ptr_cast(captures_29 as box<{}>);
>   let captures_stack21: {} = @get_boxed<captures_box21>;
>   let struct8: { [ `0 { [] }, `1 { {} } ] } = @make_struct{ x2 };
>   let unboxed12:
>         [
>            `0 { [ `0 { [] }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<0, struct8>;
>   let var47:
>         box<
>           %type_11 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed12);
>   return var47;
> }
> 
> proc lam22(
>   captures_2: box<erased>,
>    toNext1: box<%type_63 = { *fn, box<erased> }>):
>   box<
>     %type_86 =
>     [
>        `0 { [ `0 { box<%type_62 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box2: box<{ box<%type_62 = []> }>
>     = @ptr_cast(captures_2 as box<{ box<%type_62 = []> }>);
>   let captures_stack2: { box<%type_62 = []> } = @get_boxed<captures_box2>;
>   let err: box<%type_62 = []> = @get_struct_field<captures_stack2, 0>;
>   let inner2: { *fn, box<erased> } = @get_boxed<toNext1>;
>   let fnptr3: *fn = @get_struct_field<inner2, 0>;
>   let captures3: box<erased> = @get_struct_field<inner2, 1>;
>   let struct: { box<%type_62 = []> } = @make_struct{ err };
>   let var2: [ `0 { box<%type_62 = []> }, `1 { {} } ] = @make_union<0, struct>;
>   let var3:
>         box<
>           %type_86 =
>           [
>              `0 { [ `0 { box<%type_62 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr3, captures3, var2);
>   return var3;
> }
> 
> proc clos_inLine2(
>   captures_22: box<erased>,
>    toNext3: box<%type_76 = { *fn, box<erased> }>):
>   box<
>     %type_42 =
>     [
>        `0 { [ `0 { box<%type_41 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box16: box<{}> = @ptr_cast(captures_22 as box<{}>);
>   let captures_stack16: {} = @get_boxed<captures_box16>;
>   let captures_stack_18: { box<%type_76 = { *fn, box<erased> }> }
>     = @make_struct{ toNext3 };
>   let captures_box_18: box<{ box<%type_76 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_18);
>   let captures_42: box<erased> = @ptr_cast(captures_box_18 as box<erased>);
>   let fn_ptr_18: *fn = @make_fn_ptr<lam111>;
>   let var26: { *fn, box<erased> } = @make_struct{ fn_ptr_18, captures_42 };
>   let struct7: { { *fn, box<erased> } } = @make_struct{ var26 };
>   let unboxed8:
>         [
>            `0 { [ `0 { box<%type_41 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct7>;
>   let var27:
>         box<
>           %type_42 =
>           [
>              `0 { [ `0 { box<%type_41 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed8);
>   return var27;
> }
> 
> proc clos_inLine1(
>   captures_20: box<erased>,
>    toNext3: box<%type_75 = { *fn, box<erased> }>):
>   box<
>     %type_40 =
>     [
>        `0 { [ `0 { box<%type_39 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box15: box<{}> = @ptr_cast(captures_20 as box<{}>);
>   let captures_stack15: {} = @get_boxed<captures_box15>;
>   let captures_stack_17: { box<%type_75 = { *fn, box<erased> }> }
>     = @make_struct{ toNext3 };
>   let captures_box_17: box<{ box<%type_75 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_17);
>   let captures_41: box<erased> = @ptr_cast(captures_box_17 as box<erased>);
>   let fn_ptr_17: *fn = @make_fn_ptr<lam111>;
>   let var24: { *fn, box<erased> } = @make_struct{ fn_ptr_17, captures_41 };
>   let struct6: { { *fn, box<erased> } } = @make_struct{ var24 };
>   let unboxed7:
>         [
>            `0 { [ `0 { box<%type_39 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct6>;
>   let var25:
>         box<
>           %type_40 =
>           [
>              `0 { [ `0 { box<%type_39 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed7);
>   return var25;
> }
> 
> proc clos_inLine(
>   captures_16: box<erased>,
>    toNext3: box<%type_71 = { *fn, box<erased> }>):
>   box<
>     %type_44 =
>     [
>        `0 { [ `0 { box<%type_43 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box12: box<{}> = @ptr_cast(captures_16 as box<{}>);
>   let captures_stack12: {} = @get_boxed<captures_box12>;
>   let captures_stack_15: { box<%type_71 = { *fn, box<erased> }> }
>     = @make_struct{ toNext3 };
>   let captures_box_15: box<{ box<%type_71 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_15);
>   let captures_39: box<erased> = @ptr_cast(captures_box_15 as box<erased>);
>   let fn_ptr_15: *fn = @make_fn_ptr<lam111>;
>   let var17: { *fn, box<erased> } = @make_struct{ fn_ptr_15, captures_39 };
>   let struct4: { { *fn, box<erased> } } = @make_struct{ var17 };
>   let unboxed5:
>         [
>            `0 { [ `0 { box<%type_43 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, { *fn, box<erased> } }
>         ]
>     = @make_union<1, struct4>;
>   let var18:
>         box<
>           %type_44 =
>           [
>              `0 { [ `0 { box<%type_43 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @make_box(unboxed5);
>   return var18;
> }
> 
> proc clos_fail1(captures_8: box<erased>, err: box<%type_65 = []>):
>   box<%type_87 = { *fn, box<erased> }>
> {
>   let captures_box7: box<{}> = @ptr_cast(captures_8 as box<{}>);
>   let captures_stack7: {} = @get_boxed<captures_box7>;
>   let captures_stack_11: { box<%type_65 = []> } = @make_struct{ err };
>   let captures_box_11: box<{ box<%type_65 = []> }>
>     = @make_box(captures_stack_11);
>   let captures_35: box<erased> = @ptr_cast(captures_box_11 as box<erased>);
>   let fn_ptr_11: *fn = @make_fn_ptr<lam21>;
>   let unboxed2: { *fn, box<erased> } = @make_struct{ fn_ptr_11, captures_35 };
>   let var10: box<%type_87 = { *fn, box<erased> }> = @make_box(unboxed2);
>   return var10;
> }
> 
> proc handle1(
>   captures_handle: box<erased>,
>    op1:
>      box<
>        %type_11 =
>        [
>           `0 { [ `0 { [] }, `1 { {} } ] },
>           `1 { { *fn, box<erased> } },
>           `2 { str, { *fn, box<erased> } }
>        ]>):
>   box<%type_12 = { *fn, box<erased> }>
> {
>   let captures_box22: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack22: {} = @get_boxed<captures_box22>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let unboxed13: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let handle1: box<%type_80 = { *fn, box<erased> }> = @make_box(unboxed13);
>   let captures_stack_23:
>         {
>          box<%type_80 = { *fn, box<erased> }>,
>           box<
>             %type_11 =
>             [
>                `0 { [ `0 { [] }, `1 { {} } ] },
>                `1 { { *fn, box<erased> } },
>                `2 { str, { *fn, box<erased> } }
>             ]>
>          ,
>         }
>     = @make_struct{ handle1, op1 };
>   let captures_box_23:
>         box<
>           {
>            box<%type_80 = { *fn, box<erased> }>,
>             box<
>               %type_11 =
>               [
>                  `0 { [ `0 { [] }, `1 { {} } ] },
>                  `1 { { *fn, box<erased> } },
>                  `2 { str, { *fn, box<erased> } }
>               ]>
>            ,
>           }>
>     = @make_box(captures_stack_23);
>   let captures_47: box<erased> = @ptr_cast(captures_box_23 as box<erased>);
>   let fn_ptr_23: *fn = @make_fn_ptr<lam191>;
>   let unboxed14: { *fn, box<erased> } = @make_struct{ fn_ptr_23, captures_47 };
>   let var48: box<%type_12 = { *fn, box<erased> }> = @make_box(unboxed14);
>   return var48;
> }
> 
> proc lam91(
>   captures_12: box<erased>,
>    toNext2: box<%type_69 = { *fn, box<erased> }>):
>   box<
>     %type_90 =
>     [
>        `0 { [ `0 { box<%type_89 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, box<%type_70 = { *fn, box<erased> }> }
>     ]>
> {
>   let captures_box9: box<{ str }> = @ptr_cast(captures_12 as box<{ str }>);
>   let captures_stack9: { str } = @get_boxed<captures_box9>;
>   let s: str = @get_struct_field<captures_stack9, 0>;
>   let captures_stack_13: { box<%type_69 = { *fn, box<erased> }> }
>     = @make_struct{ toNext2 };
>   let captures_box_13: box<{ box<%type_69 = { *fn, box<erased> }> }>
>     = @make_box(captures_stack_13);
>   let captures_37: box<erased> = @ptr_cast(captures_box_13 as box<erased>);
>   let fn_ptr_13: *fn = @make_fn_ptr<lam81>;
>   let unboxed3: { *fn, box<erased> } = @make_struct{ fn_ptr_13, captures_37 };
>   let var12: box<%type_70 = { *fn, box<erased> }> = @make_box(unboxed3);
>   let struct2: { str, box<%type_70 = { *fn, box<erased> }> }
>     = @make_struct{ s, var12 };
>   let unboxed4:
>         [
>            `0 { [ `0 { box<%type_89 = []> }, `1 { {} } ] },
>            `1 { { *fn, box<erased> } },
>            `2 { str, box<%type_70 = { *fn, box<erased> }> }
>         ]
>     = @make_union<2, struct2>;
>   let var13:
>         box<
>           %type_90 =
>           [
>              `0 { [ `0 { box<%type_89 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, box<%type_70 = { *fn, box<erased> }> }
>           ]>
>     = @make_box(unboxed4);
>   return var13;
> }
> 
> proc clos_fail(captures_3: box<erased>, err: box<%type_58 = []>):
>   box<%type_85 = { *fn, box<erased> }>
> {
>   let captures_box3: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack3: {} = @get_boxed<captures_box3>;
>   let captures_stack_9: { box<%type_58 = []> } = @make_struct{ err };
>   let captures_box_9: box<{ box<%type_58 = []> }>
>     = @make_box(captures_stack_9);
>   let captures_33: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<lam22>;
>   let unboxed: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_33 };
>   let var4: box<%type_85 = { *fn, box<erased> }> = @make_box(unboxed);
>   return var4;
> }
> 
> proc inLine_thunk2(): { *fn, box<erased> }
> {
>   let captures_stack_6: {} = @make_struct{};
>   let captures_box_6: box<{}> = @make_box(captures_stack_6);
>   let captures_23: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos_inLine2>;
>   let inLine_closure2: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_6, captures_23 };
>   return inLine_closure2;
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
>   let captures_9: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_fail1>;
>   let fail_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_9 };
>   return fail_closure1;
> }
> 
> proc clos_outLine1(captures_27: box<erased>, s: str):
>   box<%type_94 = { *fn, box<erased> }>
> {
>   let captures_box20: box<{}> = @ptr_cast(captures_27 as box<{}>);
>   let captures_stack20: {} = @get_boxed<captures_box20>;
>   let captures_stack_21: { str } = @make_struct{ s };
>   let captures_box_21: box<{ str }> = @make_box(captures_stack_21);
>   let captures_45: box<erased> = @ptr_cast(captures_box_21 as box<erased>);
>   let fn_ptr_21: *fn = @make_fn_ptr<lam91>;
>   let unboxed11: { *fn, box<erased> } = @make_struct{ fn_ptr_21, captures_45 };
>   let var41: box<%type_94 = { *fn, box<erased> }> = @make_box(unboxed11);
>   return var41;
> }
> 
> proc clos_outLine(captures_14: box<erased>, s: str): { *fn, box<erased> }
> {
>   let captures_box11: box<{}> = @ptr_cast(captures_14 as box<{}>);
>   let captures_stack11: {} = @get_boxed<captures_box11>;
>   let captures_stack_14: { str } = @make_struct{ s };
>   let captures_box_14: box<{ str }> = @make_box(captures_stack_14);
>   let captures_38: box<erased> = @ptr_cast(captures_box_14 as box<erased>);
>   let fn_ptr_14: *fn = @make_fn_ptr<lam91>;
>   let var16: { *fn, box<erased> } = @make_struct{ fn_ptr_14, captures_38 };
>   return var16;
> }
> 
> proc fail_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_4: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_fail>;
>   let fail_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_4 };
>   return fail_closure;
> }
> 
> global inLine3: { *fn, box<erased> } = @call_direct(inLine_thunk2);
> 
> global inLine1: { *fn, box<erased> } = @call_direct(inLine_thunk1);
> 
> global inLine2: { *fn, box<erased> } = @call_direct(inLine_thunk);
> 
> global fail1: { *fn, box<erased> } = @call_direct(fail_thunk1);
> 
> proc outLine_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_7: {} = @make_struct{};
>   let captures_box_7: box<{}> = @make_box(captures_stack_7);
>   let captures_28: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos_outLine1>;
>   let outLine_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_7, captures_28 };
>   return outLine_closure1;
> }
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
> global fail2: { *fn, box<erased> } = @call_direct(fail_thunk);
> 
> global outLine2: { *fn, box<erased> } = @call_direct(outLine_thunk1);
> 
> global outLine1: { *fn, box<erased> } = @call_direct(outLine_thunk);
> 
> proc lam42(
>   captures_1: box<erased>,
>    result: [ `0 { box<%type_58 = []> }, `1 { {} } ]):
>   box<
>     %type_59 =
>     [
>        `0 { [ `0 { box<%type_58 = []> }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box1:
>         box<{ box<%type_57 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_1 as
>         box<{ box<%type_57 = { *fn, box<erased> }>, { *fn, box<erased> } }>);
>   let captures_stack1:
>         { box<%type_57 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @get_boxed<captures_box1>;
>   let continue: box<%type_57 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack1, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack1, 1>;
>   let discr: int = @get_union_id<result>;
>   switch discr {
>   0 -> {
>     let payload1: { box<%type_58 = []> } = @get_union_struct<result>;
>     let e: box<%type_58 = []> = @get_struct_field<payload1, 0>;
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
>   let inner: box<%type_85 = { *fn, box<erased> }> = join;
>   let inner1: { *fn, box<erased> } = @get_boxed<inner>;
>   let fnptr2: *fn = @get_struct_field<inner1, 0>;
>   let captures2: box<erased> = @get_struct_field<inner1, 1>;
>   let var1:
>         box<
>           %type_59 =
>           [
>              `0 { [ `0 { box<%type_58 = []> }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr2, captures2, continue);
>   return var1;
> }
> 
> proc lam41(
>   captures_6: box<erased>,
>    result: [ `0 { box<%type_65 = []> }, `1 { {} } ]):
>   box<
>     %type_48 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box5:
>         box<{ box<%type_54 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_6 as
>         box<{ box<%type_54 = { *fn, box<erased> }>, { *fn, box<erased> } }>);
>   let captures_stack5:
>         { box<%type_54 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @get_boxed<captures_box5>;
>   let continue: box<%type_54 = { *fn, box<erased> }>
>     = @get_struct_field<captures_stack5, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack5, 1>;
>   let discr1: int = @get_union_id<result>;
>   switch discr1 {
>   0 -> {
>     let payload3: { box<%type_65 = []> } = @get_union_struct<result>;
>     let e: box<%type_65 = []> = @get_struct_field<payload3, 0>;
>     let fnptr6: *fn = @get_struct_field<fail2, 0>;
>     let captures6: box<erased> = @get_struct_field<fail2, 1>;
>     @call_indirect(fnptr6, captures6, e)
>   }
>   1 -> {
>     let payload2: { {} } = @get_union_struct<result>;
>     let v: {} = @get_struct_field<payload2, 0>;
>     let fnptr5: *fn = @get_struct_field<next, 0>;
>     let captures5: box<erased> = @get_struct_field<next, 1>;
>     @call_indirect(fnptr5, captures5, v)
>   }
>   } in join join1;
>   let inner: box<%type_87 = { *fn, box<erased> }> = join1;
>   let inner3: { *fn, box<erased> } = @get_boxed<inner>;
>   let fnptr7: *fn = @get_struct_field<inner3, 0>;
>   let captures7: box<erased> = @get_struct_field<inner3, 1>;
>   let var7:
>         box<
>           %type_48 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr7, captures7, continue);
>   return var7;
> }
> 
> proc lam131(captures_26: box<erased>, lastName: str):
>   box<%type_94 = { *fn, box<erased> }>
> {
>   let captures_box19: box<{ str }> = @ptr_cast(captures_26 as box<{ str }>);
>   let captures_stack19: { str } = @get_boxed<captures_box19>;
>   let firstName: str = @get_struct_field<captures_stack19, 0>;
>   let fnptr18: *fn = @get_struct_field<outLine2, 0>;
>   let captures18: box<erased> = @get_struct_field<outLine2, 1>;
>   let var36: str = "Hello ";
>   let var37: str = " ";
>   let var38: str = "!";
>   let var39: str
>     = @call_kfn(str_concat, var36, firstName, var37, lastName, var38);
>   let var40: box<%type_94 = { *fn, box<erased> }>
>     = @call_indirect(fnptr18, captures18, var39);
>   return var40;
> }
> 
> proc lam51(
>   captures_5: box<erased>,
>    continue: box<%type_54 = { *fn, box<erased> }>):
>   box<
>     %type_48 =
>     [
>        `0 { [ `0 { [] }, `1 { {} } ] },
>        `1 { { *fn, box<erased> } },
>        `2 { str, { *fn, box<erased> } }
>     ]>
> {
>   let captures_box4: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @ptr_cast(
>         captures_5 as
>         box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack4: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @get_boxed<captures_box4>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack4, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack4, 1>;
>   let fnptr4: *fn = @get_struct_field<fromResult, 0>;
>   let captures4: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_10:
>         { box<%type_54 = { *fn, box<erased> }>, { *fn, box<erased> } }
>     = @make_struct{ continue, next };
>   let captures_box_10:
>         box<{ box<%type_54 = { *fn, box<erased> }>, { *fn, box<erased> } }>
>     = @make_box(captures_stack_10);
>   let captures_34: box<erased> = @ptr_cast(captures_box_10 as box<erased>);
>   let fn_ptr_10: *fn = @make_fn_ptr<lam42>;
>   let unboxed1: { *fn, box<erased> } = @make_struct{ fn_ptr_10, captures_34 };
>   let var5: box<%type_64 = { *fn, box<erased> }> = @make_box(unboxed1);
>   let var6:
>         box<
>           %type_48 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr4, captures4, var5);
>   return var6;
> }
> 
> proc lam61(captures_: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_ as box<{ { *fn, box<erased> } }>);
>   let captures_stack: { { *fn, box<erased> } } = @get_boxed<captures_box>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack, 0>;
>   let captures_stack_8: { { *fn, box<erased> }, { *fn, box<erased> } }
>     = @make_struct{ fromResult, next };
>   let captures_box_8: box<{ { *fn, box<erased> }, { *fn, box<erased> } }>
>     = @make_box(captures_stack_8);
>   let captures_32: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<lam51>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_32 };
>   return var;
> }
> 
> proc clos_await(captures_10: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box8: box<{}> = @ptr_cast(captures_10 as box<{}>);
>   let captures_stack8: {} = @get_boxed<captures_box8>;
>   let captures_stack_12: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_12: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_12);
>   let captures_36: box<erased> = @ptr_cast(captures_box_12 as box<erased>);
>   let fn_ptr_12: *fn = @make_fn_ptr<lam61>;
>   let var11: { *fn, box<erased> } = @make_struct{ fn_ptr_12, captures_36 };
>   return var11;
> }
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_11: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos_await>;
>   let await_closure: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_2, captures_11 };
>   return await_closure;
> }
> 
> global await1: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc lam141(captures_25: box<erased>, y: {}):
>   box<%type_91 = { *fn, box<erased> }>
> {
>   let captures_box18: box<{ str }> = @ptr_cast(captures_25 as box<{ str }>);
>   let captures_stack18: { str } = @get_boxed<captures_box18>;
>   let firstName: str = @get_struct_field<captures_stack18, 0>;
>   let fnptr16: *fn = @get_struct_field<await1, 0>;
>   let captures16: box<erased> = @get_struct_field<await1, 1>;
>   let var33: { *fn, box<erased> }
>     = @call_indirect(fnptr16, captures16, inLine3);
>   let fnptr17: *fn = @get_struct_field<var33, 0>;
>   let captures17: box<erased> = @get_struct_field<var33, 1>;
>   let captures_stack_20: { str } = @make_struct{ firstName };
>   let captures_box_20: box<{ str }> = @make_box(captures_stack_20);
>   let captures_44: box<erased> = @ptr_cast(captures_box_20 as box<erased>);
>   let fn_ptr_20: *fn = @make_fn_ptr<lam131>;
>   let unboxed10: { *fn, box<erased> } = @make_struct{ fn_ptr_20, captures_44 };
>   let var34: box<%type_77 = { *fn, box<erased> }> = @make_box(unboxed10);
>   let var35: box<%type_91 = { *fn, box<erased> }>
>     = @call_indirect(fnptr17, captures17, var34);
>   return var35;
> }
> 
> proc lam151(captures_24: box<erased>, firstName: str):
>   box<%type_94 = { *fn, box<erased> }>
> {
>   let captures_box17: box<{}> = @ptr_cast(captures_24 as box<{}>);
>   let captures_stack17: {} = @get_boxed<captures_box17>;
>   let fnptr13: *fn = @get_struct_field<await1, 0>;
>   let captures13: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr14: *fn = @get_struct_field<outLine1, 0>;
>   let captures14: box<erased> = @get_struct_field<outLine1, 1>;
>   let var28: str = "What's your last name?";
>   let var29: { *fn, box<erased> } = @call_indirect(fnptr14, captures14, var28);
>   let var30: { *fn, box<erased> } = @call_indirect(fnptr13, captures13, var29);
>   let fnptr15: *fn = @get_struct_field<var30, 0>;
>   let captures15: box<erased> = @get_struct_field<var30, 1>;
>   let captures_stack_19: { str } = @make_struct{ firstName };
>   let captures_box_19: box<{ str }> = @make_box(captures_stack_19);
>   let captures_43: box<erased> = @ptr_cast(captures_box_19 as box<erased>);
>   let fn_ptr_19: *fn = @make_fn_ptr<lam141>;
>   let unboxed9: { *fn, box<erased> } = @make_struct{ fn_ptr_19, captures_43 };
>   let var31: box<%type_78 = { *fn, box<erased> }> = @make_box(unboxed9);
>   let var32: box<%type_94 = { *fn, box<erased> }>
>     = @call_indirect(fnptr15, captures15, var31);
>   return var32;
> }
> 
> proc lam161(captures_18: box<erased>, x1: {}):
>   box<%type_91 = { *fn, box<erased> }>
> {
>   let captures_box13: box<{}> = @ptr_cast(captures_18 as box<{}>);
>   let captures_stack13: {} = @get_boxed<captures_box13>;
>   let fnptr10: *fn = @get_struct_field<await1, 0>;
>   let captures10: box<erased> = @get_struct_field<await1, 1>;
>   let var19: { *fn, box<erased> }
>     = @call_indirect(fnptr10, captures10, inLine2);
>   let fnptr11: *fn = @get_struct_field<var19, 0>;
>   let captures11: box<erased> = @get_struct_field<var19, 1>;
>   let captures_stack_16: {} = @make_struct{};
>   let captures_box_16: box<{}> = @make_box(captures_stack_16);
>   let captures_40: box<erased> = @ptr_cast(captures_box_16 as box<erased>);
>   let fn_ptr_16: *fn = @make_fn_ptr<lam151>;
>   let unboxed6: { *fn, box<erased> } = @make_struct{ fn_ptr_16, captures_40 };
>   let var20: box<%type_77 = { *fn, box<erased> }> = @make_box(unboxed6);
>   let var21: box<%type_91 = { *fn, box<erased> }>
>     = @call_indirect(fnptr11, captures11, var20);
>   return var21;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr19: *fn = @get_struct_field<await1, 0>;
>   let captures19: box<erased> = @get_struct_field<await1, 1>;
>   let fnptr20: *fn = @get_struct_field<outLine1, 0>;
>   let captures20: box<erased> = @get_struct_field<outLine1, 1>;
>   let var42: str = "What's your first name?";
>   let var43: { *fn, box<erased> } = @call_indirect(fnptr20, captures20, var42);
>   let var44: { *fn, box<erased> } = @call_indirect(fnptr19, captures19, var43);
>   let fnptr21: *fn = @get_struct_field<var44, 0>;
>   let captures21: box<erased> = @get_struct_field<var44, 1>;
>   let captures_stack_22: {} = @make_struct{};
>   let captures_box_22: box<{}> = @make_box(captures_stack_22);
>   let captures_46: box<erased> = @ptr_cast(captures_box_22 as box<erased>);
>   let fn_ptr_22: *fn = @make_fn_ptr<lam161>;
>   let var45: { *fn, box<erased> } = @make_struct{ fn_ptr_22, captures_46 };
>   let var46: { *fn, box<erased> } = @call_indirect(fnptr21, captures21, var45);
>   return var46;
> }
> 
> global main1: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> proc main_handler_thunk():
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_2 =
>             [ `0 {}, `1 { box<%type_2> }, `2 { str, box<%type_2> } ]>
>          ,
>         }
>   ]
> {
>   let fnptr30: *fn = @get_struct_field<main1, 0>;
>   let captures30: box<erased> = @get_struct_field<main1, 1>;
>   let captures_stack_26: {} = @make_struct{};
>   let captures_box_26: box<{}> = @make_box(captures_stack_26);
>   let captures_50: box<erased> = @ptr_cast(captures_box_26 as box<erased>);
>   let fn_ptr_26: *fn = @make_fn_ptr<lam171>;
>   let unboxed20: { *fn, box<erased> } = @make_struct{ fn_ptr_26, captures_50 };
>   let var67: box<%type_0 = { *fn, box<erased> }> = @make_box(unboxed20);
>   let op:
>         box<
>           %type_1 =
>           [
>              `0 { [ `0 { [] }, `1 { {} } ] },
>              `1 { { *fn, box<erased> } },
>              `2 { str, { *fn, box<erased> } }
>           ]>
>     = @call_indirect(fnptr30, captures30, var67);
>   let captures_stack_27: {} = @make_struct{};
>   let captures_box_27: box<{}> = @make_box(captures_stack_27);
>   let captures_51: box<erased> = @ptr_cast(captures_box_27 as box<erased>);
>   let fn_ptr_27: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> } = @make_struct{ fn_ptr_27, captures_51 };
>   let fnptr31: *fn = @get_struct_field<handle1, 0>;
>   let captures31: box<erased> = @get_struct_field<handle1, 1>;
>   let var68: { *fn, box<erased> } = @call_indirect(fnptr31, captures31, op);
>   let fnptr32: *fn = @get_struct_field<var68, 0>;
>   let captures32: box<erased> = @get_struct_field<var68, 1>;
>   let var69: int = 0;
>   let var70: { *fn, box<erased> } = @call_indirect(fnptr32, captures32, var69);
>   let fnptr33: *fn = @get_struct_field<var70, 0>;
>   let captures33: box<erased> = @get_struct_field<var70, 1>;
>   let struct12: {} = @make_struct{};
>   let unboxed21:
>         [
>            `0 {},
>            `1 {
>                box<
>                  %type_2 =
>                  [ `0 {}, `1 { box<%type_2> }, `2 { str, box<%type_2> } ]>
>                ,
>               },
>            `2 { str, box<%type_2> }
>         ]
>     = @make_union<0, struct12>;
>   let var71:
>         box<%type_2 = [ `0 {}, `1 { box<%type_2> }, `2 { str, box<%type_2> } ]>
>     = @make_box(unboxed21);
>   let var72:
>         [
>            `0 {
>                [ `0 { [] }, `1 { {} } ],
>                 box<
>                   %type_2 =
>                   [ `0 {}, `1 { box<%type_2> }, `2 { str, box<%type_2> } ]>
>                ,
>               }
>         ]
>     = @call_indirect(fnptr33, captures33, var71);
>   return var72;
> }
> 
> global main_handler:
>   [
>      `0 {
>          [ `0 { [] }, `1 { {} } ],
>           box<
>             %type_84 =
>             [ `0 {}, `1 { box<%type_84> }, `2 { str, box<%type_84> } ]>
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