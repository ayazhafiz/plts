# cor +solve -elab
# cor +ir -print
# cor +eval -print
let map = \f -> \xs ->
  let go = \xs ->
    when xs is
#        ^^
      | Nil -> Nil
      | Cons x xs -> Cons (f x) (go xs)
      #        ^^                   ^^
    end
  in go xs
#       ^^
;;

let mapper = \x -> A x;;
#   ^^^^^^

let l = Cons 1 (Cons 2 Nil);;
#   ^

run main = map mapper l;;
#   ^^^^   ^^^        ^

> cor-out +solve -elab
> let map = \f -> \xs ->
>   let go = \xs ->
>     when xs is
> #        ^^ [Cons '* <..[Cons .. .., Nil]'*>, Nil]'*
>       | Nil -> Nil
>       | Cons x xs -> Cons (f x) (go xs)
> #                                   ^^ [Cons '* <..[Cons .. .., Nil]'*>, Nil]'*
> #              ^^ [Cons '* <..[Cons .. .., Nil]'*>, Nil]'*
>     end
>   in go xs
> #       ^^ [Cons '* <..[Cons .. .., Nil]'*>, Nil]'*
> ;;
> 
> let mapper = \x -> A x;;
> #   ^^^^^^ 'a -[mapper]-> [A 'a]'*
> 
> let l = Cons 1 (Cons 2 Nil);;
> #   ^ [Cons Int [Cons Int [Nil]'*]'*]'*
> 
> run main = map mapper l;;
> #                     ^ [Cons Int <..[Cons .. .., Nil]?*>, Nil]?*
> #          ^^^ (Int -[mapper]-> [A Int]?a)
> #          ^^^   -[map]-> [
> #          ^^^              Cons Int <..[Cons .. .., Nil]?*>,
> #          ^^^              Nil
> #          ^^^              ]?*
> #          ^^^              -[lam (Int -[mapper]-> [A Int]?a)]-> 
> #          ^^^              [
> #          ^^^                Cons [A Int]?a
> #          ^^^                  <..[Cons .. .., Nil]?*>,
> #          ^^^                Nil
> #          ^^^                ]?*
> #   ^^^^ [Cons [A Int]?* <..[Cons .. .., Nil]?*>, Nil]?*
> 

> cor-out +ir -print
> proc mapper2(captures_2: [ `0 {} ], x1: int): [ `0 { int } ]
> {
>   let captures_stack3: {} = @get_union_struct<captures_2>;
>   let struct5: { int } = @make_struct{ x1 };
>   let var1: [ `0 { int } ] = @make_union<0, struct5>;
>   return var1;
> }
> 
> proc map2(captures_1: [ `0 {} ], f: [ `0 {} ]): [ `0 { [ `0 {} ] } ]
> {
>   let captures_stack2: {} = @get_union_struct<captures_1>;
>   let struct4: { [ `0 {} ] } = @make_struct{ f };
>   let var: [ `0 { [ `0 {} ] } ] = @make_union<0, struct4>;
>   return var;
> }
> 
> proc l_thunk():
>   box<
>     %type_0 =
>     [
>        `0 { int, [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ] },
>        `1 {}
>     ]>
> {
>   let var2: int = 1;
>   let var3: int = 2;
>   let struct6: {} = @make_struct{};
>   let var4:
>         [
>            `0 {
>                int,
>                 box<
>                   %type_0 =
>                   [
>                      `0 {
>                          int,
>                           [
>                              `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] },
>                              `1 {}
>                           ]
>                          ,
>                         },
>                      `1 {}
>                   ]>
>                ,
>               },
>            `1 {}
>         ]
>     = @make_union<1, struct6>;
>   let struct7:
>         {
>          int,
>           [
>              `0 {
>                  int,
>                   box<
>                     %type_0 =
>                     [
>                        `0 {
>                            int,
>                             [
>                                `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] },
>                                `1 {}
>                             ]
>                            ,
>                           },
>                        `1 {}
>                     ]>
>                  ,
>                 },
>              `1 {}
>           ]
>          ,
>         }
>     = @make_struct{ var3, var4 };
>   let var5:
>         [
>            `0 {
>                int,
>                 [
>                    `0 {
>                        int,
>                         box<
>                           %type_0 =
>                           [
>                              `0 {
>                                  int,
>                                   [
>                                      `0 {
>                                          int,
>                                           [ `0 { int, box<%type_0> }, `1 {} ]
>                                          ,
>                                         },
>                                      `1 {}
>                                   ]
>                                  ,
>                                 },
>                              `1 {}
>                           ]>
>                        ,
>                       },
>                    `1 {}
>                 ]
>                ,
>               },
>            `1 {}
>         ]
>     = @make_union<0, struct7>;
>   let struct8:
>         {
>          int,
>           [
>              `0 {
>                  int,
>                   [
>                      `0 {
>                          int,
>                           box<
>                             %type_0 =
>                             [
>                                `0 {
>                                    int,
>                                     [
>                                        `0 {
>                                            int,
>                                             [ `0 { int, box<%type_0> }, `1 {} ]
>                                            ,
>                                           },
>                                        `1 {}
>                                     ]
>                                    ,
>                                   },
>                                `1 {}
>                             ]>
>                          ,
>                         },
>                      `1 {}
>                   ]
>                  ,
>                 },
>              `1 {}
>           ]
>          ,
>         }
>     = @make_struct{ var2, var5 };
>   let unboxed2:
>         [
>            `0 {
>                int,
>                 [
>                    `0 {
>                        int,
>                         [
>                            `0 {
>                                int,
>                                 box<
>                                   %type_0 =
>                                   [
>                                      `0 {
>                                          int,
>                                           [
>                                              `0 {
>                                                  int,
>                                                   [
>                                                      `0 { int, box<%type_0> },
>                                                      `1 {}
>                                                   ]
>                                                  ,
>                                                 },
>                                              `1 {}
>                                           ]
>                                          ,
>                                         },
>                                      `1 {}
>                                   ]>
>                                ,
>                               },
>                            `1 {}
>                         ]
>                        ,
>                       },
>                    `1 {}
>                 ]
>                ,
>               },
>            `1 {}
>         ]
>     = @make_union<0, struct8>;
>   let var6:
>         box<
>           %type_0 =
>           [
>              `0 {
>                  int,
>                   [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ]
>                  ,
>                 },
>              `1 {}
>           ]>
>     = @make_box(unboxed2);
>   return var6;
> }
> 
> proc go11(
>   captures_go: [ `0 { [ `0 {} ] } ],
>    xs1:
>      box<
>        %type_0 =
>        [
>           `0 { int, [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ] },
>           `1 {}
>        ]>):
>   box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
> {
>   let captures_stack1: { [ `0 {} ] } = @get_union_struct<captures_go>;
>   let f: [ `0 {} ] = @get_struct_field<captures_stack1, 0>;
>   let struct1: { [ `0 {} ] } = @make_struct{ f };
>   let go: [ `0 { [ `0 {} ] } ] = @make_union<0, struct1>;
>   let inner:
>         [
>            `0 {
>                int,
>                 [
>                    `0 {
>                        int,
>                         [
>                            `0 {
>                                int,
>                                 box<
>                                   %type_0 =
>                                   [
>                                      `0 {
>                                          int,
>                                           [
>                                              `0 {
>                                                  int,
>                                                   [
>                                                      `0 { int, box<%type_0> },
>                                                      `1 {}
>                                                   ]
>                                                  ,
>                                                 },
>                                              `1 {}
>                                           ]
>                                          ,
>                                         },
>                                      `1 {}
>                                   ]>
>                                ,
>                               },
>                            `1 {}
>                         ]
>                        ,
>                       },
>                    `1 {}
>                 ]
>                ,
>               },
>            `1 {}
>         ]
>     = @get_boxed<xs1>;
>   let discr: int = @get_union_id<inner>;
>   switch discr {
>   0 -> {
>     let payload1:
>           {
>            int,
>             box<
>               %type_0 =
>               [
>                  `0 {
>                      int,
>                       [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {}
>                       ]
>                      ,
>                     },
>                  `1 {}
>               ]>
>            ,
>           }
>       = @get_union_struct<inner>;
>     let x: int = @get_struct_field<payload1, 0>;
>     let xs2:
>           box<
>             %type_0 =
>             [
>                `0 {
>                    int,
>                     [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ]
>                    ,
>                   },
>                `1 {}
>             ]>
>       = @get_struct_field<payload1, 1>;
>     let cond1: int = @get_union_id<f>;
>     switch cond1 {
>     0 -> { @call_direct(mapper2, f, x) }
>     } in join join1;
>     let cond2: int = @get_union_id<go>;
>     switch cond2 {
>     0 -> { @call_direct(go11, go, xs2) }
>     } in join join2;
>     let struct3:
>           {
>            [ `0 { int } ],
>             box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
>            ,
>           }
>       = @make_struct{ join1, join2 };
>     let unboxed1:
>           [
>              `0 {
>                  [ `0 { int } ],
>                   box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
>                  ,
>                 },
>              `1 {}
>           ]
>       = @make_union<0, struct3>;
>     @make_box(unboxed1)
>   }
>   1 -> {
>     let payload: {} = @get_union_struct<inner>;
>     let struct2: {} = @make_struct{};
>     let unboxed:
>           [
>              `0 {
>                  [ `0 { int } ],
>                   box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
>                  ,
>                 },
>              `1 {}
>           ]
>       = @make_union<1, struct2>;
>     @make_box(unboxed)
>   }
>   } in join join3;
>   return join3;
> }
> 
> global l1:
>   box<
>     %type_0 =
>     [
>        `0 { int, [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ] },
>        `1 {}
>     ]>
>   = @call_direct(l_thunk);
> 
> proc lam1(
>   captures_: [ `0 { [ `0 {} ] } ],
>    xs:
>      box<
>        %type_0 =
>        [
>           `0 { int, [ `0 { int, [ `0 { int, box<%type_0> }, `1 {} ] }, `1 {} ] },
>           `1 {}
>        ]>):
>   box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
> {
>   let captures_stack: { [ `0 {} ] } = @get_union_struct<captures_>;
>   let f: [ `0 {} ] = @get_struct_field<captures_stack, 0>;
>   let struct: { [ `0 {} ] } = @make_struct{ f };
>   let go: [ `0 { [ `0 {} ] } ] = @make_union<0, struct>;
>   let cond: int = @get_union_id<go>;
>   switch cond {
>   0 -> { @call_direct(go11, go, xs) }
>   } in join join;
>   return join;
> }
> 
> proc main_thunk():
>   box<%type_3 = [ `0 { [ `0 { int } ], box<%type_3> }, `1 {} ]>
> {
>   let struct9: {} = @make_struct{};
>   let var7: [ `0 {} ] = @make_union<0, struct9>;
>   let struct10: {} = @make_struct{};
>   let var8: [ `0 {} ] = @make_union<0, struct10>;
>   let cond3: int = @get_union_id<var7>;
>   switch cond3 {
>   0 -> { @call_direct(map2, var7, var8) }
>   } in join join4;
>   let cond4: int = @get_union_id<join4>;
>   switch cond4 {
>   0 -> { @call_direct(lam1, join4, l1) }
>   } in join join5;
>   return join5;
> }
> 
> global main:
>   box<%type_4 = [ `0 { [ `0 { int } ], box<%type_4> }, `1 {} ]>
>   = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = [0 [0 1] [0 [0 2] [1]]]
>      > Cons (A 1) (Cons (A 2) (Nil ))