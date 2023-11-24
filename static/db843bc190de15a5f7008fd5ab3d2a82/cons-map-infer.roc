# cor +solve -elab
# cor +ir -print
# cor +eval -print
let map = \f -> \xs ->
  let rec go = \xs ->
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

run main = map mapper l;;
#   ^^^^   ^^^

> cor-out +solve -elab
> let map = \f -> \xs ->
>   let rec go = \xs ->
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
> #   ^^^^^^ 'a -> [A 'a]'*
> 
> let l = Cons 1 (Cons 2 Nil);;
> 
> run main = map mapper l;;
> #          ^^^ (Int -> [A Int]'a)
> #          ^^^   -> [Cons Int <..[Cons .. .., Nil]?*>, Nil]?*
> #          ^^^        -> [
> #          ^^^             Cons [A Int]'a <..[Cons .. .., Nil]'*>,
> #          ^^^             Nil
> #          ^^^             ]'*
> #   ^^^^ [Cons [A Int]'* <..[Cons .. .., Nil]'*>, Nil]'*
> 

> cor-out +ir -print
> proc go1(
>   captures_go: box<erased>,
>    xs1: box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>):
>   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
> {
>   let captures_box2: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_go as box<{ { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> } } = @get_boxed<captures_box2>;
>   let f: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let rec_fn_ptr_go: *fn = @make_fn_ptr<go1>;
>   let go: { *fn, box<erased> } = @make_struct{ rec_fn_ptr_go, captures_go };
>   let inner:
>         [ `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }, `1 {}
>         ]
>     = @get_boxed<xs1>;
>   let discr: int = @get_union_id<inner>;
>   switch discr {
>   0 -> {
>     let payload1: { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }
>       = @get_union_struct<inner>;
>     let x: int = @get_struct_field<payload1, 0>;
>     let xs2: box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>
>       = @get_struct_field<payload1, 1>;
>     let fnptr1: *fn = @get_struct_field<f, 0>;
>     let captures1: box<erased> = @get_struct_field<f, 1>;
>     let var3: [ `0 { int } ] = @call_indirect(fnptr1, captures1, x);
>     let fnptr2: *fn = @get_struct_field<go, 0>;
>     let captures2: box<erased> = @get_struct_field<go, 1>;
>     let var4: box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>       = @call_indirect(fnptr2, captures2, xs2);
>     let struct1:
>           {
>            [ `0 { int } ],
>             box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>            ,
>           }
>       = @make_struct{ var3, var4 };
>     let union1:
>           [
>              `0 {
>                  [ `0 { int } ],
>                   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>                  ,
>                 },
>              `1 {}
>           ]
>       = @make_union<0, struct1>;
>     @make_box(union1)
>   }
>   1 -> {
>     let payload: {} = @get_union_struct<inner>;
>     let struct: {} = @make_struct{};
>     let union:
>           [
>              `0 {
>                  [ `0 { int } ],
>                   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>                  ,
>                 },
>              `1 {}
>           ]
>       = @make_union<1, struct>;
>     @make_box(union)
>   }
>   } in join join;
>   return join;
> }
> 
> proc clos1(
>   captures_1: box<erased>,
>    xs: box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>):
>   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
> {
>   let captures_box1: box<{ { *fn, box<erased> } }>
>     = @ptr_cast(captures_1 as box<{ { *fn, box<erased> } }>);
>   let captures_stack1: { { *fn, box<erased> } } = @get_boxed<captures_box1>;
>   let f: { *fn, box<erased> } = @get_struct_field<captures_stack1, 0>;
>   let captures_stack_go: { { *fn, box<erased> } } = @make_struct{ f };
>   let captures_box_go: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_go);
>   let captures_go: box<erased> = @ptr_cast(captures_box_go as box<erased>);
>   let fn_ptr_go: *fn = @make_fn_ptr<go1>;
>   let go: { *fn, box<erased> } = @make_struct{ fn_ptr_go, captures_go };
>   let fnptr: *fn = @get_struct_field<go, 0>;
>   let captures: box<erased> = @get_struct_field<go, 1>;
>   let var2: box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>     = @call_indirect(fnptr, captures, xs);
>   return var2;
> }
> 
> proc clos(captures_: box<erased>, f: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{}> = @ptr_cast(captures_ as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_1: { { *fn, box<erased> } } = @make_struct{ f };
>   let captures_box_1: box<{ { *fn, box<erased> } }>
>     = @make_box(captures_stack_1);
>   let captures_1: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos1>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_1, captures_1 };
>   return var1;
> }
> 
> proc map_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   return var;
> }
> 
> global map1: { *fn, box<erased> } = @call_direct(map_thunk);
> 
> proc clos2(captures_2: box<erased>, x1: int): [ `0 { int } ]
> {
>   let captures_box3: box<{}> = @ptr_cast(captures_2 as box<{}>);
>   let captures_stack3: {} = @get_boxed<captures_box3>;
>   let struct2: { int } = @make_struct{ x1 };
>   let var6: [ `0 { int } ] = @make_union<0, struct2>;
>   return var6;
> }
> 
> proc mapper_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_2: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos2>;
>   let var5: { *fn, box<erased> } = @make_struct{ fn_ptr_2, captures_2 };
>   return var5;
> }
> 
> global mapper1: { *fn, box<erased> } = @call_direct(mapper_thunk);
> 
> proc l_thunk(): box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>
> {
>   let var7: int = 1;
>   let var8: int = 2;
>   let struct3: {} = @make_struct{};
>   let union2:
>         [ `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }, `1 {}
>         ]
>     = @make_union<1, struct3>;
>   let var9:
>         box<
>           %type_3 =
>           [
>              `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> },
>              `1 {}
>           ]>
>     = @make_box(union2);
>   let struct4: { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }
>     = @make_struct{ var8, var9 };
>   let union3:
>         [ `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }, `1 {}
>         ]
>     = @make_union<0, struct4>;
>   let var10:
>         box<
>           %type_2 =
>           [
>              `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> },
>              `1 {}
>           ]>
>     = @make_box(union3);
>   let struct5: { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }
>     = @make_struct{ var7, var10 };
>   let union4:
>         [ `0 { int, box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]> }, `1 {}
>         ]
>     = @make_union<0, struct5>;
>   let var11: box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>
>     = @make_box(union4);
>   return var11;
> }
> 
> global l1:
>   box<%type_0 = [ `0 { int, box<%type_0> }, `1 {} ]>
>   = @call_direct(l_thunk);
> 
> proc main_thunk():
>   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
> {
>   let fnptr3: *fn = @get_struct_field<map1, 0>;
>   let captures3: box<erased> = @get_struct_field<map1, 1>;
>   let var12: { *fn, box<erased> } = @call_indirect(fnptr3, captures3, mapper1);
>   let fnptr4: *fn = @get_struct_field<var12, 0>;
>   let captures4: box<erased> = @get_struct_field<var12, 1>;
>   let var13: box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>     = @call_indirect(fnptr4, captures4, l1);
>   return var13;
> }
> 
> global main:
>   box<%type_1 = [ `0 { [ `0 { int } ], box<%type_1> }, `1 {} ]>
>   = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = [0 [0 1] [0 [0 2] [1]]]
>      > Cons (A 1) (Cons (A 2) (Nil ))