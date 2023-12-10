# cor +solve -elab
# cor +mono -print
# cor +ir -print
# cor +eval -print

let f = \x -> \t ->
  let g = when t is
  #   ^
    | T1 -> \y -> ~add x y 1
    | T2 -> \y -> ~add x y 2
    | T3 -> \y -> ~add x y 3
    end
  in g
;;

run x = f 1 T2 0
;;

> cor-out +solve -elab
> 
> let f = \x -> \t ->
>   let g = when t is
> #     ^ Int -[lam Int, lam1 Int, lam2 Int]-> Int
>     | T1 -> \y -> ~add x y 1
>     | T2 -> \y -> ~add x y 2
>     | T3 -> \y -> ~add x y 3
>     end
>   in g
> ;;
> 
> run x = f 1 T2 0
> ;;
> 

> cor-out +mono -print
> specializations:
>   let lam31 = \t -[lam31 x]->
>     let g =
>       when t is
>         | T1 -> \y -[lam4 x]-> add x y 1
>         | T2 -> \y1 -[lam11 x]-> add x y1 2
>         | T3 -> \y2 -[lam21 x]-> add x y2 3
>       end
>     in
>     g
>   
>   let lam4 = \y -[lam4 x]-> add x y 1
>   
>   let lam11 = \y1 -[lam11 x]-> add x y1 2
>   
>   let lam21 = \y2 -[lam21 x]-> add x y2 3
>   
>   let f2 = \x -[f2]->
>     \t -[lam31 x]->
>       (let g =
>          when t is
>            | T1 -> \y -[lam4 x]-> add x y 1
>            | T2 -> \y1 -[lam11 x]-> add x y1 2
>            | T3 -> \y2 -[lam21 x]-> add x y2 3
>          end
>       in
>       g)
>   
>   let x1 = ((f2 1) (T2 )) 0
>   
>   
> entry_points:
>   x1

> cor-out +ir -print
> proc lam21(captures_3: [ `0 { int }, `1 { int }, `2 { int } ], y2: int): int
> {
>   let captures_stack3: { int } = @get_union_struct<captures_3>;
>   let x: int = @get_struct_field<captures_stack3, 0>;
>   let var4: int = 3;
>   let var5: int = @call_kfn(add, x, y2, var4);
>   return var5;
> }
> 
> proc f2(captures_4: [ `0 {} ], x: int): [ `0 { int } ]
> {
>   let captures_stack4: {} = @get_union_struct<captures_4>;
>   let struct3: { int } = @make_struct{ x };
>   let var6: [ `0 { int } ] = @make_union<0, struct3>;
>   return var6;
> }
> 
> proc lam11(captures_2: [ `0 { int }, `1 { int }, `2 { int } ], y1: int): int
> {
>   let captures_stack2: { int } = @get_union_struct<captures_2>;
>   let x: int = @get_struct_field<captures_stack2, 0>;
>   let var2: int = 2;
>   let var3: int = @call_kfn(add, x, y1, var2);
>   return var3;
> }
> 
> proc lam31(captures_: [ `0 { int } ], t: [ `0 {}, `1 {}, `2 {} ]):
>   [ `0 { int }, `1 { int }, `2 { int } ]
> {
>   let captures_stack: { int } = @get_union_struct<captures_>;
>   let x: int = @get_struct_field<captures_stack, 0>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let payload: {} = @get_union_struct<t>;
>     let struct: { int } = @make_struct{ x };
>     @make_union<0, struct>
>   }
>   1 -> {
>     let payload1: {} = @get_union_struct<t>;
>     let struct1: { int } = @make_struct{ x };
>     @make_union<1, struct1>
>   }
>   2 -> {
>     let payload2: {} = @get_union_struct<t>;
>     let struct2: { int } = @make_struct{ x };
>     @make_union<2, struct2>
>   }
>   } in join join;
>   let g: [ `0 { int }, `1 { int }, `2 { int } ] = join;
>   return g;
> }
> 
> proc lam4(captures_1: [ `0 { int }, `1 { int }, `2 { int } ], y: int): int
> {
>   let captures_stack1: { int } = @get_union_struct<captures_1>;
>   let x: int = @get_struct_field<captures_stack1, 0>;
>   let var: int = 1;
>   let var1: int = @call_kfn(add, x, y, var);
>   return var1;
> }
> 
> proc x_thunk(): int
> {
>   let struct4: {} = @make_struct{};
>   let var7: [ `0 {} ] = @make_union<0, struct4>;
>   let var8: int = 1;
>   let cond: int = @get_union_id<var7>;
>   switch cond {
>   0 -> { @call_direct(f2, var7, var8) }
>   } in join join1;
>   let struct5: {} = @make_struct{};
>   let var9: [ `0 {}, `1 {}, `2 {} ] = @make_union<1, struct5>;
>   let cond1: int = @get_union_id<join1>;
>   switch cond1 {
>   0 -> { @call_direct(lam31, join1, var9) }
>   } in join join2;
>   let var10: int = 0;
>   let cond2: int = @get_union_id<join2>;
>   switch cond2 {
>   0 -> { @call_direct(lam4, join2, var10) }
>   1 -> { @call_direct(lam11, join2, var10) }
>   2 -> { @call_direct(lam21, join2, var10) }
>   } in join join3;
>   return join3;
> }
> 
> global x1: int = @call_direct(x_thunk);
> 
> entry x1;

> cor-out +eval -print
> x1 = 3
>    > 3