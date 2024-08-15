# cor +ir -print
# cor +eval -print

let f = \x ->
  let g = \t -> when t is
    | T -> ~add x 1
    | F -> g T
  end
  in g
;;

run main = f 2 F
;;

> cor-out +ir -print
> proc g11(captures_g: [ `0 { int } ], t: [ `0 {}, `1 {} ]): int
> {
>   let captures_stack: { int } = @get_union_struct<captures_g>;
>   let x: int = @get_struct_field<captures_stack, 0>;
>   let struct: { int } = @make_struct{ x };
>   let g: [ `0 { int } ] = @make_union<0, struct>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let payload1: {} = @get_union_struct<t>;
>     let struct1: {} = @make_struct{};
>     let var1: [ `0 {}, `1 {} ] = @make_union<1, struct1>;
>     let cond: int = @get_union_id<g>;
>     switch cond {
>     0 -> { @call_direct(g11, g, var1) }
>     } in join join;
>     join
>   }
>   1 -> {
>     let payload: {} = @get_union_struct<t>;
>     let var: int = 1;
>     @call_kfn(add, x, var)
>   }
>   } in join join1;
>   return join1;
> }
> 
> proc f2(captures_: [ `0 {} ], x: int): [ `0 { int } ]
> {
>   let captures_stack1: {} = @get_union_struct<captures_>;
>   let struct2: { int } = @make_struct{ x };
>   let g: [ `0 { int } ] = @make_union<0, struct2>;
>   return g;
> }
> 
> proc main_thunk(): int
> {
>   let struct3: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct3>;
>   let var3: int = 2;
>   let cond1: int = @get_union_id<var2>;
>   switch cond1 {
>   0 -> { @call_direct(f2, var2, var3) }
>   } in join join2;
>   let struct4: {} = @make_struct{};
>   let var4: [ `0 {}, `1 {} ] = @make_union<0, struct4>;
>   let cond2: int = @get_union_id<join2>;
>   switch cond2 {
>   0 -> { @call_direct(g11, join2, var4) }
>   } in join join3;
>   return join3;
> }
> 
> global main: int = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = 3
>      > 3