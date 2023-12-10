# cor +ir -print
# cor +eval -print

let map = \x ->
  let f = \y -> ~add y 1 in
  f x
;;

run main = map 1;;

> cor-out +ir -print
> proc f11(captures_: [ `0 {} ], y: int): int
> {
>   let captures_stack: {} = @get_union_struct<captures_>;
>   let var: int = 1;
>   let var1: int = @call_kfn(add, y, var);
>   return var1;
> }
> 
> proc map2(captures_1: [ `0 {} ], x: int): int
> {
>   let captures_stack1: {} = @get_union_struct<captures_1>;
>   let struct: {} = @make_struct{};
>   let f: [ `0 {} ] = @make_union<0, struct>;
>   let cond: int = @get_union_id<f>;
>   switch cond {
>   0 -> { @call_direct(f11, f, x) }
>   } in join join;
>   return join;
> }
> 
> proc main_thunk(): int
> {
>   let struct1: {} = @make_struct{};
>   let var2: [ `0 {} ] = @make_union<0, struct1>;
>   let var3: int = 1;
>   let cond1: int = @get_union_id<var2>;
>   switch cond1 {
>   0 -> { @call_direct(map2, var2, var3) }
>   } in join join1;
>   return join1;
> }
> 
> global main: int = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = 2
>      > 2