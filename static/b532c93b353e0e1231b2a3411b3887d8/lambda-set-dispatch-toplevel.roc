# cor +solve -elab
# cor +mono -print
# cor +ir -print
# cor +eval -print

let f1 = \x -> ~add x 1;;
#   ^^
let f2 = \x -> ~add x 2;;
#   ^^
let f3 = \x -> ~add x 3;;
#   ^^

let f = \t -> when t is
  | T1 -> f1
  | T2 -> f2
  | T3 -> f3
  end
;;

run x = f T2 0
;;

> cor-out +solve -elab
> 
> let f1 = \x -> ~add x 1;;
> #   ^^ Int -[f1, f2, f3]-> Int
> let f2 = \x -> ~add x 2;;
> #   ^^ Int -[f1, f2, f3]-> Int
> let f3 = \x -> ~add x 3;;
> #   ^^ Int -[f1, f2, f3]-> Int
> 
> let f = \t -> when t is
>   | T1 -> f1
>   | T2 -> f2
>   | T3 -> f3
>   end
> ;;
> 
> run x = f T2 0
> ;;
> 

> cor-out +mono -print
> specializations:
>   let f12 = \x -[f12]-> add x 1
>   
>   let f22 = \x1 -[f22]-> add x1 2
>   
>   let f32 = \x2 -[f32]-> add x2 3
>   
>   let f5 = \t -[f5]->
>     when t is | T1 -> f12 | T2 -> f22 | T3 -> f32
>     end
>   
>   let x3 = (f5 (T2 )) 0
>   
>   
> entry_points:
>   x3

> cor-out +ir -print
> proc f22(captures_1: [ `0 {}, `1 {}, `2 {} ], x1: int): int
> {
>   let captures_stack1: {} = @get_union_struct<captures_1>;
>   let var2: int = 2;
>   let var3: int = @call_kfn(add, x1, var2);
>   return var3;
> }
> 
> proc f12(captures_: [ `0 {}, `1 {}, `2 {} ], x: int): int
> {
>   let captures_stack: {} = @get_union_struct<captures_>;
>   let var: int = 1;
>   let var1: int = @call_kfn(add, x, var);
>   return var1;
> }
> 
> proc f5(captures_3: [ `0 {} ], t: [ `0 {}, `1 {}, `2 {} ]):
>   [ `0 {}, `1 {}, `2 {} ]
> {
>   let captures_stack3: {} = @get_union_struct<captures_3>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let payload: {} = @get_union_struct<t>;
>     let struct: {} = @make_struct{};
>     @make_union<0, struct>
>   }
>   1 -> {
>     let payload1: {} = @get_union_struct<t>;
>     let struct1: {} = @make_struct{};
>     @make_union<1, struct1>
>   }
>   2 -> {
>     let payload2: {} = @get_union_struct<t>;
>     let struct2: {} = @make_struct{};
>     @make_union<2, struct2>
>   }
>   } in join join;
>   return join;
> }
> 
> proc f32(captures_2: [ `0 {}, `1 {}, `2 {} ], x2: int): int
> {
>   let captures_stack2: {} = @get_union_struct<captures_2>;
>   let var4: int = 3;
>   let var5: int = @call_kfn(add, x2, var4);
>   return var5;
> }
> 
> proc x_thunk(): int
> {
>   let struct3: {} = @make_struct{};
>   let var6: [ `0 {} ] = @make_union<0, struct3>;
>   let struct4: {} = @make_struct{};
>   let var7: [ `0 {}, `1 {}, `2 {} ] = @make_union<1, struct4>;
>   let cond: int = @get_union_id<var6>;
>   switch cond {
>   0 -> { @call_direct(f5, var6, var7) }
>   } in join join1;
>   let var8: int = 0;
>   let cond1: int = @get_union_id<join1>;
>   switch cond1 {
>   0 -> { @call_direct(f12, join1, var8) }
>   1 -> { @call_direct(f22, join1, var8) }
>   2 -> { @call_direct(f32, join1, var8) }
>   } in join join2;
>   return join2;
> }
> 
> global x3: int = @call_direct(x_thunk);
> 
> entry x3;

> cor-out +eval -print
> x3 = 2
>    > 2