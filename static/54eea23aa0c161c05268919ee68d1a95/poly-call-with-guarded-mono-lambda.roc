# cor +mono -print
# cor +ir -print
# cor +eval -print

let poly = \x ->
  let f = \x -> x in
  A (f "") x
;;

run main =
  A (poly 1) (poly "")
;;

> cor-out +mono -print
> specializations:
>   let f11 = \x1 -[f11]-> x1
>   
>   let poly2 = \x -[poly2]->
>     let f = \x1 -[f11]-> x1 in
>     A (f "") x
>   
>   let poly3 = \x -[poly3]->
>     let f = \x1 -[f11]-> x1 in
>     A (f "") x
>   
>   let main = A (poly2 1) (poly3 "")
>   
>   
> entry_points:
>   main

> cor-out +ir -print
> proc f11(captures_: [ `0 {} ], x1: str): str
> {let captures_stack: {} = @get_union_struct<captures_>;
>  return x1;}
> 
> proc poly3(captures_2: [ `0 {} ], x: str): [ `0 { str, str } ]
> {
>   let captures_stack2: {} = @get_union_struct<captures_2>;
>   let struct2: {} = @make_struct{};
>   let f: [ `0 {} ] = @make_union<0, struct2>;
>   let var2: str = "";
>   let cond1: int = @get_union_id<f>;
>   switch cond1 {
>   0 -> { @call_direct(f11, f, var2) }
>   } in join join1;
>   let struct3: { str, str } = @make_struct{ join1, x };
>   let var3: [ `0 { str, str } ] = @make_union<0, struct3>;
>   return var3;
> }
> 
> proc poly2(captures_1: [ `0 {} ], x: int): [ `0 { str, int } ]
> {
>   let captures_stack1: {} = @get_union_struct<captures_1>;
>   let struct: {} = @make_struct{};
>   let f: [ `0 {} ] = @make_union<0, struct>;
>   let var: str = "";
>   let cond: int = @get_union_id<f>;
>   switch cond {
>   0 -> { @call_direct(f11, f, var) }
>   } in join join;
>   let struct1: { str, int } = @make_struct{ join, x };
>   let var1: [ `0 { str, int } ] = @make_union<0, struct1>;
>   return var1;
> }
> 
> proc main_thunk(): [ `0 { [ `0 { str, int } ], [ `0 { str, str } ] } ]
> {
>   let struct4: {} = @make_struct{};
>   let var4: [ `0 {} ] = @make_union<0, struct4>;
>   let var5: int = 1;
>   let cond2: int = @get_union_id<var4>;
>   switch cond2 {
>   0 -> { @call_direct(poly2, var4, var5) }
>   } in join join2;
>   let struct5: {} = @make_struct{};
>   let var6: [ `0 {} ] = @make_union<0, struct5>;
>   let var7: str = "";
>   let cond3: int = @get_union_id<var6>;
>   switch cond3 {
>   0 -> { @call_direct(poly3, var6, var7) }
>   } in join join3;
>   let struct6: { [ `0 { str, int } ], [ `0 { str, str } ] }
>     = @make_struct{ join2, join3 };
>   let var8: [ `0 { [ `0 { str, int } ], [ `0 { str, str } ] } ]
>     = @make_union<0, struct6>;
>   return var8;
> }
> 
> global main:
>   [ `0 { [ `0 { str, int } ], [ `0 { str, str } ] } ]
>   = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = [0 [0 [] 1] [0 [] []]]
>      > A (A "" 1) (A "" "")