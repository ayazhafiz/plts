# cor +mono -print
# cor +ir -print
# cor +eval -print

sig after : ({} -> ([A,B] -> [A,B])) -> ([A,B] -> [A,B])
let after = \cont ->
  let f = \a ->
    let inner = cont {}
    in inner a
  in f
;;

let nestForever =
  let nester = \x ->
    let f = \r -> \t -> when t is
    | A -> B
    | B -> nester r A
    end
    in
    after f
  in
  nester
;;

run main = nestForever {} B
;;

> cor-out +mono -print
> specializations:
>   let nester11 = \x -[nester11]->
>     let f1 =
>       \r -[f31 nester]->
>         \t -[lam1 nester r]->
>           when t is
>             | A -> B 
>             | B -> (nester r) (A )
>           end
>     in
>     after2 f1
>   
>   let f31 = \r -[f31 nester]->
>     \t -[lam1 nester r]->
>       when t is | A -> B  | B -> (nester r) (
>                                         A )
>       end
>   
>   let lam1 = \t -[lam1 nester r]->
>     when t is | A -> B  | B -> (nester r) (
>                                         A )
>     end
>   
>   let f21 = \a -[f21 cont]->
>     let inner = cont {} in
>     inner a
>   
>   let after2 = \cont -[after2]->
>     let f =
>       \a -[f21 cont]->
>         (let inner = cont {} in
>         inner a)
>     in
>     f
>   
>   let nestForever1 =
>     let nester =
>       \x -[nester11]->
>         let f1 =
>           \r -[f31 nester]->
>             \t -[lam1 nester r]->
>               when t is
>                 | A -> B 
>                 | B -> (nester r) (A )
>               end
>         in
>         after2 f1
>     in
>     nester
>   
>   let main = (nestForever1 {}) (B )
>   
>   
> entry_points:
>   main

> cor-out +ir -print
> proc nestForever_thunk(): [ `0 {} ]
> {
>   let struct7: {} = @make_struct{};
>   let nester: [ `0 {} ] = @make_union<0, struct7>;
>   return nester;
> }
> 
> proc after2(captures_3: [ `0 {} ], cont: [ `0 { [ `0 {} ] } ]):
>   [ `0 { [ `0 { [ `0 {} ] } ] } ]
> {
>   let captures_stack4: {} = @get_union_struct<captures_3>;
>   let struct6: { [ `0 { [ `0 {} ] } ] } = @make_struct{ cont };
>   let f: [ `0 { [ `0 { [ `0 {} ] } ] } ] = @make_union<0, struct6>;
>   return f;
> }
> 
> proc f31(captures_: [ `0 { [ `0 {} ] } ], r: {}): [ `0 { [ `0 {} ], {} } ]
> {
>   let captures_stack1: { [ `0 {} ] } = @get_union_struct<captures_>;
>   let nester: [ `0 {} ] = @get_struct_field<captures_stack1, 0>;
>   let struct3: { [ `0 {} ], {} } = @make_struct{ nester, r };
>   let var1: [ `0 { [ `0 {} ], {} } ] = @make_union<0, struct3>;
>   return var1;
> }
> 
> global nestForever1: [ `0 {} ] = @call_direct(nestForever_thunk);
> 
> proc nester11(captures_nester: [ `0 {} ], x: {}):
>   [ `0 { [ `0 { [ `0 {} ] } ] } ]
> {
>   let captures_stack: {} = @get_union_struct<captures_nester>;
>   let struct: {} = @make_struct{};
>   let nester: [ `0 {} ] = @make_union<0, struct>;
>   let struct1: { [ `0 {} ] } = @make_struct{ nester };
>   let f1: [ `0 { [ `0 {} ] } ] = @make_union<0, struct1>;
>   let struct2: {} = @make_struct{};
>   let var: [ `0 {} ] = @make_union<0, struct2>;
>   let cond: int = @get_union_id<var>;
>   switch cond {
>   0 -> { @call_direct(after2, var, f1) }
>   } in join join;
>   return join;
> }
> 
> proc f21(captures_2: [ `0 { [ `0 { [ `0 {} ] } ] } ], a: [ `0 {}, `1 {} ]):
>   [ `0 {}, `1 {} ]
> {
>   let captures_stack3: { [ `0 { [ `0 {} ] } ] } = @get_union_struct<captures_2>;
>   let cont: [ `0 { [ `0 {} ] } ] = @get_struct_field<captures_stack3, 0>;
>   let var3: {} = @make_struct{};
>   let cond3: int = @get_union_id<cont>;
>   switch cond3 {
>   0 -> { @call_direct(f31, cont, var3) }
>   } in join join4;
>   let inner: [ `0 { [ `0 {} ], {} } ] = join4;
>   let cond4: int = @get_union_id<inner>;
>   switch cond4 {
>   0 -> { @call_direct(lam1, inner, a) }
>   } in join join5;
>   return join5;
> }
> 
> proc lam1(captures_1: [ `0 { [ `0 {} ], {} } ], t: [ `0 {}, `1 {} ]):
>   [ `0 {}, `1 {} ]
> {
>   let captures_stack2: { [ `0 {} ], {} } = @get_union_struct<captures_1>;
>   let nester: [ `0 {} ] = @get_struct_field<captures_stack2, 0>;
>   let r: {} = @get_struct_field<captures_stack2, 1>;
>   let discr: int = @get_union_id<t>;
>   switch discr {
>   0 -> {
>     let payload: {} = @get_union_struct<t>;
>     let struct4: {} = @make_struct{};
>     @make_union<1, struct4>
>   }
>   1 -> {
>     let payload1: {} = @get_union_struct<t>;
>     let cond1: int = @get_union_id<nester>;
>     switch cond1 {
>     0 -> { @call_direct(nester11, nester, r) }
>     } in join join1;
>     let struct5: {} = @make_struct{};
>     let var2: [ `0 {}, `1 {} ] = @make_union<0, struct5>;
>     let cond2: int = @get_union_id<join1>;
>     switch cond2 {
>     0 -> { @call_direct(f21, join1, var2) }
>     } in join join2;
>     join2
>   }
>   } in join join3;
>   return join3;
> }
> 
> proc main_thunk(): [ `0 {}, `1 {} ]
> {
>   let var4: {} = @make_struct{};
>   let cond5: int = @get_union_id<nestForever1>;
>   switch cond5 {
>   0 -> { @call_direct(nester11, nestForever1, var4) }
>   } in join join6;
>   let struct8: {} = @make_struct{};
>   let var5: [ `0 {}, `1 {} ] = @make_union<1, struct8>;
>   let cond6: int = @get_union_id<join6>;
>   switch cond6 {
>   0 -> { @call_direct(f21, join6, var5) }
>   } in join join7;
>   return join7;
> }
> 
> global main: [ `0 {}, `1 {} ] = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = [1]
>      > B 