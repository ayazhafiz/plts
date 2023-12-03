# cor +ir -print
# cor +eval -print

let poly = \x ->
  let f = \x -> x in
  A (f "") x
;;

run main =
  A (poly 1) (poly "")
;;

> cor-out +ir -print
> proc f1(captures_: box<erased>, x1: str): str
> {
>   let captures_box: box<{}> = @ptr_cast(captures_ as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   return x1;
> }
> 
> proc clos_poly1(captures_3: box<erased>, x: str): [ `0 { str, str } ]
> {
>   let captures_box2: box<{}> = @ptr_cast(captures_3 as box<{}>);
>   let captures_stack2: {} = @get_boxed<captures_box2>;
>   let captures_stack_3: {} = @make_struct{};
>   let captures_box_3: box<{}> = @make_box(captures_stack_3);
>   let captures_6: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<f1>;
>   let f1: { *fn, box<erased> } = @make_struct{ fn_ptr_3, captures_6 };
>   let fnptr1: *fn = @get_struct_field<f1, 0>;
>   let captures1: box<erased> = @get_struct_field<f1, 1>;
>   let var3: str = "";
>   let var4: str = @call_indirect(fnptr1, captures1, var3);
>   let struct1: { str, str } = @make_struct{ var4, x };
>   let var5: [ `0 { str, str } ] = @make_union<0, struct1>;
>   return var5;
> }
> 
> proc clos_poly(captures_1: box<erased>, x: int): [ `0 { str, int } ]
> {
>   let captures_box1: box<{}> = @ptr_cast(captures_1 as box<{}>);
>   let captures_stack1: {} = @get_boxed<captures_box1>;
>   let captures_stack_2: {} = @make_struct{};
>   let captures_box_2: box<{}> = @make_box(captures_stack_2);
>   let captures_5: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<f1>;
>   let f1: { *fn, box<erased> } = @make_struct{ fn_ptr_2, captures_5 };
>   let fnptr: *fn = @get_struct_field<f1, 0>;
>   let captures: box<erased> = @get_struct_field<f1, 1>;
>   let var: str = "";
>   let var1: str = @call_indirect(fnptr, captures, var);
>   let struct: { str, int } = @make_struct{ var1, x };
>   let var2: [ `0 { str, int } ] = @make_union<0, struct>;
>   return var2;
> }
> 
> proc poly_thunk1(): { *fn, box<erased> }
> {
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_4: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos_poly1>;
>   let poly_closure1: { *fn, box<erased> }
>     = @make_struct{ fn_ptr_1, captures_4 };
>   return poly_closure1;
> }
> 
> proc poly_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_2: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_poly>;
>   let poly_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_2 };
>   return poly_closure;
> }
> 
> global poly2: { *fn, box<erased> } = @call_direct(poly_thunk1);
> 
> global poly1: { *fn, box<erased> } = @call_direct(poly_thunk);
> 
> proc main_thunk(): [ `0 { [ `0 { str, int } ], [ `0 { str, str } ] } ]
> {
>   let fnptr2: *fn = @get_struct_field<poly1, 0>;
>   let captures2: box<erased> = @get_struct_field<poly1, 1>;
>   let var6: int = 1;
>   let var7: [ `0 { str, int } ] = @call_indirect(fnptr2, captures2, var6);
>   let fnptr3: *fn = @get_struct_field<poly2, 0>;
>   let captures3: box<erased> = @get_struct_field<poly2, 1>;
>   let var8: str = "";
>   let var9: [ `0 { str, str } ] = @call_indirect(fnptr3, captures3, var8);
>   let struct2: { [ `0 { str, int } ], [ `0 { str, str } ] }
>     = @make_struct{ var7, var9 };
>   let var10: [ `0 { [ `0 { str, int } ], [ `0 { str, str } ] } ]
>     = @make_union<0, struct2>;
>   return var10;
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