# cor +ir -print
# cor +eval -print

let map = \x ->
  let f = \y -> ~add y 1 in
  f x
;;

run main = map 1;;

> cor-out +ir -print
> proc f2(captures_: box<erased>, y: int): int
> {
>   let captures_box: box<{}> = @ptr_cast(captures_ as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let var: int = 1;
>   let var1: int = @call_kfn(add, y, var);
>   return var1;
> }
> 
> proc clos_map2(captures_1: box<erased>, x: int): int
> {
>   let captures_box1: box<{}> = @ptr_cast(captures_1 as box<{}>);
>   let captures_stack1: {} = @get_boxed<captures_box1>;
>   let captures_stack_1: {} = @make_struct{};
>   let captures_box_1: box<{}> = @make_box(captures_stack_1);
>   let captures_3: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<f2>;
>   let f: { *fn, box<erased> } = @make_struct{ fn_ptr_1, captures_3 };
>   let fnptr: *fn = @get_struct_field<f, 0>;
>   let captures: box<erased> = @get_struct_field<f, 1>;
>   let var2: int = @call_indirect(fnptr, captures, x);
>   return var2;
> }
> 
> proc map2_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_2: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos_map2>;
>   let map2_closure: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_2 };
>   return map2_closure;
> }
> 
> global map2: { *fn, box<erased> } = @call_direct(map2_thunk);
> 
> proc main_thunk(): int
> {
>   let fnptr1: *fn = @get_struct_field<map2, 0>;
>   let captures1: box<erased> = @get_struct_field<map2, 1>;
>   let var3: int = 1;
>   let var4: int = @call_indirect(fnptr1, captures1, var3);
>   return var4;
> }
> 
> global main: int = @call_direct(main_thunk);
> 
> entry main;

> cor-out +eval -print
> main = 2
>      > 2