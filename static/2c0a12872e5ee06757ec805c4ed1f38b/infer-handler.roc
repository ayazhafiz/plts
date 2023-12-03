# cor +solve -print
# cor +mono -print
# cor +ir -print

run main_handler =
    let handle = \op -> when op is
        | StdoutLine s f -> handle (f {})
        | Done x -> Done x
    end
    in
    handle (Done 1)
;;

> cor-out +solve -print
> let main_handler =
>   let handle =
>     \op -[handle]->
>       when op is
>         | StdoutLines f -> handle (f {})
>         | Donex -> Done x
>       end
>   in
>   handle (Done 1)

> cor-out +mono -print
> specializations:
>   let handle1 = \-[handle1]-> op
>     when op is
>       | StdoutLines f -> handle1 (f {})
>       | Donex -> Done x
>     end
>   
>   let main_handler =
>     let handle1 =
>       \op -[handle1]->
>         when op is
>           | StdoutLines f -> handle1 (f {})
>           | Donex -> Done x
>         end
>     in
>     handle1 (Done 1)
>   
>   
> entry_points:
>   main_handler

> cor-out +ir -print
> proc handle1(
>   captures_handle: box<erased>,
>    op: [ `0 { int }, `1 { [], { *fn, box<erased> } } ]):
>   [ `0 { int } ]
> {
>   let captures_box: box<{}> = @ptr_cast(captures_handle as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let rec_fn_ptr_handle: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> }
>     = @make_struct{ rec_fn_ptr_handle, captures_handle };
>   let discr: int = @get_union_id<op>;
>   switch discr {
>   0 -> {
>     let payload1: { int } = @get_union_struct<op>;
>     let x: int = @get_struct_field<payload1, 0>;
>     let struct: { int } = @make_struct{ x };
>     @make_union<0, struct>
>   }
>   1 -> {
>     let payload: { [], { *fn, box<erased> } } = @get_union_struct<op>;
>     let s: [] = @get_struct_field<payload, 0>;
>     let f: { *fn, box<erased> } = @get_struct_field<payload, 1>;
>     let fnptr: *fn = @get_struct_field<handle1, 0>;
>     let captures: box<erased> = @get_struct_field<handle1, 1>;
>     let fnptr1: *fn = @get_struct_field<f, 0>;
>     let captures1: box<erased> = @get_struct_field<f, 1>;
>     let var: {} = @make_struct{};
>     let var1: [ `0 { int }, `1 { [], { *fn, box<erased> } } ]
>       = @call_indirect(fnptr1, captures1, var);
>     @call_indirect(fnptr, captures, var1)
>   }
>   } in join join;
>   return join;
> }
> 
> proc main_handler_thunk(): [ `0 { int } ]
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<handle1>;
>   let handle1: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   let fnptr2: *fn = @get_struct_field<handle1, 0>;
>   let captures2: box<erased> = @get_struct_field<handle1, 1>;
>   let var2: int = 1;
>   let struct1: { int } = @make_struct{ var2 };
>   let var3: [ `0 { int }, `1 { [], { *fn, box<erased> } } ]
>     = @make_union<0, struct1>;
>   let var4: [ `0 { int } ] = @call_indirect(fnptr2, captures2, var3);
>   return var4;
> }
> 
> global main_handler: [ `0 { int } ] = @call_direct(main_handler_thunk);
> 
> entry main_handler;