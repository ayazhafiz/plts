# cor +parse -print
# cor +can -print
# cor +solve -elab
# cor +ir -print
# Task.roc
Task v op : (v -> op) -> op

sig await : Task a op -> (a -> Task b op) -> Task b op
let await = \fromResult -> \next ->
    \continue ->
        fromResult (\result ->
#       ^^^^^^^^^^
            let inner = next result in
            inner continue)
;;

# StdinEffect.roc
OpIn a b : [
    StdinLine (Str -> OpIn a b),
    Done a,
]b

# Stdin.roc
sig lineIn : Task Str (OpIn * *)
let lineIn = \toNext -> StdinLine (\s -> toNext s)
;;

# StdoutEffect.roc
OpOut a b : [
    StdoutLine Str ({} -> OpOut a b),
    Done a,
]b

# Stdout.roc
sig lineOut : Str -> Task {} (OpOut * *)
let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
;;

# Platform
# really, we want a syntax like [Done a](OpIn a)(OpOut a) here
Op a : [
    StdinLine (Str -> Op a),
    StdoutLine Str ({} -> Op a),
    Done a,
]

sig main : Task {} (Op *)
run main = await lineIn (\s -> lineOut s)
#          ^^^^^          ^    ^^^^^^^
;;

> cor-out +parse -print
> Task 'v 'op : ('v1 -> 'op1) -> 'op2
> 
> sig await :
>   (Task 'a 'op)
>     -> ('a1 -> Task 'b 'op1) -> Task 'b1 'op2
> let await =
>   \fromResult ->
>     \next ->
>       \continue ->
>         fromResult
>           \result ->
>             (let inner = next result in
>             inner continue)
> 
> OpIn 'a 'b :
>   [
>      StdinLine (Str -> OpIn 'a1 'b1),
>      Done 'a2
>   ]'b2
> 
> sig lineIn : Task Str (OpIn '* '*)
> let lineIn =
>   \toNext -> (StdinLine \s -> toNext s)
> 
> OpOut 'a 'b :
>   [
>      StdoutLine Str ({} -> OpOut 'a1 'b1),
>      Done 'a2
>   ]'b2
> 
> sig lineOut : Str -> Task {} (OpOut '* '*)
> let lineOut =
>   \s -> \toNext -> (StdoutLine s \x -> toNext x)
> 
> Op 'a :
>   [
>      StdinLine (Str -> Op 'a1),
>      StdoutLine Str ({} -> Op 'a2),
>      Done 'a3
>   ]
> 
> sig main : Task {} (Op '*)
> run main = await lineIn \s -> lineOut s

> cor-out +can -print
> Task 'v 'op : ('v -> 'op) -> 'op
> 
> sig await :
>   (Task 'a 'op)
>     -> ('a -> Task 'b 'op) -> Task 'b 'op
> let await =
>   \fromResult ->
>     \next ->
>       \continue ->
>         fromResult
>           \result ->
>             (let inner = next result in
>             inner continue)
> 
> OpIn 'a 'b :
>   [
>      StdinLine (Str -> %<..OpIn 'a 'b>),
>      Done 'a
>   ]'b
> 
> sig lineIn : Task Str %(OpIn '* '*)
> let lineIn =
>   \toNext -> (StdinLine \s -> toNext s)
> 
> OpOut 'a 'b :
>   [
>      StdoutLine Str ({} -> %<..OpOut 'a 'b>),
>      Done 'a
>   ]'b
> 
> sig lineOut : Str -> Task {} %(OpOut '* '*)
> let lineOut =
>   \s -> \toNext -> (StdoutLine s \x -> toNext x)
> 
> Op 'a :
>   [
>      StdinLine (Str -> %<..Op 'a>),
>      StdoutLine Str ({} -> %<..Op 'a>),
>      Done 'a
>   ]
> 
> sig main : Task {} %(Op '*)
> run main = await lineIn \s -> lineOut s

> cor-out +solve -elab
> # Task.roc
> Task v op : (v -> op) -> op
> 
> sig await : Task a op -> (a -> Task b op) -> Task b op
> let await = \fromResult -> \next ->
>     \continue ->
>         fromResult (\result ->
> #       ^^^^^^^^^^ Task 'a 'op
>             let inner = next result in
>             inner continue)
> ;;
> 
> # StdinEffect.roc
> OpIn a b : [
>     StdinLine (Str -> OpIn a b),
>     Done a,
> ]b
> 
> # Stdin.roc
> sig lineIn : Task Str (OpIn * *)
> let lineIn = \toNext -> StdinLine (\s -> toNext s)
> ;;
> 
> # StdoutEffect.roc
> OpOut a b : [
>     StdoutLine Str ({} -> OpOut a b),
>     Done a,
> ]b
> 
> # Stdout.roc
> sig lineOut : Str -> Task {} (OpOut * *)
> let lineOut = \s -> (\toNext -> StdoutLine s (\x -> toNext x))
> ;;
> 
> # Platform
> # really, we want a syntax like [Done a](OpIn a)(OpOut a) here
> Op a : [
>     StdinLine (Str -> Op a),
>     StdoutLine Str ({} -> Op a),
>     Done a,
> ]
> 
> sig main : Task {} (Op *)
> run main = await lineIn (\s -> lineOut s)
> #                              ^^^^^^^ Str -> Task {} %(OpOut ?* ?*)
> #                         ^ Str
> #          ^^^^^ (Task Str %(OpIn ?* ?*))
> #          ^^^^^   -> (Str -> Task {} %(OpOut ?* ?*))
> #          ^^^^^        -> Task {} %(Op '*)
> ;;
> 

> cor-out +ir -print
> proc clos3(captures_3: box<erased>, result: []): []
> {
>   let captures_box3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }> =
>     @ptr_cast(
>       captures_3 as
>       box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack3: { { *fn, box<erased> }, { *fn, box<erased> } } =
>     @get_boxed<captures_box3>;
>   let continue: { *fn, box<erased> } = @get_struct_field<captures_stack3, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack3, 1>;
>   let fnptr1: *fn = @get_struct_field<next, 0>;
>   let captures1: box<erased> = @get_struct_field<next, 1>;
>   let inner: { *fn, box<erased> } = @call_indirect(fnptr1, captures1, result);
>   let fnptr2: *fn = @get_struct_field<inner, 0>;
>   let captures2: box<erased> = @get_struct_field<inner, 1>;
>   let var5: [] = @call_indirect(fnptr2, captures2, continue);
>   return var5;
> }
> 
> proc clos2(captures_2: box<erased>, continue: { *fn, box<erased> }): []
> {
>   let captures_box2: box<{ { *fn, box<erased> }, { *fn, box<erased> } }> =
>     @ptr_cast(
>       captures_2 as
>       box<{ { *fn, box<erased> }, { *fn, box<erased> } }>);
>   let captures_stack2: { { *fn, box<erased> }, { *fn, box<erased> } } =
>     @get_boxed<captures_box2>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack2, 0>;
>   let next: { *fn, box<erased> } = @get_struct_field<captures_stack2, 1>;
>   let fnptr: *fn = @get_struct_field<fromResult, 0>;
>   let captures: box<erased> = @get_struct_field<fromResult, 1>;
>   let captures_stack_3: { { *fn, box<erased> }, { *fn, box<erased> } } =
>     @make_struct{ continue, next };
>   let captures_box_3: box<{ { *fn, box<erased> }, { *fn, box<erased> } }> =
>     @make_box(captures_stack_3);
>   let captures_3: box<erased> = @ptr_cast(captures_box_3 as box<erased>);
>   let fn_ptr_3: *fn = @make_fn_ptr<clos3>;
>   let var3: { *fn, box<erased> } = @make_struct{ fn_ptr_3, captures_3 };
>   let var4: [] = @call_indirect(fnptr, captures, var3);
>   return var4;
> }
> 
> proc clos1(captures_1: box<erased>, next: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box1: box<{ { *fn, box<erased> } }> =
>     @ptr_cast(captures_1 as box<{ { *fn, box<erased> } }>);
>   let captures_stack1: { { *fn, box<erased> } } = @get_boxed<captures_box1>;
>   let fromResult: { *fn, box<erased> } = @get_struct_field<captures_stack1, 0>;
>   let captures_stack_2: { { *fn, box<erased> }, { *fn, box<erased> } } =
>     @make_struct{ fromResult, next };
>   let captures_box_2: box<{ { *fn, box<erased> }, { *fn, box<erased> } }> =
>     @make_box(captures_stack_2);
>   let captures_2: box<erased> = @ptr_cast(captures_box_2 as box<erased>);
>   let fn_ptr_2: *fn = @make_fn_ptr<clos2>;
>   let var2: { *fn, box<erased> } = @make_struct{ fn_ptr_2, captures_2 };
>   return var2;
> }
> 
> proc clos(captures_: box<erased>, fromResult: { *fn, box<erased> }):
>   { *fn, box<erased> }
> {
>   let captures_box: box<{}> = @ptr_cast(captures_ as box<{}>);
>   let captures_stack: {} = @get_boxed<captures_box>;
>   let captures_stack_1: { { *fn, box<erased> } } = @make_struct{ fromResult };
>   let captures_box_1: box<{ { *fn, box<erased> } }> =
>     @make_box(captures_stack_1);
>   let captures_1: box<erased> = @ptr_cast(captures_box_1 as box<erased>);
>   let fn_ptr_1: *fn = @make_fn_ptr<clos1>;
>   let var1: { *fn, box<erased> } = @make_struct{ fn_ptr_1, captures_1 };
>   return var1;
> }
> 
> proc await_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_: {} = @make_struct{};
>   let captures_box_: box<{}> = @make_box(captures_stack_);
>   let captures_: box<erased> = @ptr_cast(captures_box_ as box<erased>);
>   let fn_ptr_: *fn = @make_fn_ptr<clos>;
>   let var: { *fn, box<erased> } = @make_struct{ fn_ptr_, captures_ };
>   return var;
> }
> 
> global await: { *fn, box<erased> } = @call_direct(await_thunk);
> 
> proc clos5(captures_5: box<erased>, s: []): [ `0 { { *fn, box<erased> } } ]
> {
>   let captures_box5: box<{ { *fn, box<erased> } }> =
>     @ptr_cast(captures_5 as box<{ { *fn, box<erased> } }>);
>   let captures_stack5: { { *fn, box<erased> } } = @get_boxed<captures_box5>;
>   let toNext: { *fn, box<erased> } = @get_struct_field<captures_stack5, 0>;
>   let fnptr3: *fn = @get_struct_field<toNext, 0>;
>   let captures3: box<erased> = @get_struct_field<toNext, 1>;
>   let var9: [ `0 { { *fn, box<erased> } } ] =
>     @call_indirect(fnptr3, captures3, s);
>   return var9;
> }
> 
> proc clos4(captures_4: box<erased>, toNext: { *fn, box<erased> }):
>   [ `0 { { *fn, box<erased> } } ]
> {
>   let captures_box4: box<{}> = @ptr_cast(captures_4 as box<{}>);
>   let captures_stack4: {} = @get_boxed<captures_box4>;
>   let captures_stack_5: { { *fn, box<erased> } } = @make_struct{ toNext };
>   let captures_box_5: box<{ { *fn, box<erased> } }> =
>     @make_box(captures_stack_5);
>   let captures_5: box<erased> = @ptr_cast(captures_box_5 as box<erased>);
>   let fn_ptr_5: *fn = @make_fn_ptr<clos5>;
>   let var7: { *fn, box<erased> } = @make_struct{ fn_ptr_5, captures_5 };
>   let struct: { { *fn, box<erased> } } = @make_struct{ var7 };
>   let var8: [ `0 { { *fn, box<erased> } } ] = @make_union<0, struct>;
>   return var8;
> }
> 
> proc lineIn_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_4: {} = @make_struct{};
>   let captures_box_4: box<{}> = @make_box(captures_stack_4);
>   let captures_4: box<erased> = @ptr_cast(captures_box_4 as box<erased>);
>   let fn_ptr_4: *fn = @make_fn_ptr<clos4>;
>   let var6: { *fn, box<erased> } = @make_struct{ fn_ptr_4, captures_4 };
>   return var6;
> }
> 
> global lineIn: { *fn, box<erased> } = @call_direct(lineIn_thunk);
> 
> proc clos8(captures_8: box<erased>, x: []):
>   [ `0 { str, { *fn, box<erased> } } ]
> {
>   let captures_box8: box<{ { *fn, box<erased> } }> =
>     @ptr_cast(captures_8 as box<{ { *fn, box<erased> } }>);
>   let captures_stack8: { { *fn, box<erased> } } = @get_boxed<captures_box8>;
>   let toNext1: { *fn, box<erased> } = @get_struct_field<captures_stack8, 0>;
>   let fnptr4: *fn = @get_struct_field<toNext1, 0>;
>   let captures4: box<erased> = @get_struct_field<toNext1, 1>;
>   let var14: [ `0 { str, { *fn, box<erased> } } ] =
>     @call_indirect(fnptr4, captures4, x);
>   return var14;
> }
> 
> proc clos7(captures_7: box<erased>, toNext1: { *fn, box<erased> }):
>   [ `0 { str, { *fn, box<erased> } } ]
> {
>   let captures_box7: box<{ str }> = @ptr_cast(captures_7 as box<{ str }>);
>   let captures_stack7: { str } = @get_boxed<captures_box7>;
>   let s1: str = @get_struct_field<captures_stack7, 0>;
>   let captures_stack_8: { { *fn, box<erased> } } = @make_struct{ toNext1 };
>   let captures_box_8: box<{ { *fn, box<erased> } }> =
>     @make_box(captures_stack_8);
>   let captures_8: box<erased> = @ptr_cast(captures_box_8 as box<erased>);
>   let fn_ptr_8: *fn = @make_fn_ptr<clos8>;
>   let var12: { *fn, box<erased> } = @make_struct{ fn_ptr_8, captures_8 };
>   let struct1: { str, { *fn, box<erased> } } = @make_struct{ s1, var12 };
>   let var13: [ `0 { str, { *fn, box<erased> } } ] = @make_union<0, struct1>;
>   return var13;
> }
> 
> proc clos6(captures_6: box<erased>, s1: str): { *fn, box<erased> }
> {
>   let captures_box6: box<{}> = @ptr_cast(captures_6 as box<{}>);
>   let captures_stack6: {} = @get_boxed<captures_box6>;
>   let captures_stack_7: { str } = @make_struct{ s1 };
>   let captures_box_7: box<{ str }> = @make_box(captures_stack_7);
>   let captures_7: box<erased> = @ptr_cast(captures_box_7 as box<erased>);
>   let fn_ptr_7: *fn = @make_fn_ptr<clos7>;
>   let var11: { *fn, box<erased> } = @make_struct{ fn_ptr_7, captures_7 };
>   return var11;
> }
> 
> proc lineOut_thunk(): { *fn, box<erased> }
> {
>   let captures_stack_6: {} = @make_struct{};
>   let captures_box_6: box<{}> = @make_box(captures_stack_6);
>   let captures_6: box<erased> = @ptr_cast(captures_box_6 as box<erased>);
>   let fn_ptr_6: *fn = @make_fn_ptr<clos6>;
>   let var10: { *fn, box<erased> } = @make_struct{ fn_ptr_6, captures_6 };
>   return var10;
> }
> 
> global lineOut: { *fn, box<erased> } = @call_direct(lineOut_thunk);
> 
> proc clos9(captures_9: box<erased>, s2: str): { *fn, box<erased> }
> {
>   let captures_box9: box<{ { *fn, box<erased> } }> =
>     @ptr_cast(captures_9 as box<{ { *fn, box<erased> } }>);
>   let captures_stack9: { { *fn, box<erased> } } = @get_boxed<captures_box9>;
>   let lineOut: { *fn, box<erased> } = @get_struct_field<captures_stack9, 0>;
>   let fnptr7: *fn = @get_struct_field<lineOut, 0>;
>   let captures7: box<erased> = @get_struct_field<lineOut, 1>;
>   let var18: { *fn, box<erased> } = @call_indirect(fnptr7, captures7, s2);
>   return var18;
> }
> 
> proc main_thunk(): { *fn, box<erased> }
> {
>   let fnptr5: *fn = @get_struct_field<await, 0>;
>   let captures5: box<erased> = @get_struct_field<await, 1>;
>   let var15: { *fn, box<erased> } = @call_indirect(fnptr5, captures5, lineIn);
>   let fnptr6: *fn = @get_struct_field<var15, 0>;
>   let captures6: box<erased> = @get_struct_field<var15, 1>;
>   let captures_stack_9: { { *fn, box<erased> } } = @make_struct{ lineOut };
>   let captures_box_9: box<{ { *fn, box<erased> } }> =
>     @make_box(captures_stack_9);
>   let captures_9: box<erased> = @ptr_cast(captures_box_9 as box<erased>);
>   let fn_ptr_9: *fn = @make_fn_ptr<clos9>;
>   let var16: { *fn, box<erased> } = @make_struct{ fn_ptr_9, captures_9 };
>   let var17: { *fn, box<erased> } = @call_indirect(fnptr6, captures6, var16);
>   return var17;
> }
> 
> global main: { *fn, box<erased> } = @call_direct(main_thunk);
> 
> entry main;