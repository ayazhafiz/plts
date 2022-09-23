# cor +solve -elab
# cor +mono -print
# cor +eval -print
let x = A1 B2 in
let result = when x is
#                 ^
  | A1 B1 | A2 B2 | A3 B3 -> R1
  | A1 B2 | A2 B1 -> R2
  | A1 B3 | A2 B3 -> R3
in result
#  ^^^^^^

> cor-out +solve -elab
> let x = A1 B2 in
> let result = when x is
> #                 ^ [A1 [B1, B2, B3], A2 [B1, B2, B3], A3 [B3]]
>   | A1 B1 | A2 B2 | A3 B3 -> R1
>   | A1 B2 | A2 B1 -> R2
>   | A1 B3 | A2 B3 -> R3
> in result
> #  ^^^^^^ [R1, R2, R3]
> 

> cor-out +mono -print
> let %1 : {} = @build_struct;
> let %2 : [ `0, `1, `2 ] = @build_tag 1 %1;
> let %3 : { [ `0, `1, `2 ] } = @build_struct %2;
> let %4 : [ `0 [ `0, `1, `2 ], `1 [ `0, `1, `2 ], `2 [ `0 ] ] = @build_tag 0 %3;
> let x : [ `0 [ `0, `1, `2 ], `1 [ `0, `1, `2 ], `2 [ `0 ] ] = %4;
> let %8 : int = @get_tag_id x;
> switch %8 {
>   0: {
>     let %10 : { [ `0, `1, `2 ] } = @get_union_struct x;
>     let %5 : [ `0, `1, `2 ] = @get_field 0 %10;
>     let %11 : int = @get_tag_id %5;
>     switch %11 {
>       0: {
>         let %13 : {} = @get_union_struct %5;
>         let %14 : {} = @build_struct;
>         let %15 : [ `0, `1, `2 ] = @build_tag 0 %14;
>         feed %15
>       }
>       1: {
>         let %16 : {} = @get_union_struct %5;
>         let %17 : {} = @build_struct;
>         let %18 : [ `0, `1, `2 ] = @build_tag 1 %17;
>         feed %18
>       }
>       2: {
>         let %19 : {} = @get_union_struct %5;
>         let %20 : {} = @build_struct;
>         let %21 : [ `0, `1, `2 ] = @build_tag 2 %20;
>         feed %21
>       }
>     } in join %12 : [ `0, `1, `2 ]
>     feed %12
>   }
>   1: {
>     let %22 : { [ `0, `1, `2 ] } = @get_union_struct x;
>     let %6 : [ `0, `1, `2 ] = @get_field 0 %22;
>     let %23 : int = @get_tag_id %6;
>     switch %23 {
>       0: {
>         let %25 : {} = @get_union_struct %6;
>         let %26 : {} = @build_struct;
>         let %27 : [ `0, `1, `2 ] = @build_tag 1 %26;
>         feed %27
>       }
>       1: {
>         let %28 : {} = @get_union_struct %6;
>         let %29 : {} = @build_struct;
>         let %30 : [ `0, `1, `2 ] = @build_tag 0 %29;
>         feed %30
>       }
>       2: {
>         let %31 : {} = @get_union_struct %6;
>         let %32 : {} = @build_struct;
>         let %33 : [ `0, `1, `2 ] = @build_tag 2 %32;
>         feed %33
>       }
>     } in join %24 : [ `0, `1, `2 ]
>     feed %24
>   }
>   2: {
>     let %34 : { [ `0 ] } = @get_union_struct x;
>     let %7 : [ `0 ] = @get_field 0 %34;
>     let %37 : {} = @get_union_struct %7;
>     let %38 : {} = @build_struct;
>     let %39 : [ `0, `1, `2 ] = @build_tag 0 %38;
>     feed %39
>   }
> } in join %9 : [ `0, `1, `2 ]
> let result : [ `0, `1, `2 ] = %9;
> ret result

> cor-out +eval -print
> R2 {}