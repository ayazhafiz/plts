# cor +solve -elab
# cor +mono -print
let x : [A, B, C] = A in
#   ^
let z : [A, B, C, D, E] = when x is
#   ^
  | A | B as y -> y
#            ^    ^
#   ^^^^^^^^^^
  | C -> A
#        ^
#   ^
in z

> cor-out +solve -elab
> let x : [A, B, C] = A in
> #   ^ [A, B, C]
> let z : [A, B, C, D, E] = when x is
> #   ^ [A, B, C, D, E]
>   | A | B as y -> y
> #                 ^ [A, B, C, D, E]
> #            ^ [A, B, C, D, E]
> #   ^^^^^^^^^^ [A, B, C]
>   | C -> A
> #        ^ [A, B, C, D, E]
> #   ^ [A, B, C]
> in z
> 

> cor-out +mono -print
> let %1 : {} = @build_struct;
> let %2 : [ `0, `1, `2 ] = @build_tag 0 %1;
> let x : [ `0, `1, `2 ] = %2;
> let %3 : int = @get_tag_id x;
> switch %3 {
>   0: {
>     let %5 : {} = @get_union_struct x;
>     let %6 : int = @get_tag_id x;
>     switch %6 {
>       0: {
>         let %8 : {} = @get_union_struct x;
>         let %9 : {} = @build_struct;
>         let %10 : [ `0, `1, `2, `3, `4 ] = @build_tag 0 %9;
>         feed %10
>       }
>       1: {
>         let %11 : {} = @get_union_struct x;
>         let %12 : {} = @build_struct;
>         let %13 : [ `0, `1, `2, `3, `4 ] = @build_tag 1 %12;
>         feed %13
>       }
>       2: {
>         let %14 : {} = @get_union_struct x;
>         let %15 : {} = @build_struct;
>         let %16 : [ `0, `1, `2, `3, `4 ] = @build_tag 2 %15;
>         feed %16
>       }
>     } in join %7 : [ `0, `1, `2, `3, `4 ]
>     let y : [ `0, `1, `2, `3, `4 ] = %7;
>     feed y
>   }
>   1: {
>     let %17 : {} = @get_union_struct x;
>     let %18 : int = @get_tag_id x;
>     switch %18 {
>       0: {
>         let %20 : {} = @get_union_struct x;
>         let %21 : {} = @build_struct;
>         let %22 : [ `0, `1, `2, `3, `4 ] = @build_tag 0 %21;
>         feed %22
>       }
>       1: {
>         let %23 : {} = @get_union_struct x;
>         let %24 : {} = @build_struct;
>         let %25 : [ `0, `1, `2, `3, `4 ] = @build_tag 1 %24;
>         feed %25
>       }
>       2: {
>         let %26 : {} = @get_union_struct x;
>         let %27 : {} = @build_struct;
>         let %28 : [ `0, `1, `2, `3, `4 ] = @build_tag 2 %27;
>         feed %28
>       }
>     } in join %19 : [ `0, `1, `2, `3, `4 ]
>     let y : [ `0, `1, `2, `3, `4 ] = %19;
>     feed y
>   }
>   2: {
>     let %29 : {} = @get_union_struct x;
>     let %30 : {} = @build_struct;
>     let %31 : [ `0, `1, `2, `3, `4 ] = @build_tag 0 %30;
>     feed %31
>   }
> } in join %4 : [ `0, `1, `2, `3, `4 ]
> let z : [ `0, `1, `2, `3, `4 ] = %4;
> ret z