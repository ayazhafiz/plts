# cor +solve -elab
# cor +ir -print
# cor +eval -print
let x : [A, B, C] = B in
#   ^
let z = match x with
#   ^
  | A | B as y -> y
#            ^    ^
#   ^^^^^^^^^^
  | C -> A
#        ^
#   ^
in z

> cor-out +solve -elab
> let x : [A, B, C] = B in
> #   ^ [A, B, C]
> let z = match x with
> #   ^ [A, B]
>   | A | B as y -> y
> #                 ^ [A, B]
> #            ^ [A, B]
> #   ^^^^^^^^^^ [A, B, C]
>   | C -> A
> #        ^ [A, B]
> #   ^ [A, B, C]
> in z
> 

> cor-out +ir -print
> let %1 : {} = @build_struct;
> let %2 : [ `0, `1, `2 ] = @build_tag 1 %1;
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
>         let %10 : [ `0, `1 ] = @build_tag 0 %9;
>         feed %10
>       }
>       1: {
>         let %11 : {} = @get_union_struct x;
>         let %12 : {} = @build_struct;
>         let %13 : [ `0, `1 ] = @build_tag 1 %12;
>         feed %13
>       }
>     } in join %7 : [ `0, `1 ]
>     let y : [ `0, `1 ] = %7;
>     feed y
>   }
>   1: {
>     let %15 : {} = @get_union_struct x;
>     let %16 : int = @get_tag_id x;
>     switch %16 {
>       0: {
>         let %18 : {} = @get_union_struct x;
>         let %19 : {} = @build_struct;
>         let %20 : [ `0, `1 ] = @build_tag 0 %19;
>         feed %20
>       }
>       1: {
>         let %21 : {} = @get_union_struct x;
>         let %22 : {} = @build_struct;
>         let %23 : [ `0, `1 ] = @build_tag 1 %22;
>         feed %23
>       }
>     } in join %17 : [ `0, `1 ]
>     let y : [ `0, `1 ] = %17;
>     feed y
>   }
>   2: {
>     let %25 : {} = @get_union_struct x;
>     let %26 : {} = @build_struct;
>     let %27 : [ `0, `1 ] = @build_tag 0 %26;
>     feed %27
>   }
> } in join %4 : [ `0, `1 ]
> let z : [ `0, `1 ] = %4;
> ret z

> cor-out +eval -print
> B