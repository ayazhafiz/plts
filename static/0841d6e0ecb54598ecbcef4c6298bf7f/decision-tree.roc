# cor +solve -elab
# cor +mono -print
# cor +eval -print
let x = Add Zero (Mul N1 N2) Reals in
#   ^
let result = when x is
#                 ^
  | Add (Add _ _) Zero Reals -> B1
  | Add (Mul _ _) Zero Reals -> B2
  | Add _ (Mul _ _)    Reals -> B3
  | Add _ (Add _ _)    Reals -> B4
  | Add _ _            Reals -> B5
  | Sub _ _            Reals -> B6
in result
#  ^^^^^^

> cor-out +solve -elab
> let x = Add Zero (Mul N1 N2) Reals in
> #   ^ [Add [Add '8 '7, Mul '16 '15, Zero] [Add '31 '30, Mul [N1] [N2], Zero] 
> #   ^   [Reals], Sub '45 '44 [Reals]]
> let result = when x is
> #                 ^ [Add [Add '8 '7, Mul '16 '15, Zero] [Add '31 '30, Mul [N1] [N2], Zero] 
> #                 ^   [Reals], Sub '45 '44 [Reals]]
>   | Add (Add _ _) Zero Reals -> B1
>   | Add (Mul _ _) Zero Reals -> B2
>   | Add _ (Mul _ _)    Reals -> B3
>   | Add _ (Add _ _)    Reals -> B4
>   | Add _ _            Reals -> B5
>   | Sub _ _            Reals -> B6
> in result
> #  ^^^^^^ [B1, B2, B3, B4, B5, B6]
> 

> cor-out +mono -print
> let %1 : {} = @build_struct;
> let %2 : [ `0 void void, `1 void void, `2 ] = @build_tag 2 %1;
> let %3 : {} = @build_struct;
> let %4 : [ `0 ] = @build_tag 0 %3;
> let %5 : {} = @build_struct;
> let %6 : [ `0 ] = @build_tag 0 %5;
> let %7 : { [ `0 ], [ `0 ] } = @build_struct %4 %6;
> let %8 : [ `0 void void, `1 [ `0 ] [ `0 ], `2 ] = @build_tag 1 %7;
> let %9 : {} = @build_struct;
> let %10 : [ `0 ] = @build_tag 0 %9;
> let %11 : { [ `0 void void, `1 void void, `2 ], [ `0 void void, `1 [ `0 ] [ `0 ], `2 ], [ `0 ] } = @build_struct %2 %8 %10;
> let %12 : [ `0 [ `0 void void, `1 void void, `2 ] [ `0 void void, `1 [ `0 ] [ `0 ], `2 ] [ `0 ], `1 void void [ `0 ] ] = @build_tag 0 %11;
> let x : [ `0 [ `0 void void, `1 void void, `2 ] [ `0 void void, `1 [ `0 ] [ `0 ], `2 ] [ `0 ], `1 void void [ `0 ] ] = %12;
> let %27 : int = @get_tag_id x;
> switch %27 {
>   0: {
>     let %29 : { [ `0 void void, `1 void void, `2 ], [ `0 void void, `1 [ `0 ] [ `0 ], `2 ], [ `0 ] } = @get_union_struct x;
>     let %13 : [ `0 void void, `1 void void, `2 ] = @get_field 0 %29;
>     let %14 : [ `0 void void, `1 [ `0 ] [ `0 ], `2 ] = @get_field 1 %29;
>     let %15 : [ `0 ] = @get_field 2 %29;
>     let %32 : {} = @get_union_struct %15;
>     let %33 : int = @get_tag_id %14;
>     switch %33 {
>       0: {
>         let %35 : { void, void } = @get_union_struct %14;
>         let %19 : void = @get_field 0 %35;
>         let %20 : void = @get_field 1 %35;
>         let %36 : {} = @build_struct;
>         let %37 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 3 %36;
>         feed %37
>       }
>       1: {
>         let %38 : { [ `0 ], [ `0 ] } = @get_union_struct %14;
>         let %21 : [ `0 ] = @get_field 0 %38;
>         let %22 : [ `0 ] = @get_field 1 %38;
>         let %39 : {} = @build_struct;
>         let %40 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 2 %39;
>         feed %40
>       }
>       2: {
>         let %41 : {} = @get_union_struct %14;
>         let %42 : int = @get_tag_id %13;
>         switch %42 {
>           0: {
>             let %44 : { void, void } = @get_union_struct %13;
>             let %23 : void = @get_field 0 %44;
>             let %24 : void = @get_field 1 %44;
>             let %45 : {} = @build_struct;
>             let %46 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 0 %45;
>             feed %46
>           }
>           1: {
>             let %47 : { void, void } = @get_union_struct %13;
>             let %25 : void = @get_field 0 %47;
>             let %26 : void = @get_field 1 %47;
>             let %48 : {} = @build_struct;
>             let %49 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 1 %48;
>             feed %49
>           }
>           2: {
>             let %50 : {} = @get_union_struct %13;
>             let %51 : {} = @build_struct;
>             let %52 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 4 %51;
>             feed %52
>           }
>         } in join %43 : [ `0, `1, `2, `3, `4, `5 ]
>         feed %43
>       }
>     } in join %34 : [ `0, `1, `2, `3, `4, `5 ]
>     feed %34
>   }
>   1: {
>     let %53 : { void, void, [ `0 ] } = @get_union_struct x;
>     let %16 : void = @get_field 0 %53;
>     let %17 : void = @get_field 1 %53;
>     let %18 : [ `0 ] = @get_field 2 %53;
>     let %56 : {} = @get_union_struct %18;
>     let %57 : {} = @build_struct;
>     let %58 : [ `0, `1, `2, `3, `4, `5 ] = @build_tag 5 %57;
>     feed %58
>   }
> } in join %28 : [ `0, `1, `2, `3, `4, `5 ]
> let result : [ `0, `1, `2, `3, `4, `5 ] = %28;
> ret result

> cor-out +eval -print
> B3 {}