# cor +solve -elab
# cor +ir -print
# cor +eval -print
let x : [A [M, N, Q], B [Q, R], C [N, Q, L]] = C L in
let z = match x with
#   ^         ^
  | A M | B R | C Q | C L as y -> y
  | A Q -> G
  | A N -> H I
in z

> cor-out +solve -elab
> let x : [A [M, N, Q], B [Q, R], C [N, Q, L]] = C L in
> let z = match x with
> #             ^ [A [M, N, Q], B [Q, R], C [L, N, Q]]
> #   ^ [A [M], B [R], C [L, Q], G, H [I]]
>   | A M | B R | C Q | C L as y -> y
>   | A Q -> G
>   | A N -> H I
> in z
> 

> cor-out +ir -print
> let %1 : {} = @build_struct;
> let %2 : [ `0 {}, `1 {}, `2 {} ] = @build_union 0 %1;
> let %3 : { [ `0 {}, `1 {}, `2 {} ] } = @build_struct %2;
> let %4 : [ `0 { [ `0 {}, `1 {}, `2 {} ]}, `1 { [ `0 {}, `1 {} ]}, `2 { [ `0 {}, `1 {}, `2 {} ]} ] = @build_union 2 %3;
> let x : [ `0 { [ `0 {}, `1 {}, `2 {} ]}, `1 { [ `0 {}, `1 {} ]}, `2 { [ `0 {}, `1 {}, `2 {} ]} ] = %4;
> let %8 : int = @get_union_id x;
> switch %8 {
>   0: {
>     let %10 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>     let %5 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %10;
>     let %11 : int = @get_union_id %5;
>     switch %11 {
>       0: {
>         let %13 : {} = @get_union_struct %5;
>         let %17 : int = @get_union_id x;
>         switch %17 {
>           0: {
>             let %19 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %14 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %19;
>             let %22 : {} = @get_union_struct %14;
>             let %23 : {} = @build_struct;
>             let %24 : [ `0 {} ] = @build_union 0 %23;
>             let %25 : { [ `0 {} ] } = @build_struct %24;
>             let %26 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 0 %25;
>             feed %26
>           }
>           1: {
>             let %29 : { [ `0 {}, `1 {} ] } = @get_union_struct x;
>             let %15 : [ `0 {}, `1 {} ] = @get_field 0 %29;
>             let %33 : {} = @get_union_struct %15;
>             let %34 : {} = @build_struct;
>             let %35 : [ `0 {} ] = @build_union 0 %34;
>             let %36 : { [ `0 {} ] } = @build_struct %35;
>             let %37 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 1 %36;
>             feed %37
>           }
>           2: {
>             let %38 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %16 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %38;
>             let %39 : int = @get_union_id %16;
>             switch %39 {
>               0: {
>                 let %41 : {} = @get_union_struct %16;
>                 let %42 : {} = @build_struct;
>                 let %43 : [ `0 {}, `1 {} ] = @build_union 0 %42;
>                 let %44 : { [ `0 {}, `1 {} ] } = @build_struct %43;
>                 let %45 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %44;
>                 feed %45
>               }
>               2: {
>                 let %47 : {} = @get_union_struct %16;
>                 let %48 : {} = @build_struct;
>                 let %49 : [ `0 {}, `1 {} ] = @build_union 1 %48;
>                 let %50 : { [ `0 {}, `1 {} ] } = @build_struct %49;
>                 let %51 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %50;
>                 feed %51
>               }
>             } in join %40 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>             feed %40
>           }
>         } in join %18 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>         let y : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = %18;
>         feed y
>       }
>       1: {
>         let %52 : {} = @get_union_struct %5;
>         let %53 : {} = @build_struct;
>         let %54 : [ `0 {} ] = @build_union 0 %53;
>         let %55 : { [ `0 {} ] } = @build_struct %54;
>         let %56 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 4 %55;
>         feed %56
>       }
>       2: {
>         let %57 : {} = @get_union_struct %5;
>         let %58 : {} = @build_struct;
>         let %59 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 3 %58;
>         feed %59
>       }
>     } in join %12 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>     feed %12
>   }
>   1: {
>     let %60 : { [ `0 {}, `1 {} ] } = @get_union_struct x;
>     let %6 : [ `0 {}, `1 {} ] = @get_field 0 %60;
>     let %64 : {} = @get_union_struct %6;
>     let %68 : int = @get_union_id x;
>     switch %68 {
>       0: {
>         let %70 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>         let %65 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %70;
>         let %73 : {} = @get_union_struct %65;
>         let %74 : {} = @build_struct;
>         let %75 : [ `0 {} ] = @build_union 0 %74;
>         let %76 : { [ `0 {} ] } = @build_struct %75;
>         let %77 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 0 %76;
>         feed %77
>       }
>       1: {
>         let %80 : { [ `0 {}, `1 {} ] } = @get_union_struct x;
>         let %66 : [ `0 {}, `1 {} ] = @get_field 0 %80;
>         let %84 : {} = @get_union_struct %66;
>         let %85 : {} = @build_struct;
>         let %86 : [ `0 {} ] = @build_union 0 %85;
>         let %87 : { [ `0 {} ] } = @build_struct %86;
>         let %88 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 1 %87;
>         feed %88
>       }
>       2: {
>         let %89 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>         let %67 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %89;
>         let %90 : int = @get_union_id %67;
>         switch %90 {
>           0: {
>             let %92 : {} = @get_union_struct %67;
>             let %93 : {} = @build_struct;
>             let %94 : [ `0 {}, `1 {} ] = @build_union 0 %93;
>             let %95 : { [ `0 {}, `1 {} ] } = @build_struct %94;
>             let %96 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %95;
>             feed %96
>           }
>           2: {
>             let %98 : {} = @get_union_struct %67;
>             let %99 : {} = @build_struct;
>             let %100 : [ `0 {}, `1 {} ] = @build_union 1 %99;
>             let %101 : { [ `0 {}, `1 {} ] } = @build_struct %100;
>             let %102 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %101;
>             feed %102
>           }
>         } in join %91 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>         feed %91
>       }
>     } in join %69 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>     let y : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = %69;
>     feed y
>   }
>   2: {
>     let %103 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>     let %7 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %103;
>     let %104 : int = @get_union_id %7;
>     switch %104 {
>       0: {
>         let %106 : {} = @get_union_struct %7;
>         let %110 : int = @get_union_id x;
>         switch %110 {
>           0: {
>             let %112 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %107 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %112;
>             let %115 : {} = @get_union_struct %107;
>             let %116 : {} = @build_struct;
>             let %117 : [ `0 {} ] = @build_union 0 %116;
>             let %118 : { [ `0 {} ] } = @build_struct %117;
>             let %119 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 0 %118;
>             feed %119
>           }
>           1: {
>             let %122 : { [ `0 {}, `1 {} ] } = @get_union_struct x;
>             let %108 : [ `0 {}, `1 {} ] = @get_field 0 %122;
>             let %126 : {} = @get_union_struct %108;
>             let %127 : {} = @build_struct;
>             let %128 : [ `0 {} ] = @build_union 0 %127;
>             let %129 : { [ `0 {} ] } = @build_struct %128;
>             let %130 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 1 %129;
>             feed %130
>           }
>           2: {
>             let %131 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %109 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %131;
>             let %132 : int = @get_union_id %109;
>             switch %132 {
>               0: {
>                 let %134 : {} = @get_union_struct %109;
>                 let %135 : {} = @build_struct;
>                 let %136 : [ `0 {}, `1 {} ] = @build_union 0 %135;
>                 let %137 : { [ `0 {}, `1 {} ] } = @build_struct %136;
>                 let %138 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %137;
>                 feed %138
>               }
>               2: {
>                 let %140 : {} = @get_union_struct %109;
>                 let %141 : {} = @build_struct;
>                 let %142 : [ `0 {}, `1 {} ] = @build_union 1 %141;
>                 let %143 : { [ `0 {}, `1 {} ] } = @build_struct %142;
>                 let %144 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %143;
>                 feed %144
>               }
>             } in join %133 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>             feed %133
>           }
>         } in join %111 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>         let y : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = %111;
>         feed y
>       }
>       2: {
>         let %146 : {} = @get_union_struct %7;
>         let %150 : int = @get_union_id x;
>         switch %150 {
>           0: {
>             let %152 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %147 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %152;
>             let %155 : {} = @get_union_struct %147;
>             let %156 : {} = @build_struct;
>             let %157 : [ `0 {} ] = @build_union 0 %156;
>             let %158 : { [ `0 {} ] } = @build_struct %157;
>             let %159 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 0 %158;
>             feed %159
>           }
>           1: {
>             let %162 : { [ `0 {}, `1 {} ] } = @get_union_struct x;
>             let %148 : [ `0 {}, `1 {} ] = @get_field 0 %162;
>             let %166 : {} = @get_union_struct %148;
>             let %167 : {} = @build_struct;
>             let %168 : [ `0 {} ] = @build_union 0 %167;
>             let %169 : { [ `0 {} ] } = @build_struct %168;
>             let %170 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 1 %169;
>             feed %170
>           }
>           2: {
>             let %171 : { [ `0 {}, `1 {}, `2 {} ] } = @get_union_struct x;
>             let %149 : [ `0 {}, `1 {}, `2 {} ] = @get_field 0 %171;
>             let %172 : int = @get_union_id %149;
>             switch %172 {
>               0: {
>                 let %174 : {} = @get_union_struct %149;
>                 let %175 : {} = @build_struct;
>                 let %176 : [ `0 {}, `1 {} ] = @build_union 0 %175;
>                 let %177 : { [ `0 {}, `1 {} ] } = @build_struct %176;
>                 let %178 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %177;
>                 feed %178
>               }
>               2: {
>                 let %180 : {} = @get_union_struct %149;
>                 let %181 : {} = @build_struct;
>                 let %182 : [ `0 {}, `1 {} ] = @build_union 1 %181;
>                 let %183 : { [ `0 {}, `1 {} ] } = @build_struct %182;
>                 let %184 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = @build_union 2 %183;
>                 feed %184
>               }
>             } in join %173 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>             feed %173
>           }
>         } in join %151 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>         let y : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = %151;
>         feed y
>       }
>     } in join %105 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
>     feed %105
>   }
> } in join %9 : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ]
> let z : [ `0 { [ `0 {} ]}, `1 { [ `0 {} ]}, `2 { [ `0 {}, `1 {} ]}, `3 {}, `4 { [ `0 {} ]} ] = %9;
> ret z

> cor-out +eval -print
> C L