# gtlc

The gradually typed lambda calculus of [Siek and Taha](http://www.schemeworkshop.org/2006/13-siek.pdf).

Flow:
- [lex](./lexer.mll)/[parse](./parser.mly)
- [type inference](./infer.ml)
- [typecheck](./typecheck.ml)
- [cast insertion](./cast_ir.ml)
- backends
  - [interpretation](./eval.ml)
  - code generation
    - [IR lowering/optimization](./lift_ir.ml)
        - [C codegen](./c.ml)
        - [TypeScript codegen](./typescript.ml)

## References

References (mutable values) are implemented as in Siek and Taha's paper. However,
there is no prior art (that I know of) relating gradually-typed references with
type inference. The implemented type reference follows Siek and Vachharajani, 2008.
We perform a modification of their algorithm to support references, namely to
constrain type variables inside of a reference via strictly equality rather than the
consistency relation. This is necessary because references must be invariant
(convince yourself that this is true). The only exception is when we have a
contraint like <code>? ~ ref \`t1</code>, in which case we generate the child
consistency-relation constraint <code>? ~ `t1</code> rather than forcing strict
equality of <code>`t1</code> with `?`, since we may find a more-informative type
for <code>`t1</code> later on.
