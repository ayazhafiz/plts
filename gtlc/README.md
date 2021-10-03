# gtlc

The gradually typed lambda calculus of [Siek and Taha](http://www.schemeworkshop.org/2006/13-siek.pdf).

Flow:
- [lex](./lexer.mll)/[parse](./parser.mly)
- [typecheck](./typecheck.ml)
- [cast insertion](./cast_ir.ml)
- backends
  - [interpretation](./eval.ml)
  - code generation
    - [IR lowering/optimization](./lift_ir.ml)
        - [C codegen](./c.ml)
        - [TypeScript codegen](./typescript.ml)
