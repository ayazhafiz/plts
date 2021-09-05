Repository: [gh:ayazhafiz/plts](https://github.com/ayazhafiz/plts).

## Lambda Calculus

- [Emulating the Lambda Calculus in TypeScript's Type System](https://ayazhafiz.com/articles/21/typescript-type-system-lambda-calculus): Evaluating the lambda calculus entirely using the TypeScript type system.

## Flow Typing

- [lang_narrow](https://ayazhafiz.com/lang_narrow): A language with unions,
records, and flow typing. A checker, interpreter, and C code generator is
implemented.
  - [Playground](https://ayazhafiz.com/lang_narrow)
  - [Blog post](https://ayazhafiz.com/articles/21/lang-narrow)


- [FT](./ft/www/index.html): The FT (flow typing) calculus from David
Pearce's 2012 paper [_Sound and Complete Flow Typing with Unions,
Intersections, and Negations_](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf).
Like `lang_narrow`, but smaller and proven sound and complete. A checker as
described by Pearce and a self-designed inferer is implemented.
  - [Playground](./ft/www/index.html)
  - [Pearce, 2012](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf)
  - [Blog post](https://ayazhafiz.com/articles/21/type-inference-for-flow-typing): type inference for the calculus

## Typed Assembly

- [TAL](./TAL/www/index.html): A compiler from a System F-like language to the
    Typed Assembly Language of [Morrisett, et.al. 1998](https://dash.harvard.edu/handle/1/2797451).
    Also includes a compiler to x86 assembly using [Linear Scan Register
    Allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)
    (Poletto & Sarkar 1999).
  - [Playground](./TAL/www/index.html)

## Subtyping

- [HO21](./huang_oliveira_21/www/index.html): An implementation of the
    algorithmic duotyping calculus invented by Huang and Oliveira in
    [Distributing Intersection and Union Types with Splits and Duality](https://dl.acm.org/doi/pdf/10.1145/3473594) (2021).
    The calculus includes union, intersection, and arrow types in the presence
    of non-trivial distributivity rules. The authors' duotyping algorithm is
    somewhat novel in that it computes subtyping relationship entirely on
    surface types of the language, without normalizing to a form like DNF.
    This implementation includes a type-derivation tree generator.
    - [Playground](./huang_oliveira_21/www/index.html)
    - [Huang and Oliveira, 2021](https://dl.acm.org/doi/pdf/10.1145/3473594)
