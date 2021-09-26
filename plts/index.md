# plts

Implementations of type systems and programming languages I find interesting.

Repository: [gh:ayazhafiz/plts](https://github.com/ayazhafiz/plts).

## Introductory

- [Emulating the Lambda Calculus in TypeScript's Type System](https://ayazhafiz.com/articles/21/typescript-type-system-lambda-calculus): Evaluating the lambda calculus entirely using the TypeScript type system.

- [TAPL](https://github.com/ayazhafiz/plts/blob/base/tapl): Selected
  implementations of languages formalized in _Types and Programming Languages_
  (Pierce 2002).

## Flow Typing

- [lang_narrow](https://ayazhafiz.com/lang_narrow): A language with unions,
  records, and flow typing. A checker, interpreter, and C code generator is
  implemented.

  - [Playground](https://ayazhafiz.com/lang_narrow)
  - [Blog post](https://ayazhafiz.com/articles/21/lang-narrow)

- [FT][ft-pg]: The FT (flow typing) calculus from David
  Pearce's 2012 paper [_Sound and Complete Flow Typing with Unions,
  Intersections, and Negations_](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf).
  Like `lang_narrow`, but smaller and proven sound and complete. Includes a
  self-designed type inferer guaranteed to infer principal types.
  - [Playground][ft-pg]
  - [Pearce, 2012](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf)
  - [Blog post](https://ayazhafiz.com/articles/21/type-inference-for-flow-typing): type inference for the calculus

## Gradual Typing

- [gtlc][gtlc-pg]: A compiler for the gradually-typed lambda calculus,
  employing the type consistency relation of [Siek and Taha](http://www.schemeworkshop.org/2006/13-siek.pdf) (2006).
  The GTLC allows a developer to omit type annotations during development at
  the expense of run-time type casts. While the ahead-of-time typechecker will
  catch any non-sensical type errors, the runtime system will catch any cast
  errors.

  The compiler is multi-phase, optimizing, includes an interpretive mode, and
  provides code generators to C and TypeScript (a type inferer is upcoming).

  - [Playground][gtlc-pg]
  - [Siek and Taha, 2006](http://www.schemeworkshop.org/2006/13-siek.pdf)

## Typed Assembly

- [TAL][tal-pg]: A compiler from a System F-like language to the
  Typed Assembly Language of [Morrisett, et.al. 1998](https://dash.harvard.edu/handle/1/2797451).
  Also includes a compiler to x86 assembly using [Linear Scan Register
  Allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)
  (Poletto & Sarkar 1999).
  - [Playground][tal-pg]

## Subtyping

- [HO21][ho21-pg]: An implementation of the
  algorithmic duotyping calculus invented by Huang and Oliveira in
  [Distributing Intersection and Union Types with Splits and Duality](https://dl.acm.org/doi/pdf/10.1145/3473594) (2021).
  The calculus includes union, intersection, and arrow types in the presence
  of non-trivial distributivity rules. The authors' duotyping algorithm is
  somewhat novel in that it computes subtyping relationship entirely on
  surface types of the language, without normalizing to a form like DNF.
  This implementation includes a type-derivation tree generator.

  - [Playground][ho21-pg]
  - [Huang and Oliveira, 2021](https://dl.acm.org/doi/pdf/10.1145/3473594)

- [simple_sub](https://github.com/ayazhafiz/plts/blob/base/simple_sub): A type system
  that supports type inference in the presence of subtyping and polymorphism, as
  described by Parreaux's _[The Simple Essence of Algebraic Subtyping](https://lptk.github.io/files/%5Bv1.8%5D%20simple-essence-algebraic-subtyping.pdf)_ (2020). Parreaux's
  work distills Dolan's [2017 thesis](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf)
  on Algebraic Subtyping into a simpler core.

## Dependent Types

- [deptypes](https://github.com/ayazhafiz/plts/blob/base/deptypes): A dependent
  type theory as described in Chapter 2 of Pierce's _Advanced Topics in Types
  and Programming Languages_.

- [more deptypes](https://github.com/ayazhafiz/plts/pull/3): Additional,
  alternate implementations of the basic dependently-typed lambda calculus.

[ft-pg]: https://ayazhafiz.com/plts/playground/ft
[gtlc-pg]: https://ayazhafiz.com/plts/playground/gtlc
[tal-pg]: https://ayazhafiz.com/plts/playground/tal
[ho21-pg]: https://ayazhafiz.com/plts/playground/ho21
