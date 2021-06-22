# Skittles

This repository contains implementations of type systems and
programming languages I find interesting.

[Online Playgrounds](https://ayazhafiz.com/skittles)

- [tapl](./tapl/) - Selected implementations of languages formalized in _Types and
  Programming Languages_ (Pierce 2002).
- [deptypes](./deptypes) - A dependent type theory as described in Chapter 2 of
    Pierce's _Advanced Topics in Types and Programming Languages_.
- [lang_narrow](https://github.com/ayazhafiz/lang_narrow) - Self-designed language with flow typing, supporting unions and records.
- [simple_sub](./simple_sub/) - a type system that supports type inference in the presence of
  subtyping and polymorphism, as described by
  [Parreaux's _The Simple Essence of Algebraic Subtyping_ (2020)](https://lptk.github.io/files/%5Bv1.8%5D%20simple-essence-algebraic-subtyping.pdf).
  Parreaux's work distills [Dolan's 2017 thesis](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf)
  on Algebraic Subtyping into a simpler core.
- [FT](./ft/) - an implementation of David Pearce's _[Sound and Complete Flow Typing with Unions, Intersections, and Negations](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf)_. Like `lang_narrow`, but proven sound and complete. Also includes a self-designed type inferer guaranteed to infer minimal types.
