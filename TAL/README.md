# TAL

A compiler from a System F-like language to the Typed Assembly Language, as in
[Morrisett, Walker, Crary, and Glew (1998)](https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf).
Also includes a code generator to x86 assembly.

TODO

- [x] Frontend
- [x] CPS Conversion
- [X] Closure Conversion
  - [X] Hoisting
- [X] Allocation
- [X] Cgen System A to TAL
- [X] TAL emulation
- [X] Cgen TAL to x86
- [X] Optimizations
  - [X] CPS: Danvy and Filinski [1992]
