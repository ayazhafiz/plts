# HO21

This is an implementation of the algorithmic subtyping system presented in
_[Distributing Intersection and Union Types with Splits and Duality (Functional Pearl)](https://dl.acm.org/doi/pdf/10.1145/3473594)_ (Huang and Oliveira, 2021).

The system includes intersection, union, and function types with non-trivial
distributivity rules like

```
(A -> B) & (A -> C) <: A -> (B & C)
```

and is able to make subtyping judgements directly on surface types, without
additional pre-processing or normalization to a form like DNF. The subtyping
checking algorithm is "moded" to flip between subtype and supertype checking, in
accordance with the duality between intersection and union types. The algorithm
employs minimal backtracking, as discussed in the paper.

Huang and Oliveira provide an implementation in their paper; this implementation
is just for my own understanding and exposure on a playground.
