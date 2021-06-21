# DNF+

This is an implementation of the subtype-checking algorithm described in David Pearce's _[Sound and Complete Flow Typing with Unions, Intersections, and Negations](https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf)_,
so-called DNF+ because of Pearce's construction of a "DNF+" canonical form
underlying the subtyping algorithm.

The type system consists of the following syntactic types:

```
Type ::= any
       | never
       | int
       | (T_1, ..., T_n)
       | !T
       | T_1 & ... & T_n
       | T_1 | ... | T_n
```

All but `never` are typable by the user. `any` and `never` are the top and bottom
types, and `!T` is the negation type of `T`.

The problem of subtyping in such a type system boils down to constructing a
canonical, sound, and complete normal form. For example, we would like to say
that

```
any <: int & !int
```

and

```
(int | (int, int), int) <: (int, int) | ((int, int), int)
```

hold (Pearce, 7), but indeed it is not so clear how to normalize these types in
a way to make these subtyping comparisons trivial.

Pearce's contribution is as follows. First, we introduce a notion of "type atoms":

```
Atom ::= T+
       | T-
       | T*

T+ ::= any | int | (T_1+, ..., T_n+)
T- ::= !T+
T* ::= T+ | T-
```

In 3.1, Pearce proves critical theorems on soundness and completeness, and
indivisibility of atoms.

Next, Pearce presents the construction of a Disjunctive Normal Form on a type T,
denoted `DNF(T)`. 3.2 presents an algorithm presents an algorithm for
strongly-normalizing a `Type` T to a `DNF(T)` consisting of atoms in the form
`DNF(T) ~ |_i &_j T*_{i, j}`, which is to say `DNF(T)` consists of unions on the
toplevel, intersections inside, and there are no nested negations (due to the
definition of `T*`).

Next, Pearce introduces the idea of a Canonical Conjuct (4.2), which canonicalizes
an intersection `T_1 & ... & T_n` to the form

```
T+_1 & !T+_2 & ... & !T+_n
```

with the additional properties that

1. Forall `!T+_k`, `T+_1 \ne T+_k` and `T+_k <: T+_1`, which is to say that
   `!T+_k` takes away from the entire type strictly less than `T+_1` introduces.

2. For any two `!T+_k`, `!T+_m`, `T+_k` and `T+_m` have no subtyping relations,
   preventing any two such negations from subsuming each either, thus avoiding
   "redundant subtraction" of the total type.

Definition 8 (4.3) gives a rewriting procedure `CAN` for producing a Canonical
Conjuct for an intersection type, or the bottom type `never`.

This sets up all that is needed to define the Canonical Disjunctive Normal Form
DNF+ (4.4). Given a user type `T` and `DNF(T) ~ |_i &_j T*_{i, j}`, we say that
`DNF+(T) = |_i CAN(&_j T*_{i, j})`. The subtyping relation now boils down to

```
DNF+(T_1 & !T_2) = never
------------------------
       T_1 <: T_2
```

which of course follows from the (set|category|homotopy)-theoretical presumption
that

```
    T_1 <: T_2
<=> [T_1] <= [T_2]
<=> [T_1] \ [T_2] = !
<=> T_1 & !T_2 = never
```

Pearce gives a simple example in 4.5:

```
any <: int | !int

  DNF+(any & !(int | !int))
= CAN(DNF(any & !(int | !int)))
= CAN(any & !int & int)
= never
```

## Notes

- The subtyping algorithm is clearly not trivial in time complexity, for a
  number of reasons. Firstly, as noted in Definition 6 (3.2), the construction
  of `DNF(T)` may be exponential in time complexity. Secondly, the algorithms
  `DNF` and `CAN` are both order-insensitive, which means application of
  rewrite rules takes quadratic time (without implementation tricks).

- In fact, the exponential growth of `DNF(T)` can be tremendous. I have been
  unable to terminate the normalization of
  ```
  (((int, int) & !int, (int, int) & !int), ((int, int) & !int, (int, int) & !int))`
  ```

## Extensions

- Pearce calls the bottom type `void`; I call it `never`.

- This implementation flow-types function calls. This means when checking a
    call, we inline the type of arguments into the body of the function and then
    check the body, rather than using a constant return type for all argument
    subtypes.

- This implementation also includes a type inferer that is guaranteed to infer
    the minimal legal type for a variable, if any exists. See
    [infer.ml](./infer.ml). A proof of this property is upcoming.
