# Compiling effects via capabilities

An exploratory language with effect handlers, compiled via capabilities as in
*Compiling Effect Handlers in Capability-Passing Style (Schuster, et.al. 2020)*.

The idea is to
- type effects in a scope explicitly
- pass effect handler implementations into the scopes they are used
- re-write effectful scopes via CPS, supporting effect continuations as... well,
    continuations

## Deep vs shallow handlers

TODO

## Specializing tail-callable handlers

TODO

## Eliminating redundant intermediate closures and memory leaks

TODO
