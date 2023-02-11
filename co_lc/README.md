# co_lc

A lambda calculus with stackful coroutines and defunctionalization, compiled to
a bytecode stack machine.

The language is statically typed with complete inference; however, type
annotations cannot be supplied by the programmer.

## Language example

```
let fib = \n ->
  let {} = yield in
  if n < 2
  then n
  else (fib (n - 1)) + (fib (n - 2))

let exec = \state ->
  stat state.0
  | `Pending ->
    let fib1 = resume runFib in
    exec {fib1, 0, state.2 + 1}
  | `Done n -> {state.0, n, state.2}

in
let runFib = spawn (fib 28) in
exec {runFib, 0, 0}
```

## Bytecode

Stack on call:

```
arg2
arg1
----- do call
fp
sp
----- new fn, fp=here
local1
local2


DO ret
pop until fp=here
pop fp=old_fp
pop pc=old_pc 
pop args
```

Coroutine repr:

```
{bit, return_value, stkidx, stkdirty}

bit - 0=Pending, 1=Done
stkidx is the stack this coroutine is running on
stkdirty is a dirty integer marking the last yield point of the stack. this way
  we can check if someone is resuming a coroutine that has already completed.
  presently coroutines are not multi-shot.

Multi-shot coroutines: need to convert to CPS I think?

Size: 3 cells + return value cell size
```

Yield:

```
- Save pc, fp of stack as-is
- Push 0s and `Pending` onto parent stack
- Yield control back to parent stack
```

Spawn:

```
- Create new stack
- Set pc, args to new target function on new stack
- Run until hit yield or return over the top of the stack.
  - If yield, see Yield above.
  - If return, pop the stack, push the return value and `Done` onto the parent stack. Yield control to the parent stack.
```

Resume:

```
- If coroutine is done, no-op 
- If coroutine is pending
  - Find stack
  - If stack missing or dirty integer != stkdirty, error
  - Switch VM state and run until hit yield
```

Stat:

```
Bit 0 is `Done, 1 is `Pending
In `Done branch, read out return value
```

Translation of example:

```
+:
  n <- #pop
  m <- #pop
  #push (m + n)

-:
  n <- #pop
  m <- #pop
  #push (m - n)

*:
  n <- #pop
  m <- #pop
  #push (m * n)

0<:
  n <- #pop
  #push 0 < n

0=:
  n <- #pop
  #push n == 0
  
fib:
  sp-add 4    # push stack pointer forward for locals
  yield
  push fp[-3] # load n
  0<
  store-into fp[1] # | redundant
  push fp[1]       # |
  jmpz fib-b2
fib-b1:
  push fp[-3] # push n
  store-into fp[0] # store into res
  jmp fib-fin
fib-b2:
  push fp[-3] # push n
  push 1
  -
  call fib 1
  store-into fp[2] # store fib (n-1)
  push fp[-3] # push n
  push 2
  -
  call fib 1
  store-into fp[3] # store fib (n-2)
  push fp[2]
  push fp[3]
  store-into fp[0] # store into res
  jmp fib-fin
fib-fin:
  ret 1            # ret will store N bytes at head of fp as return value (here 1), reset to fp, restore fp/pc, pop args, and then store ret on head of stack

exec:
  # stack
  #   state.2           -8
  #   state.1           -7
  #   state.0.stkdirty  -6
  #   state.0.stkidx    -5
  #   state.0.return    -4
  #   state.0.bit       -3
  #   fp old
  #   sp old
  #   LOCALS
  # locals
  #  ret - 4 (coroutine) + 1 + 1 = 6 bytes
  #    .2            0
  #    .1            1
  #    .0.stkdirty   2
  #    .0.stkidx     3
  #    .0.return     4
  #    .0.bit        5
  #  fib1 - 4 bytes
  #    .stkdirty     6
  #    .stkidx       7
  #    .return       8
  #    .bit          9
  #  state.2 + 1 - 1 byte
  #    state.2       10
  sp-add 11
  # stat looks at .bit, .return
  push fp[-3]
  0=  # bit = 0 : `Pending
  jmpz exec-pending
exec-done:
  # n is seen as state.0.return
  push fp[-8]
  store-into fp[0] # ret.2 = state.2
  push fp[-4]
  store-into fp[0] # ret.1 = state.0.return
  push fp[-6..-3]
  store-into fp[5..2] # ret.0 = state.0
exec-pending:
  push fp[-6..-3]
  resume 1
  store-into fp[9]
  store-into fp[8]
  store-into fp[7]
  store-into fp[6] # stored fib1
  push fp[-8]
  push 1
  +
  store-into fp[10]
  push fp[10]   # arg.2 = state.2 + 1
  push 0        # arg.1 = 0
  push fp[6..9] # arg.0 = fib1
  call exec 6
  store-into fp[5..0] # store result
exec-fin:
  ret 4   # return value is 4 cells

main:
  # locals
  #  ret - 6 bytes
  #    .2            0
  #    .1            1
  #    .0.stkdirty   2
  #    .0.stkidx     3
  #    .0.return     4
  #    .0.bit        5
  #  runFib - 4 bytes
  #    .stkdirty     6
  #    .stkidx       7
  #    .return       8
  #    .bit          9
  sp-add 10
  push 28
  push fib
  spawn 1
  store-into fp[9..6]
  push 0
  push 0
  push fp[9..6]
  call exec 6
  store-into fp[5..0]
  ret 6
```
