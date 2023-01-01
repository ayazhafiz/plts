let choice[flip : Flip, fail : Fail] = \n ->
  if n < 1
    do fail()
  else if (do flip())
    return n
  else choice(n − 1)
in 
let handledChoice = \n -> 
  handle flip = Flip((), k) => append (do (k True)) (do (k False)) in
  handle fail = Fail((), k) => Nil in
  Cons(choice[lift flip, fail] n, Nil)
in
handledChoice 1
