let choice[flip : Flip, fail : Fail] = \n ->
  if @lt n 1 then
    do fail()
  else if (do flip()) then
    return n
  else choice (@sub n 1)
in 
let handledChoice = \n -> 
  handle flip = Flip((), k) => @mul (do (k True)) (do (k False)) in
  handle fail = Fail((), k) => 0 in
  choice[lift flip, fail] n
in
handledChoice 5
