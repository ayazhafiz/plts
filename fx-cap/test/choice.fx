let choice[flip : Flip, fail : Fail] = \n ->
  if @lt n 1 then
    do (fail true)
  else if (do (flip true)) then
    return n
  else choice (@sub n 1)
in 
let handledChoice = \n -> 
  handle flip = Flip x k -> @mul (do (k True)) (do (k False)) in
  handle fail = Fail x k -> 0 in
  choice[lift flip, fail] n
in
handledChoice 5
