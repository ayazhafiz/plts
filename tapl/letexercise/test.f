/* Examples for testing */

a: Bool;
lambda x:Bool. x;
let x = true in let y = x in let y = y in y;
(lambda x:Bool->Bool. let y = x in let z = y in if y false then true else false) 
  (lambda x:Bool. if x then false else true); 
