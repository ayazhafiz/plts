let n = read_int ()

(* Original translated source *)

let choice flip fail =
  let rec loop n k1 k2 =
    if n < 1 then fail () k1 k2
    else flip () (fun x k3 -> if x then k1 n k3 else loop (n - 1) k1 k3) k2
  in
  loop

let handled_choice n =
  let flip () k = k true @ k false in
  let fail () k1 k2 = k2 [] in
  let lifted_flip () k1 k2 = flip () (fun x -> k1 x k2) in
  choice lifted_flip fail n (fun x1 k2 -> k2 [ x1 ]) (fun x2 -> x2)

let _ =
  let r = handled_choice n in
  List.iter (Printf.printf "%d ") r;
  print_endline ""

(* Source with three continuations *)

let choice flip fail const =
  let rec loop n k1 k2 k3 =
    if n < 1 then fail () k1 k2 k3
    else if n == 6 then const () k1 k2 k3
    else
      flip ()
        (fun x k4 k5 -> if x then k1 n k4 k5 else loop (n - 1) k1 k4 k5)
        k2 k3
  in
  loop

let handled_choice n =
  let flip () k = k true @ k false in
  let fail () k1 k2 = k2 [] in
  let const () k1 k2 = k2 [ 141; 252; 363 ] in
  let lifted_flip x k k' =
    (fun x k k' -> flip x (fun y -> k y k')) x (fun y -> k y k')
  in
  let lifted_fail x k k' = fail x (fun y -> k y k') in
  choice lifted_flip lifted_fail const n
    (* lift all non-terminal continuations by exactly one lambda *)
      (fun x1 k -> k [ x1 ])
    (fun x2 k -> k x2)
    (fun x3 -> x3)

let _ =
  let r = handled_choice n in
  List.iter (Printf.printf "%d ") r
