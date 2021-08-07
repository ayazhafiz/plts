open Tal

type testcase = {
  name : string;
  input : string;
  pretty_f : string;
  typecheck_f : string;
  pretty_k : string;
}

let rm1 s = String.sub s 1 (String.length s - 1)

let cases =
  [
    {
      name = "fib6";
      input =
        rm1 {|
(fix f (n: int): int.
  if0 n then 1 else n * f (n - 1)) 6|};
      pretty_f =
        rm1
          {|
((fix f(n: int): int.
   (if0 n then 1 else
     (n * (f (n - 1))))) 6)|};
      typecheck_f = "int";
      pretty_k =
        rm1
          {|
((λ(x1: ((int, ((int) -> void)) -> void)).
   ((λ(x2: int). (x1(x2, (fix
        tl_halt(x: int). halt<int>x))))(6)))(
  (fix
        f(n: int, c: ((int) -> void)).
    ((λ(x3: int).
       (if0 x3 then (c(1)) else
         ((λ(x11: int).
            ((λ(x12: ((int, ((int) -> void)) -> void)).
               ((λ(x13: int).
                  ((λ(x23: int).
                     (let y1 = x13 - x23 in
                       ((λ(x22: int).
                          (x12(x22,
                            (λ(x21: int). (let y = x11 * x21 in (c(y)))))))(y1))))(1)))(n)))(f)))(n))))(n)))))|};
    };
    {
      name = "twice";
      input =
        rm1
          {|
Λa. fix in_f(f: a->a): a->a.
      fix in_x(x: a): a.
        (f (f x))|};
      pretty_f =
        rm1
          {|
Λa.
  (fix in_f(f: (a -> a)): (a -> a).
    (fix in_x(x: a): a. (f (f x))))|};
      typecheck_f = "∀a.((a -> a) -> (a -> a))";
      pretty_k =
        rm1
          {|
((fix
        tl_halt(x1:
                        (∀<a>.
                          (((((((a, ((a) -> void)) -> void),
                               ((((a, ((a) -> void)) -> void)) -> void))
                               -> void)) -> void)) -> void)).
   halt<
     (∀<a>.
       (((((((a, ((a) -> void)) -> void),
            ((((a, ((a) -> void)) -> void)) -> void)) -> void)) -> void))
       -> void)>x)((λ<a>.
                     (c:
                        ((((((a, ((a) -> void)) -> void),
                            ((((a, ((a) -> void)) -> void)) -> void)) -> void))
                          -> void)).
                     (c((fix
        in_f(f: ((a, ((a) -> void)) -> void),
                          c1: ((((a, ((a) -> void)) -> void)) -> void)).
                          (c((fix
        in_x(x: a, c2: ((a) -> void)).
                               ((λ(x11: ((a, ((a) -> void)) -> void)).
                                  ((λ(x12: ((a, ((a) -> void)) -> void)).
                                     ((λ(x21: a).
                                        (x12(x21, (λ(x2: a). (x11(x2, c))))))(x)))(f)))(f)))))))))))|};
    };
  ]

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ())

let mk_test driver (name, input, expect) =
  let test () =
    let out = parse_term input |> driver in
    Alcotest.(check string) input expect out
  in
  (name, `Quick, wrap test)

let f_pp_tests =
  let cases =
    List.map (fun { name; input; pretty_f; _ } -> (name, input, pretty_f)) cases
  in
  List.map (mk_test F.string_of_term) cases

let f_typecheck_tests =
  let cases =
    List.map
      (fun { name; input; typecheck_f; _ } -> (name, input, typecheck_f))
      cases
  in
  List.map (mk_test (fun t -> F.typeof t |> F.string_of_ty)) cases

let f_to_k_tests =
  let cases =
    List.map (fun { name; input; pretty_k; _ } -> (name, input, pretty_k)) cases
  in
  List.map (mk_test (fun t -> K.of_F t |> K.string_of_term)) cases

let k_typecheck_tests =
  let cases = List.map (fun { name; input; _ } -> (name, input, "")) cases in
  List.map
    (mk_test (fun t ->
         K.of_F t |> K.check_well_typed;
         ""))
    cases

let () =
  Alcotest.run "TAL tests"
    [
      ("[F] Pretty Printing", f_pp_tests);
      ("[F] Typecheck", f_typecheck_tests);
      ("[F->K] Closure Conversion", f_to_k_tests);
      ("[K] Typecheck", f_to_k_tests);
    ]
