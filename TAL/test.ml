open Tal

type testcase = {
  name : string;
  input : string;
  output : string option;
  pretty_f : string;
  typecheck_f : string;
  pretty_k : string;
  pretty_c : string option;
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
      output = Some "720";
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
((fix f(n: int, c: ((int) -> void)).
   (if0 n then (c(1)) else
     (let y1 = n - 1 in (f(y1, (λ(v: int). (let y = n * v in (c(y)))))))))(6,
  (λ(v1: int). halt<int>v1)))|};
      pretty_c = None;
    };
    {
      name = "twice";
      input =
        rm1
          {|
Λa. fix in_f(f: a->a): a->a.
      fix in_x(x: a): a.
        (f (f x))|};
      output = None;
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
halt<
  (∀<a>.
    (((((((a, ((a) -> void)) -> void),
         ((((a, ((a) -> void)) -> void)) -> void)) -> void)) -> void)) -> void)>
  (λ<a>.
    (c:
       ((((((a, ((a) -> void)) -> void),
           ((((a, ((a) -> void)) -> void)) -> void)) -> void)) -> void)).
    (c((fix in_f(f: ((a, ((a) -> void)) -> void),
         c1: ((((a, ((a) -> void)) -> void)) -> void)).
         (c1((fix in_x(x: a, c2: ((a) -> void)).
               (f(x, (λ(v: a). (f(v, (λ(v1: a). (c2(v1)))))))))))))))|};
      pretty_c = None;
    };
  ]

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n\n" ^ Printexc.get_backtrace ())

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
  List.map (mk_test (fun t -> F.(elaborate t |> typeof |> string_of_ty))) cases

let f_eval_tests =
  let cases =
    List.filter_map
      (function
        | { name; input; output = Some output; _ } -> Some (name, input, output)
        | _ -> None)
      cases
  in
  List.map (mk_test (fun t -> F.(eval t |> string_of_term))) cases

let k_pp_tests =
  let cases =
    List.map (fun { name; input; pretty_k; _ } -> (name, input, pretty_k)) cases
  in
  List.map
    (mk_test (fun t -> F.elaborate t |> K.of_F |> K.string_of_term))
    cases

let k_typecheck_tests =
  let cases = List.map (fun { name; input; _ } -> (name, input, "")) cases in
  List.map
    (mk_test (fun t ->
         F.elaborate t |> K.of_F |> K.check_well_typed;
         ""))
    cases

let k_eval_tests =
  let cases =
    List.filter_map
      (fun { name; input; output; _ } ->
        Option.map (fun o -> (name, input, o)) output)
      cases
  in
  List.map
    (mk_test (fun t -> K.(F.elaborate t |> of_F |> eval |> string_of_value)))
    cases

let c_pp_tests =
  let cases =
    List.filter_map
      (fun { name; input; pretty_c; _ } ->
        Option.map (fun o -> (name, input, o)) pretty_c)
      cases
  in
  List.map
    (mk_test (fun t -> F.elaborate t |> K.of_F |> C.of_K |> C.string_of_term))
    cases

let c_typecheck_tests =
  let cases = List.map (fun { name; input; _ } -> (name, input, "")) cases in
  List.map
    (mk_test (fun t ->
         F.elaborate t |> K.of_F |> C.of_K |> C.check_well_typed;
         ""))
    cases

let c_eval_tests =
  let cases =
    List.filter_map
      (function
        | { name; input; output = Some output; _ } -> Some (name, input, output)
        | _ -> None)
      cases
  in
  List.map
    (mk_test (fun t ->
         C.(F.elaborate t |> K.of_F |> of_K |> eval |> string_of_value)))
    cases

let () =
  Alcotest.run "TAL tests"
    [
      ("[F] Pretty Printing", f_pp_tests);
      ("[F] Typecheck", f_typecheck_tests);
      ("[F] Eval", f_eval_tests);
      ("[K] Pretty Printing", k_pp_tests);
      ("[K] Typecheck", k_typecheck_tests);
      ("[K] Eval", k_eval_tests);
      ("[C] Pretty Printing", c_pp_tests);
      ("[C] Typecheck", c_typecheck_tests);
      ("[C] Eval", c_eval_tests);
    ]
