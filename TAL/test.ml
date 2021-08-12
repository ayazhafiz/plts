open Talc
module TAL = OCamlTAL

type testcase = {
  name : string;
  input : string;
  output : string option;
  pretty_f : string;
  typecheck_f : string;
  pretty_k : string;
  pretty_c : string option;
  pretty_h : string option;
  pretty_a : string option;
  pretty_tal : string option;
}

let rm1 s = String.sub s 1 (String.length s - 1)

let cases =
  [
    {
      name = "fib6";
      input =
        rm1
          {|
let $fact =
  (fix f (n: int): int.
    if0 n then 1
    else n * f (n - 1))
in $fact 6|};
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
      pretty_h = None;
      pretty_a = None;
      pretty_tal = None;
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
      pretty_h = None;
      pretty_a = None;
      pretty_tal = None;
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

let to_k parsed = F.elaborate parsed |> K.convert

let to_c parsed = to_k parsed |> C.convert

let to_h parsed = to_c parsed |> H.convert

let to_a parsed = to_h parsed |> A.convert

let to_tal parsed = to_a parsed |> TAL.convert

let mk_typecheck_wf_tests driver =
  let cases = List.map (fun { name; input; _ } -> (name, input, "")) cases in
  List.map
    (mk_test (fun t ->
         driver t;
         ""))
    cases

let mk_opt_pp_tests output driver =
  let cases =
    List.filter_map
      (fun ({ name; input; _ } as c) ->
        Option.map (fun o -> (name, input, o)) (output c))
      cases
  in
  List.map (mk_test driver) cases

let mk_eval_tests driver =
  let cases =
    List.filter_map
      (function
        | { name; input; output = Some output; _ } -> Some (name, input, output)
        | _ -> None)
      cases
  in
  List.map (mk_test driver) cases

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

let f_eval_tests = mk_eval_tests (fun t -> F.(eval t |> string_of_term))

let k_pp_tests =
  let cases =
    List.map (fun { name; input; pretty_k; _ } -> (name, input, pretty_k)) cases
  in
  List.map (mk_test (fun t -> to_k t |> K.string_of_term)) cases

let k_typecheck_tests =
  mk_typecheck_wf_tests (fun t -> to_k t |> K.check_well_typed)

let k_eval_tests =
  mk_eval_tests (fun t -> to_k t |> K.eval |> K.string_of_value)

let c_pp_tests =
  mk_opt_pp_tests
    (fun { pretty_c; _ } -> pretty_c)
    (fun t -> to_c t |> C.string_of_term)

let c_typecheck_tests =
  mk_typecheck_wf_tests (fun t -> to_c t |> C.check_well_typed)

let c_eval_tests =
  mk_eval_tests (fun t -> to_c t |> C.eval |> C.string_of_value)

let h_pp_tests =
  mk_opt_pp_tests
    (fun { pretty_h; _ } -> pretty_h)
    (fun t -> to_h t |> H.string_of_term)

let h_typecheck_tests =
  mk_typecheck_wf_tests (fun t -> to_h t |> H.check_well_typed)

let h_eval_tests =
  mk_eval_tests (fun t -> to_h t |> H.eval |> H.string_of_value)

let a_pp_tests =
  mk_opt_pp_tests
    (fun { pretty_a; _ } -> pretty_a)
    (fun t -> to_a t |> A.string_of_term)

let a_typecheck_tests =
  mk_typecheck_wf_tests (fun t -> to_a t |> A.check_well_typed)

let a_eval_tests =
  mk_eval_tests (fun t -> to_a t |> A.eval |> A.string_of_value)

let tal_pp_tests =
  mk_opt_pp_tests
    (fun { pretty_tal; _ } -> pretty_tal)
    (fun t -> to_tal t |> TAL.string_of_term)

let tal_typecheck_tests =
  mk_typecheck_wf_tests (fun t -> to_tal t |> TAL.check_well_typed)

let tal_eval_tests =
  mk_eval_tests (fun t -> to_tal t |> TAL.eval |> TAL.string_of_value)

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
      ("[H] Pretty Printing", h_pp_tests);
      ("[H] Typecheck", h_typecheck_tests);
      ("[H] Eval", h_eval_tests);
      ("[A] Pretty Printing", a_pp_tests);
      ("[A] Typecheck", a_typecheck_tests);
      (* ("[A] Eval", a_eval_tests); *)
      (* TODO: fix eval, problem is with runtime heap ): *)
      ("[TAL] Pretty Printing", tal_pp_tests);
      ("[TAL] Typecheck", tal_typecheck_tests);
      ("[TAL] Eval", tal_eval_tests);
    ]
