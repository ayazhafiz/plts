open Tal

type testcase = {
  name : string;
  input : string;
  output : string option;
  pretty_f : string;
  typecheck_f : string;
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
      (function
        | { name; input; output = Some output; _ } -> Some (name, input, output)
        | _ -> None)
      cases
  in
  List.map
    (mk_test (fun t -> K.(F.elaborate t |> of_F |> eval |> string_of_value)))
    cases

let () =
  Alcotest.run "TAL tests"
    [
      ("[F] Pretty Printing", f_pp_tests);
      ("[F] Typecheck", f_typecheck_tests);
      ("[F] Eval", f_eval_tests);
      ("[K] Typecheck", k_typecheck_tests);
      ("[K] Eval", k_eval_tests);
    ]
