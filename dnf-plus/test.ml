open Ft

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ())

type type_test = {
  input : string;
  parse : string;
  dnf : string;
  dnf_plus : string;
}

let type_cases =
  [
    {
      input = "int | any";
      parse = "any | int";
      dnf = "any | int";
      dnf_plus = "any | int";
    };
    {
      input = "!int|any";
      parse = "any | !int";
      dnf = "any | !int";
      dnf_plus = "any | any&!int";
    };
    {
      input = "!(int|any)";
      parse = "!(any | int)";
      dnf = "!any & !int";
      dnf_plus = "never";
    };
    {
      input = "!(int&any)";
      parse = "!(any & int)";
      dnf = "!any | !int";
      dnf_plus = "any & !int";
    };
    {
      input = "int & (any | (int, int))";
      parse = "int & (any | (int, int))";
      dnf = "any&int | int&(int, int)";
      dnf_plus = "int";
    };
    {
      input = "(int | (int, int), any)";
      parse = "(int | (int, int), any)";
      dnf = "(int, any) | ((int, int), any)";
      dnf_plus = "(int, any) | ((int, int), any)";
    };
    {
      input = "(any & !int, any)";
      parse = "(any & !int, any)";
      dnf = "(any, any) & !(int, any)";
      dnf_plus = "(any, any) & !(int, any)";
    };
    {
      input = "(int, int)&int | int&any";
      parse = "any&int | int&(int, int)";
      dnf = "any&int | int&(int, int)";
      dnf_plus = "int";
    };
    {
      input = "(int|any) | ((int, int)|(any, any&(int|(any|(int, any)))))";
      parse = "any | int | (any, any & (any | int | (int, any))) | (int, int)";
      dnf =
        {|any | int | (any, any) | (int, int) | (any, any)&(any, int) |
  (any, any)&(any, (int, any))|};
      dnf_plus =
        "any | int | (any, any) | (any, int) | (any, (int, any)) | (int, int)";
    };
  ]

let mk_test driver (input, expect) =
  let test _ =
    let out = parse_ty input |> driver |> string_of_ty in
    Alcotest.(check string) input expect out
  in
  (input, `Quick, wrap test)

let type_parser_tests =
  let cases = List.map (fun { input; parse; _ } -> (input, parse)) type_cases in
  List.map (mk_test (fun t -> t)) cases

let dnf_tests =
  let cases = List.map (fun { input; dnf; _ } -> (input, dnf)) type_cases in
  List.map (mk_test (fun t -> dnf t |> ty_of_dnf)) cases

let dnf_plus_tests =
  let cases =
    List.map (fun { input; dnf_plus; _ } -> (input, dnf_plus)) type_cases
  in
  List.map (mk_test dnf_plus) cases

let subtype_cases =
  [
    "any <: int | !int";
    "int | !int <: any";
    "(int&(int, int), int) <: (int, int) & ((int, int), int)";
  ]

let subtype_tests =
  let open Str in
  let mk_test (case, l, r, expect) =
    let test () =
      Alcotest.(check bool) case expect (parse_ty l <: parse_ty r)
    in
    (case, `Quick, wrap test)
  in
  List.map
    (fun c ->
      match split (regexp_string "<:") c with
      | [ l; r ] -> (c, l, r, true)
      | _ -> (
          match split (regexp_string "</:") c with
          | [ l; r ] -> (c, l, r, false)
          | _ -> failwith "bad case"))
    subtype_cases
  |> List.map mk_test

let () =
  Alcotest.run "FT tests"
    [
      ("Type Parser", type_parser_tests);
      ("Disjunctive Normal Form", dnf_tests);
      ("Canonicalized Disjunctive Normal Form", dnf_plus_tests);
      ("Subtyping", subtype_tests);
    ]
