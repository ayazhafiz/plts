open Ft

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ())

type test = { input : string; parse : string; dnf : string }

let t input parse dnf = { input; parse; dnf }

let cases =
  [
    { input = "int | any"; parse = "any | int"; dnf = "any | int" };
    { input = "!int|any"; parse = "any | !int"; dnf = "any | !int" };
    { input = "!(int|any)"; parse = "!(any | int)"; dnf = "!any & !int" };
    { input = "!(int&any)"; parse = "!(any & int)"; dnf = "!any | !int" };
    {
      input = "int & (any | (int, int))";
      parse = "int & (any | (int, int))";
      dnf = "any&int | int&(int, int)";
    };
    {
      input = "(int | (int, int), any)";
      parse = "(int | (int, int), any)";
      dnf = "(int, any) | ((int, int), any)";
    };
    {
      input = "(any & !int, any)";
      parse = "(any & !int, any)";
      dnf = "(any, any) & !(int, any)";
    };
    {
      input = "(int, int)&int | int&any";
      parse = "any&int | int&(int, int)";
      dnf = "any&int | int&(int, int)";
    };
    {
      input = "(int|any) | ((int, int)|(any, any&(int|(any|(int, any)))))";
      parse = "any | int | (any, any & (any | int | (int, any))) | (int, int)";
      dnf =
        {|any | int | (any, any) | (int, int) | (any, any)&(any, int) |
  (any, any)&(any, (int, any))|};
    };
  ]

let mk_test driver (input, expect) =
  let test _ =
    let out = parse_ty input |> driver |> string_of_ty in
    Alcotest.(check string) input expect out
  in
  (input, `Quick, wrap test)

let type_parser_tests =
  let cases = List.map (fun { input; parse; _ } -> (input, parse)) cases in
  List.map (mk_test (fun t -> t)) cases

let dnf_tests =
  let cases = List.map (fun { input; dnf; _ } -> (input, dnf)) cases in
  List.map (mk_test dnf) cases

let () =
  Alcotest.run "FT tests"
    [
      ("type parser", type_parser_tests); ("Disjunctive Normal Form", dnf_tests);
    ]
