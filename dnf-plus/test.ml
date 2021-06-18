open Ft

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ())

let rm1 s = String.sub s 1 (String.length s - 1)

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
        rm1
          {|
any |
  int |
  (any, any) |
  (int, int) |
  (any, any)&(any, int) |
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

let program_cases =
  [
    ( {|
fn f(x: any) = if x is int then 1 else (0, 0)
in (f 1, f (1, 2))
|},
      Ok "(int, (int, int))" );
  ]

let program_tests =
  let mk_test (p, expect) =
    let p = String.trim p in
    let test () =
      let result = typecheck (parse_term p) |> Result.map string_of_ty in
      Alcotest.(check (result string string)) p expect result
    in
    (p, `Quick, wrap test)
  in
  List.map mk_test program_cases

let readfi path =
  let ch = open_in path in
  let content = really_input_string ch (in_channel_length ch) in
  close_in ch;
  content

let writefi path content =
  let ch = open_out path in
  output_string ch content;
  close_out ch

let examples =
  Sys.readdir "examples" |> Array.to_list
  |> List.filter (fun f -> Filename.extension f = ".ft")
  |> List.map (Filename.concat "examples")

let example_tests =
  let mk_test example =
    let test _ =
      let p = readfi example |> parse_term in
      ignore (typecheck p);
      let realannot = Filename.remove_extension example ^ ".realannot" in
      let annotated = string_of_term p in
      ignore (parse_term annotated) (* make sure it parses *);
      writefi realannot annotated
    in
    (example, `Quick, test)
  in
  List.map mk_test examples

let () =
  Alcotest.run "FT tests"
    [
      ("Type Parser", type_parser_tests);
      ("Disjunctive Normal Form", dnf_tests);
      ("Canonicalized Disjunctive Normal Form", dnf_plus_tests);
      ("Subtyping", subtype_tests);
      ("Program Typing", program_tests);
      ("Examples", example_tests);
    ]
