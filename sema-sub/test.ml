open Gtlc

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n\n" ^ Printexc.get_backtrace ())

let trim = String.trim

let stringify_error = function Ok s -> "OK: " ^ s | Error s -> "ERROR: " ^ s

let ( >>= ) = Result.bind

let mk_test driver (name, input, expect) =
  let test () =
    let out = parse input >>= driver |> stringify_error in
    Alcotest.(check string) input expect out
  in
  (name, `Quick, wrap test)

type case = {
  input : string;
  infer : string option;
  typecheck : string;
  cast : string option;
  llift : string option;
  eval : string option;
  typescript : string option;
}

let cases =
  [
    {
      input = "(λx: nat. succ x) #t";
      infer = None;
      typecheck = "ERROR: Argument is not consistent with domain of application";
      cast = None;
      llift = None;
      eval = None;
      typescript = None;
    };
    {
      input = "(λx. succ x) #t";
      infer = Some "OK: (λx: ?. succ x) #t";
      typecheck = "OK: nat";
      cast = None;
      llift = None;
      eval = None;
      typescript = None;
    };
    {
      input =
        trim
          {|
let apply1To = λf: ? -> nat. f 1 in
apply1To (λx: nat. succ x)
|};
      infer =
        Some
          ("OK: "
          ^ trim
              {|
let apply1To: ? = λf: ? -> nat. f 1 in
apply1To (λx: nat. succ x)
|}
          );
      typecheck = "OK: ?";
      cast = None;
      llift = None;
      eval = None;
      typescript = None;
    };
    {
      input = "(λf: ? -> nat. f 1) (λx: nat. succ x)";
      infer = None;
      typecheck = "OK: nat";
      cast = None;
      llift =
        Some
          ("OK: "
          ^ trim
              {|
fn gen1(f: Clos(? -> nat)): nat
  = decl x1: ? = <?>1;
    return apply(f, x1);
fn gen2(x: nat): nat
  = return apply(succ, x);
decl x2: Clos(? -> nat) = <Clos(? -> nat)>fnptr(gen2);
return apply(fnptr(gen1), x2);
|}
          );
      eval = Some "OK: 2";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(f: v<Fn<v<unknown>, v<number>>>): v<number> {
  const x1: v<unknown> = _cast(_nn(1), _tu);
  return f.value.apply(x1);
}
function gen2(x: v<number>): v<number> {
  return succ.value.apply(x);
}
function main1(): v<number> {
  const x2
          : v<Fn<v<unknown>, v<number>>>
          = _cast(_nf(new FnPtr(gen2), _tf(_tn, _tn)), _tf(_tu, _tn));
  return _nf(new FnPtr(gen1), _tf(_tf(_tu, _tn), _tn)).value.apply(x2);
}
_print(main1());
|}
          );
    };
    {
      input =
        trim
          {|
let fix = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y)) in
let fact: nat -> nat =
  fix (\fact. \n. if eqn n 0 then 1 else (mult n (fact (pred n)))) in
fact 10
|};
      infer = None;
      typecheck = "OK: nat";
      cast = None;
      llift = None;
      eval = Some "OK: 3628800";
      typescript = None;
    };
  ]

let into_lifted e =
  elaborate e |> Result.map (fun e -> insert_casts e |> lift ~optimize:true)

let infer_tests =
  List.filter_map
    (function
      | { input; infer = Some exp; _ } ->
          mk_test
            (fun (e, ft) ->
              infer e ft |> Result.map (fun e -> string_of_expr e))
            (input, input, exp)
          |> Option.some
      | _ -> None)
    cases

let typecheck_tests =
  List.map
    (fun { input; typecheck; _ } ->
      mk_test
        (fun (e, _) ->
          elaborate e
          |> Result.map (fun e -> ty_of_elaborated_expr e |> string_of_ty))
        (input, input, typecheck))
    cases

let cast_tests =
  List.filter_map
    (function
      | { input; cast = Some expect; _ } ->
          mk_test
            (fun (e, _) ->
              elaborate e
              |> Result.map (fun e -> insert_casts e |> string_of_cast_expr))
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let llift_tests =
  List.filter_map
    (function
      | { input; llift = Some expect; _ } ->
          mk_test
            (fun (e, _) -> into_lifted e |> Result.map string_of_lifted_program)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let eval_tests =
  List.filter_map
    (function
      | { input; eval = Some expect; _ } ->
          mk_test
            (fun (e, _) -> elaborate e >>= eval |> Result.map string_of_value)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let codegen_ts_tests =
  List.filter_map
    (function
      | { input; typescript = Some expect; _ } ->
          mk_test
            (fun (e, _) ->
              into_lifted e |> Result.map (Cgen.typescript ~with_prelude:false))
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let () =
  Alcotest.run "GTLC tests"
    [
      ("Infer", infer_tests);
      ("Typecheck", typecheck_tests);
      ("Cast Insertion", cast_tests);
      ("Lambda Lifting", llift_tests);
      ("Eval", eval_tests);
      ("Codegen: TypeScript", codegen_ts_tests);
    ]
