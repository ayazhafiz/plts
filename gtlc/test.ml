open Gtlc

let wrap test =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  try test
  with _ as e ->
    Printexc.record_backtrace false;
    Alcotest.fail (Printexc.to_string e ^ "\n\n" ^ Printexc.get_backtrace ())

let mk_test driver (name, input, expect) =
  let test () =
    let out = parse input |> Result.get_ok |> driver in
    Alcotest.(check string) input expect out
  in
  (name, `Quick, wrap test)

let trim = String.trim

let stringify_error = function Ok s -> "OK: " ^ s | Error s -> "ERROR: " ^ s

let ( >>= ) = Result.bind

type case = {
  input : string;
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
      typecheck = "ERROR: Argument is not consistent with domain of application";
      cast = None;
      llift = None;
      eval = None;
      typescript = None;
    };
    {
      input = "(λx. succ x) #t";
      typecheck = "OK: nat";
      cast = None;
      llift =
        Some
          ("OK: "
          ^ trim
              {|
fn gen1(env1: {}, x: ?): nat
  = decl x1: nat = <nat>x;
    return apply(succ, x1)
decl x2: ? = <?>true;
return apply(pack(gen1, {}), x2)
|}
          );
      eval = Some "ERROR: Cast Error at (<nat>(<?>true))";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(env1: [], x: v<unknown>): v<number> {
  const x1: v<number> = _cast(x, _tn);
  return succ.value.apply(x1);
}
function main1(): v<number> {
  const x2: v<unknown> = _cast(_nb(true), _tu);
  return _nf(new Clos(gen1, []), _tf(_tu, _tn)).value.apply(x2);
}
_print(main1());
|}
          );
    };
    {
      input = "(λf: ? -> nat. f 1) (λx: nat. succ x)";
      typecheck = "OK: nat";
      cast = None;
      llift =
        Some
          ("OK: "
          ^ trim
              {|
fn gen1(env1: {}, f: Clos(? -> nat)): nat
  = decl x1: ? = <?>1;
    return apply(f, x1)
fn gen2(env2: {}, x: nat): nat
  = return apply(succ, x)
decl x2: Clos(? -> nat) = <Clos(? -> nat)>pack(gen2, {});
return apply(pack(gen1, {}), x2)
|}
          );
      eval = Some "OK: 2";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(env1: [], f: v<Clos<v<unknown>, v<number>>>): v<number> {
  const x1: v<unknown> = _cast(_nn(1), _tu);
  return f.value.apply(x1);
}
function gen2(env2: [], x: v<number>): v<number> {
  return succ.value.apply(x);
}
function main1(): v<number> {
  const x2
          : v<Clos<v<unknown>, v<number>>>
          = _cast(_nf(new Clos(gen2, []), _tf(_tn, _tn)), _tf(_tu, _tn));
  return _nf(new Clos(gen1, []), _tf(_tf(_tu, _tn), _tn)).value.apply(x2);
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
      typecheck = "OK: nat";
      cast = None;
      llift = None;
      eval = Some "OK: 3628800";
      typescript = None;
    };
  ]

let into_lifted e =
  elaborate e |> Result.map (fun e -> insert_casts e |> lift ~optimize:true)

let typecheck_tests =
  List.map
    (fun { input; typecheck; _ } ->
      mk_test
        (fun e ->
          elaborate e
          |> Result.map (fun e -> ty_of_elaborated_expr e |> string_of_ty)
          |> stringify_error)
        (input, input, typecheck))
    cases

let cast_tests =
  List.filter_map
    (function
      | { input; cast = Some expect; _ } ->
          mk_test
            (fun e ->
              elaborate e
              |> Result.map (fun e -> insert_casts e |> string_of_cast_expr)
              |> stringify_error)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let llift_tests =
  List.filter_map
    (function
      | { input; llift = Some expect; _ } ->
          mk_test
            (fun e ->
              into_lifted e
              |> Result.map string_of_lifted_program
              |> stringify_error)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let eval_tests =
  List.filter_map
    (function
      | { input; eval = Some expect; _ } ->
          mk_test
            (fun e ->
              elaborate e >>= eval |> Result.map string_of_value
              |> stringify_error)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let codegen_ts_tests =
  List.filter_map
    (function
      | { input; typescript = Some expect; _ } ->
          mk_test
            (fun e ->
              into_lifted e
              |> Result.map (Cgen.typescript ~with_prelude:false)
              |> stringify_error)
            (input, input, expect)
          |> Option.some
      | _ -> None)
    cases

let () =
  Alcotest.run "GTLC tests"
    [
      ("Typecheck", typecheck_tests);
      ("Cast Insertion", cast_tests);
      ("Lambda Lifting", llift_tests);
      ("Eval", eval_tests);
      ("Codegen: TypeScript", codegen_ts_tests);
    ]
