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
fn gen1(succ: nat -> nat, x: ?): nat
  = decl x1: nat = <nat>x;
    decl x2: nat = succ(x1);
    x2
decl x3: ? = <?>true;
decl x4: nat = gen1(succ, x3);
x4
|}
          );
      eval = Some "ERROR: Cast Error at (<nat>(<?>true))";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(succ: ((_: number) => number), x: unknown): number {
  const x1: number = x as number;
  const x2: number = succ(x1);
  return x2;
}
function main1(): number {
  const x3: unknown = true as unknown;
  const x4: number = gen1(succ, x3);
  return x4;
}
main1();
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
fn gen1(succ: nat -> nat, x: nat): nat = decl x1: nat = succ(x);
                                         x1
fn gen2(f: ? -> nat): nat = decl x2: ? = <?>1;
                            decl x3: nat = f(x2);
                            x3
decl x4: nat -> nat = gen1(succ);
decl x5: ? -> nat = <? -> nat>x4;
decl x6: nat = gen2(x5);
x6
|}
          );
      eval = Some "OK: 2";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(succ: ((_: number) => number), x: number): number {
  const x1: number = succ(x);
  return x1;
}
function gen2(f: ((_: unknown) => number)): number {
  const x2: unknown = 1 as unknown;
  const x3: number = f(x2);
  return x3;
}
function main1(): number {
  const x4: ((_: number) => number) = gen1(succ);
  const x5: ((_: unknown) => number) = x4 as ((_: unknown) => number);
  const x6: number = gen2(x5);
  return x6;
}
main1();
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
      llift =
        Some
          ("OK: "
          ^ trim
              {|
fn gen1(x: ?, y: ?): ?
  = decl x1: ? -> ? = <? -> ?>x;
    decl x2: ? = x1(x);
    decl x3: ? -> ? = <? -> ?>x2;
    decl x4: ? = x3(y);
    x4
fn gen2(f: ?, x: ?): ?
  = decl x5: (? -> ?) -> ? = <(? -> ?) -> ?>f;
    decl x6: ? -> ? = gen1(x);
    decl x7: ? = x5(x6);
    x7
fn gen3(x: ?, y: ?): ?
  = decl x8: ? -> ? = <? -> ?>x;
    decl x9: ? = x8(x);
    decl x10: ? -> ? = <? -> ?>x9;
    decl x11: ? = x10(y);
    x11
fn gen4(f: ?, x: ?): ?
  = decl x12: (? -> ?) -> ? = <(? -> ?) -> ?>f;
    decl x13: ? -> ? = gen3(x);
    decl x14: ? = x12(x13);
    x14
fn gen5(f: ?): ?
  = decl x15: ? -> ? = gen2(f);
    decl x16: ? = <?>x15;
    decl x17: ? = gen4(f, x16);
    x17
fn gen6(eqn: nat -> nat -> bool,
        fact: ?,
        mult: nat -> nat -> nat,
        pred: nat -> nat,
        n: ?): nat
  = decl x18: nat = <nat>n;
    decl x19: bool = eqn(x18, 0);
    decl res1: nat;
    if x19
    then
      res1 = 1;
    else decl x25: nat = <nat>n;decl x20: nat -> ? = <nat -> ?>fact;
      decl x21: nat = <nat>n;decl x22: nat = pred(x21);decl x23: ? = x20(x22);
      decl x24: nat = <nat>x23;decl x26: nat = mult(x25, x24);res1 = x26;
    res1
fn gen7(eqn: nat -> nat -> bool,
        mult: nat -> nat -> nat,
        pred: nat -> nat,
        fact: ?): ? -> nat
  = decl x27: ? -> nat = gen6(eqn, fact, mult, pred);
    x27
fn gen8(fact: nat -> nat): nat = decl x28: nat = fact(10);
                                 x28
fn gen9(eqn: nat -> nat -> bool,
        mult: nat -> nat -> nat,
        pred: nat -> nat,
        fix: ?): nat
  = decl x29: (? -> ? -> nat) -> ? = <(? -> ? -> nat) -> ?>fix;
    decl x30: ? -> ? -> nat = gen7(eqn, mult, pred);
    decl x31: ? = x29(x30);
    decl x32: nat -> nat = <nat -> nat>x31;
    decl x33: nat = gen8(x32);
    x33
decl x34: ? = <?>gen5;
decl x35: nat = gen9(eqn, mult, pred, x34);
x35
|}
          );
      eval = Some "OK: 3628800";
      typescript =
        Some
          ("OK: "
          ^ trim
              {|
function gen1(x: unknown, y: unknown): unknown {
  const x1: ((_: unknown) => unknown) = x as ((_: unknown) => unknown);
  const x2: unknown = x1(x);
  const x3: ((_: unknown) => unknown) = x2 as ((_: unknown) => unknown);
  const x4: unknown = x3(y);
  return x4;
}
function gen2(f: unknown, x: unknown): unknown {
  const x5
          : ((_: ((_: unknown) => unknown)) => unknown)
          = f as ((_: ((_: unknown) => unknown)) => unknown);
  const x6: ((_: unknown) => unknown) = gen1(x);
  const x7: unknown = x5(x6);
  return x7;
}
function gen3(x: unknown, y: unknown): unknown {
  const x8: ((_: unknown) => unknown) = x as ((_: unknown) => unknown);
  const x9: unknown = x8(x);
  const x10: ((_: unknown) => unknown) = x9 as ((_: unknown) => unknown);
  const x11: unknown = x10(y);
  return x11;
}
function gen4(f: unknown, x: unknown): unknown {
  const x12
          : ((_: ((_: unknown) => unknown)) => unknown)
          = f as ((_: ((_: unknown) => unknown)) => unknown);
  const x13: ((_: unknown) => unknown) = gen3(x);
  const x14: unknown = x12(x13);
  return x14;
}
function gen5(f: unknown): unknown {
  const x15: ((_: unknown) => unknown) = gen2(f);
  const x16: unknown = x15 as unknown;
  const x17: unknown = gen4(f, x16);
  return x17;
}
function gen6(eqn: ((_: number) => ((_: number) => boolean)),
              fact: unknown,
              mult: ((_: number) => ((_: number) => number)),
              pred: ((_: number) => number),
              n: unknown): number {
  const x18: number = n as number;
  const x19: boolean = eqn(x18, 0);
  let res1: number;
  if
    (x19) {
    res1 = 1;
  } else {
    const x25: number = n as number;
    const x20: ((_: number) => unknown) = fact as ((_: number) => unknown);
    const x21: number = n as number;
    const x22: number = pred(x21);
    const x23: unknown = x20(x22);
    const x24: number = x23 as number;
    const x26: number = mult(x25, x24);
    res1 = x26;
  }
  return res1;
}
function gen7(eqn: ((_: number) => ((_: number) => boolean)),
              mult: ((_: number) => ((_: number) => number)),
              pred: ((_: number) => number),
              fact: unknown): ((_: unknown) => number) {
  const x27: ((_: unknown) => number) = gen6(eqn, fact, mult, pred);
  return x27;
}
function gen8(fact: ((_: number) => number)): number {
  const x28: number = fact(10);
  return x28;
}
function gen9(eqn: ((_: number) => ((_: number) => boolean)),
              mult: ((_: number) => ((_: number) => number)),
              pred: ((_: number) => number),
              fix: unknown): number {
  const x29
          : ((_: ((_: unknown) => ((_: unknown) => number))) => unknown)
          = fix as ((_: ((_: unknown) => ((_: unknown) => number))) => unknown);
  const x30
          : ((_: unknown) => ((_: unknown) => number))
          = gen7(eqn, mult, pred);
  const x31: unknown = x29(x30);
  const x32: ((_: number) => number) = x31 as ((_: number) => number);
  const x33: number = gen8(x32);
  return x33;
}
function main1(): number {
  const x34: unknown = gen5 as unknown;
  const x35: number = gen9(eqn, mult, pred, x34);
  return x35;
}
main1();
|}
          );
    };
  ]

let into_lifted e = elaborate e |> Result.map (fun e -> insert_casts e |> lift)

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
              into_lifted e |> Result.map Cgen.typescript |> stringify_error)
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
