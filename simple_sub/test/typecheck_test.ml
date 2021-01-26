let infer debug term =
  let open SimpleSub in
  let res = Typecheck.typeTerm Lib.default_ctx 0 term in
  if debug then Printf.eprintf "infer:\n  %s\n" (Print.string_of_sty res);
  res

let canonicalize debug sty =
  let open SimpleSub in
  let res = Simplify.compactSimpleTy sty in
  if debug then Printf.eprintf "canonical:\n  %s\n" (Print.string_of_cty res);
  res

let simplify debug cty =
  let open SimpleSub in
  let res = Simplify.simplifyTy cty in
  if debug then Printf.eprintf "simple:\n  %s\n" (Print.string_of_cty res);
  res

let tyck debug s =
  let open SimpleSub in
  let term = Lexing.from_string s |> Parser.term_only Lexer.read in
  Parsing.clear_parser ();

  term |> infer debug |> canonicalize debug |> simplify debug
  |> Simplify.coalesceCompactTy |> Print.string_of_ty

let t ?(d = false) term expect =
  let ty = tyck d term in
  Alcotest.test_case term `Quick (fun () ->
      Alcotest.(check string) term expect ty)

let e ?(d = false) term expect =
  Alcotest.test_case term `Quick (fun () ->
      Alcotest.check_raises term (Failure expect) (fun () ->
          let _ = tyck d term in
          ()))

(* The test sets from Parreaux's impl, to prove conformence. *)

let triv_set =
  [
    t "42" "int";
    t "fn x -> 42" "any -> int";
    t "fn x -> x" "'a -> 'a";
    t "fn x -> x 42" "(int -> 'a) -> 'a";
    t "(fn x -> x) 42" "int";
    t "let twice = fn f -> fn x -> f (f x) in twice" "('a|'b -> 'a) -> 'b -> 'a";
  ]

let bool_set =
  [
    t "true" "bool";
    t "not true" "bool";
    t "fn x -> not x" "bool -> bool";
    t "(fn x -> not x) true" "bool";
    t "fn x -> fn y -> fn z -> if x then y else z" "bool -> 'a -> 'a -> 'a";
    t "fn x -> fn y -> if x then y else x" "'a&bool -> 'a -> 'a";
  ]

let bool_fail_set =
  [
    e "succ true" "cannot constrain bool <: int";
    e "fn x -> succ (not x)" "cannot constrain bool <: int";
    e "(fn x -> not x.f) {f: 123}" "cannot constrain int <: bool";
    e "(fn f -> fn x -> not (f x.u)) false" "cannot constrain bool <: 'a -> 'b";
  ]

let record_set =
  [
    t "{}" "{}";
    t "{f: 42}" "{f: int}";
    t "{f: 42}.f" "int";
    t "fn x -> x.f" "{f: 'a} -> 'a";
    t "(fn x -> x.f) {f: 42}" "int";
    t "fn f -> {x: f 42}.x" "(int -> 'a) -> 'a";
    t "fn f -> {x: f 42, y: 123}.y" "(int -> any) -> int";
    t "if true then {a: 1, b: true} else {b: false, c: 42}" "{b: bool}";
  ]

let record_fail_set =
  [
    e "{a: 123, b: true }.c" "no field \"c\" in {a: int, b: bool}";
    e "fn x -> {a: x}.b" "no field \"b\" in {a: 'a}";
  ]

let self_app_set =
  [
    t "fn x -> x x" "'a&('a -> 'b) -> 'b";
    t "fn x -> x x x" "'a&('a -> 'a -> 'b) -> 'b";
    t "fn x -> fn y -> x y x" "'a&('b -> 'a -> 'c) -> 'b -> 'c";
    t "fn x -> fn y -> x x y" "'a&('a -> 'b -> 'c) -> 'b -> 'c";
    t "(fn x -> x x) (fn x -> x x)" "never";
    t "fn x -> {l: x x, r: x}" "'a&('a -> 'b) -> {l: 'b, r: 'a}";
    (* Y combinator *)
    t "(fn f -> (fn x -> f (x x)) (fn x -> f (x x)))" "('a -> 'a) -> 'a";
    (* Z combinator *)
    t "(fn f -> (fn x -> f (fn v -> (x x) v)) (fn x -> f (fn v -> (x x) v)))"
      "(('a -> 'b) -> 'c&('a -> 'b)) -> 'c";
    (* Hungry fnction *)
    t
      "(fn f -> (fn x -> f (fn v -> (x x) v)) (fn x -> f (fn v -> (x x) v))) \
       (fn f -> fn x -> f)"
      "any -> (any -> 'a) as 'a";
    t "let rec trutru = fn g -> trutru (g true) in trutru"
      "(bool -> 'a) as 'a -> never";
    t "fn i -> if ((i i) true) then true else true"
      "'a&('a -> bool -> bool) -> bool";
  ]

let let_poly_set =
  [
    t "let f = fn x -> x in {a: f 0, b: f true}" "{a: int, b: bool}";
    t "fn y -> let f = fn x -> x in {a: f y, b: f true}"
      "'a -> {a: 'a, b: bool}";
    t "fn y -> let f = fn x -> y x in {a: f 0, b: f true}"
      "(bool ∨ int -> 'a) -> {a: 'a, b: 'a}";
    t "fn y -> let f = fn x -> x y in {a: f (fn z -> z), b: f (fn z -> true)}"
      "'a -> {a: 'a, b: bool}";
    t "fn y -> let f = fn x -> x y in {a: f (fn z -> z), b: f (fn z -> succ z)}"
      "'a ∧ int -> {a: 'a, b: int}";
    e "(fn k -> k (fn x -> let tmp = add x 1 in x)) (fn f -> f true)"
      "cannot constrain bool <: int";
    e
      "(fn k -> let test = k (fn x -> let tmp = add x 1 in x) in test) (fn f \
       -> f true)"
      "cannot constrain bool <: int";
  ]

let recursive =
  [
    t "let rec f = fn x -> f x.u in f" "{u: 'a} as 'a -> never";
    t "let rec r = fn a -> r in if true then r else r" "(any -> 'a) as 'a";
    t
      "let rec l = fn a -> l in let rec r = fn a -> fn a -> r in if true then \
       l else r"
      "(any -> any -> 'a) as 'a";
    t
      "let rec l = fn a -> fn a -> fn a -> l in let rec r = fn a -> fn a -> r \
       in if true then l else r"
      "(any -> any -> any -> any -> any -> any -> 'a) as 'a";
    t
      "let rec recursive_monster = fn x -> {thing: x, self: recursive_monster \
       x} in recursive_monster"
      "'a -> {self: 'b, thing: 'a} as 'b";
  ]

(* Run it *)
let () =
  Alcotest.run "typecheck"
    [
      ("trivial cases", triv_set);
      ("bools", bool_set);
      ("bool failures", bool_fail_set);
      ("records", record_set);
      ("record failures", record_fail_set);
      ("self-application", self_app_set);
      ("let-polymorphism", let_poly_set);
      ("recursive", recursive);
    ]
