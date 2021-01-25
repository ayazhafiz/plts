let term s =
  let open SimpleSub in
  let dummy =
    Printf.sprintf "let dummy = %s" s
    |> Lexing.from_string |> Parser.program Lexer.read
  in
  Parsing.clear_parser ();
  Lib.pipeline_typecheck Lib.default_ctx dummy |> fun a ->
  ( match a with
  | [ Ok ("dummy", ty) ], _ -> ty
  | _ -> failwith "impossible state" )
  |> Print.string_of_ty

let go name what expect =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name what expect)

let triv () =
  Alcotest.(check string) "int" (term "42") "int";
  Alcotest.(check string) "top->int" (term "fn x -> 42") "any -> int"

let triv_set =
  [
    go "int" (term "42") "int";
    go "any -> int" (term "fn x -> 42") "any -> int";
    go "identity" (term "fn x -> x") "'a -> 'a";
    go "higher-order fn" (term "fn x -> x 42") "(int -> 'a) -> 'a";
    go "arg to identity" (term "(fn x -> x) 42") "int";
    go "twice"
      (term "let twice = fn f -> fn x -> f (f x) in twice")
      "('a ∨ 'b -> 'a) -> 'b -> 'a";
  ]

(* Run it *)
let () = Alcotest.run "typecheck" [ ("trivial cases", triv_set) ]
