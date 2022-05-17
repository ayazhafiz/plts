(** Module [proof] proves theorems. *)

open Resolve
open Norm

(** An environment G is consistent iff it is satisfiable iff G \ X is satisfiable. *)
let rec inconsistent g =
  match ClauseSet.free g with
  | Some x ->
      let g' = resolve g x in
      if ClauseSet.is_false g' then true else inconsistent g'
  | None -> false

let%expect_test "inconsistent" =
  let cases =
    [ [ "X => Y"; "Y => Z"; "X" ]; [ "X => Y"; "Y => Z" ]; [ "X"; "!X" ] ]
  in
  let results =
    List.map
      (fun g ->
        List.map Load.parse g |> Syntax.conj_list |> to_can_cnf |> inconsistent)
      cases
    |> List.map Bool.to_string |> String.concat "\n"
  in
  print_string results;
  [%expect {|
      false
      false
      true |}]

(** A formula [a] is a consequence of environment [g] iff [g \cup {!a}] is not satisfiable. *)
let prove g a =
  let a' = Syntax.Neg a in
  let g' = Syntax.conj_list ~first:a' g in
  inconsistent (to_can_cnf g')

let%expect_test "prove" =
  let cases =
    [
      ([ "X => Y"; "Y => Z"; "X" ], "Z");
      ([ "X => Y"; "Y => Z" ], "Z");
      ([ "X"; "!X" ], "X");
    ]
  in
  let results =
    List.map (fun (g, a) -> prove (List.map Load.parse g) (Load.parse a)) cases
    |> List.map Bool.to_string |> String.concat "\n"
  in
  print_string results;
  [%expect {|
      true
      false
      true |}]
