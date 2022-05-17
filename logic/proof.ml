(** Module [proof] proves theorems. *)

open Resolve
open Norm

(** An environment G is consistent iff it is satisfiable iff G \ X is satisfiable. *)
let rec inconsistent g =
  match ClauseSet.free g with
  | Some x -> (
      match resolve g x with Ok g' -> inconsistent g' | Error Bottom -> true)
  | None -> false

(** A formula [a] is a consequence of environment [g] iff [g \cup {!a}] is not satisfiable. *)
let prove g a =
  let a' = Syntax.Neg a in
  let g' = List.fold_left (fun a b -> Syntax.Conj (a, b)) a' g in
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
