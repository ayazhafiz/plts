(** Module [resolve] computes the resolvent of an environment relative to a
    variable.
    A resolvent G \ X of environment G with respect to X is
      G \ X = G' \cup { C \/ D | C \in G_X, D \in G_{!X} }
    where
      G' = { C | X \notin C, !X \notin C, C \in G }
      G_X = { C | C \/ X \in G }
      G_{!X} = { D | D \/ !X \in G } *)

open Norm

type bottom = Bottom

(** Computes the resolvent of an environment [g] relative to [x].
    [g] is in canonical clausal form [cnf]. *)
let resolve g x =
  let g' =
    g |> ClauseSet.filter (LitSet.for_all (fun l -> l <> Var x && l <> Neg x))
  in
  let gx =
    g
    |> ClauseSet.filter (LitSet.mem (Var x))
    |> ClauseSet.map (LitSet.remove (Var x))
  in
  let gx' =
    g
    |> ClauseSet.filter (LitSet.mem (Neg x))
    |> ClauseSet.map (LitSet.remove (Neg x))
  in
  let g'' = ClauseSet.merge gx gx' in
  ClauseSet.union g' g'' |> ClauseSet.pack

let%expect_test "resolve" =
  let cases =
    [
      ([ {|C \/ X|}; {|!X \/ D|} ], "X");
      ([ {|C1 \/ X \/ C2|}; {|D1 \/ !X \/ D2|} ], "X");
      ([ {|!C => X|}; {|X => D|} ], "X");
    ]
  in
  let results =
    List.map
      (fun (g, x) ->
        let g = List.map Syntax.parse g |> Syntax.conj_list |> to_can_cnf in
        resolve g x)
      cases
    |> List.map print |> String.concat "\n"
  in
  print_string results;
  [%expect {|
      C ∨ D
      C1 ∨ C2 ∨ D1 ∨ D2
      C ∨ D |}]
