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
  match ClauseSet.find_first_opt LitSet.is_false g'' with
  | Some _ -> Error Bottom
  | None -> Ok (ClauseSet.union g' g'')
