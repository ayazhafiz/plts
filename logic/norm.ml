(** Conjuctive normal form of a formula. *)

module S = Syntax

type variable = Syntax.variable

type literal = Var of variable  (** X *) | Neg of variable  (** !X *)

module LitSet = struct
  include Set.Make (struct
    type t = literal

    let compare = compare
  end)

  let to_list s = List.of_seq @@ to_seq s
end

type clause = LitSet.t
(** [clause C] is a disjunction of literals. An empty disjunction is False. *)

module ClauseSet = struct
  include Set.Make (struct
    type t = LitSet.t

    let compare = LitSet.compare
  end)

  let to_list s = List.of_seq @@ to_seq s
end

type cnf = ClauseSet.t
(** [formula A] is in CNF when it is a list of clause conjunctions.
    An empty conjunctions is True. *)

(** Computes canonical CNF of a formula. *)
let to_can_cnf a : cnf =
  let merge a b =
    (* (C & D) | (E & F) -> [[C], [D]] | [[E], [F]] -> (C | E) & (C | F) & (D | E) & (D | F)  *)
    ClauseSet.fold
      (fun c total ->
        ClauseSet.map (fun d -> LitSet.union c d) b |> ClauseSet.union total)
      a ClauseSet.empty
  in
  let rec pos = function
    | S.Var x -> ClauseSet.singleton @@ LitSet.singleton @@ Var x
    | S.Neg a -> neg a
    | S.Conj (a, b) -> ClauseSet.union (pos a) (pos b)
    | S.Disj (a, b) -> merge (pos a) (pos b)
    | S.Imp (a, b) -> merge (neg a) (pos b) (* A => B = !A \/ B *)
    | S.Bot -> ClauseSet.singleton LitSet.empty
    | S.Top -> ClauseSet.empty
  and neg = function
    | S.Var x -> ClauseSet.singleton @@ LitSet.singleton @@ Neg x
    | S.Neg a -> pos a
    | S.Conj (a, b) -> merge (neg a) (neg b)
    | S.Disj (a, b) -> ClauseSet.union (neg a) (neg b)
    | S.Imp (a, b) -> ClauseSet.union (pos a) (neg b)
    | S.Bot -> pos S.Top
    | S.Top -> pos S.Bot
  in
  (* A /\ (A \/ B) ~> A *)
  let simpl a =
    let sorted_by_len =
      ClauseSet.to_list a
      |> List.sort (fun a b -> compare (LitSet.cardinal a) (LitSet.cardinal b))
    in
    let rec find_unique seen = function
      | [] -> []
      | big_clause :: rest -> (
          let seen_current =
            List.exists (fun clause -> LitSet.subset clause big_clause) seen
          in
          match seen_current with
          | true -> find_unique seen rest
          | false -> big_clause :: find_unique (big_clause :: seen) rest)
    in
    find_unique [] sorted_by_len
  in
  ClauseSet.of_list @@ simpl @@ pos a

let print =
  let print_lit = function Var x -> x | Neg x -> "¬" ^ x in
  let print_clause consider_paren clause =
    let clause = LitSet.to_list clause in
    let needs_paren = consider_paren && List.length clause > 1 in
    let printed = String.concat {| ∨ |} (List.map print_lit clause) in
    if needs_paren then "(" ^ printed ^ ")" else printed
  in
  let print_cnf cnf =
    let cnf = ClauseSet.to_list cnf in
    String.concat {| ∧ |} (List.map (print_clause (List.length cnf > 1)) cnf)
  in
  print_cnf

let%expect_test "to_cnf" =
  let cases =
    [
      {|!(A /\ B)|};
      {|!(A \/ B)|};
      {|(A /\ B) \/ C|};
      {|A \/ (B /\ C)|};
      {|A => B|};
      {|¬(¬X ∨ Y) ∨ (¬Y ∨ Z)|};
    ]
  in
  let cnfed =
    List.map (fun s -> print @@ to_can_cnf @@ Load.parse s) cases
    |> String.concat "\n"
  in
  print_string cnfed;
  [%expect
    {|
      ¬A ∨ ¬B
      ¬A ∧ ¬B
      (A ∨ C) ∧ (B ∨ C)
      (A ∨ B) ∧ (A ∨ C)
      B ∨ ¬A
      Z ∨ ¬Y |}]
