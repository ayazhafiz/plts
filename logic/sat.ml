(** Module [sat] computes boolean satisfiability. *)

(** Naive recursive backtracking algorithm for boolean satisfiability. *)
module Naive = struct
  open Syntax

  let subst x c a =
    let rec go = function
      | Var y -> if x = y then c else Var y
      | Neg a -> Neg (go a)
      | Conj (a, b) -> Conj (go a, go b)
      | Disj (a, b) -> Disj (go a, go b)
      | Imp (a, b) -> Imp (go a, go b)
      | Top -> Top
      | Bot -> Bot
    in
    go a

  let if_none f = function Some a -> Some a | None -> f

  let rec free = function
    | Var x -> Some x
    | Neg a -> free a
    | Conj (a, b) | Disj (a, b) | Imp (a, b) -> free a |> if_none (free b)
    | Top | Bot -> None

  let rec sat a =
    match free a with
    | Some x -> sat (subst x Top a) || sat (subst x Bot a)
    | None -> eval a
end

(** DPLL (1962) algorithm for boolean satisfiability, using canonical conjuctive normal form. *)
module DPLL = struct
  open Norm

  let obv_sat = to_can_cnf Top

  let obv_unsat = to_can_cnf Bot

  let opp = function Var x -> Neg x | Neg x -> Var x

  (** Eliminates a literal in a Can CNF. If the literal is positive, we
      eliminate the disjunctions that positively reference it and remove it from
      the disjunctions that negatively reference it.
      Respectively for negative literals. *)
  let elim a x =
    a
    |> ClauseSet.filter (fun clause -> not (LitSet.mem x clause))
    |> ClauseSet.map (LitSet.filter (fun lit -> lit <> opp x))

  (** Finds a unitary variable in a Can CNF, that is a singleton disjunction. *)
  let unitary a =
    ClauseSet.find_first_opt (fun disj -> LitSet.cardinal disj = 1) a
    |> Option.map LitSet.choose

  (** Finds a pure variable in a Can CNF, that is a variable that only appears
      positively or negatively. *)
  let pure a =
    let vars = ClauseSet.fold LitSet.union a LitSet.empty in
    LitSet.find_first_opt (fun lit -> not (LitSet.mem (opp lit) vars)) vars

  let sat a =
    let cnf = to_can_cnf a in
    let rec sat a =
      (* 1a. If a is True, it is satisfiable *)
      if ClauseSet.equal a obv_sat then true
        (* 1b. If a is False, it is unsatisfiable *)
      else if ClauseSet.equal a obv_unsat then false
      else
        match unitary a with
        (* 2. If there is a unitary clause, apply it. *)
        | Some x -> sat (elim a x)
        | None -> (
            match pure a with
            (* 3. If there is a pure clause, apply it. *)
            | Some x -> sat (elim a x)
            | None ->
                (* 4. Split on an arbitrary variable. *)
                let x = LitSet.choose @@ ClauseSet.choose a in
                sat (elim a x) || sat (elim a x))
    in

    sat cnf
end

let%expect_test "sat" =
  let cases =
    [
      {|!(A /\ B)|};
      {|!(A \/ B)|};
      {|(A /\ B) \/ C|};
      {|A \/ (B /\ C)|};
      {|A => B|};
      {|¬(¬X ∨ Y) ∨ (¬Y ∨ Z)|};
      {|A /\ !A |};
    ]
  in
  let sats = [ Naive.sat; DPLL.sat ] in
  let cnfed =
    List.map
      (fun s ->
        let formula = Load.parse s in
        let sats = List.map (fun sat -> sat formula) sats in
        assert (List.for_all Fun.id sats || not (List.exists Fun.id sats));
        s ^ ": " ^ Bool.to_string (List.hd sats))
      cases
    |> String.concat "\n"
  in
  print_string cnfed;
  [%expect
    {|
      !(A /\ B): true
      !(A \/ B): true
      (A /\ B) \/ C: true
      A \/ (B /\ C): true
      A => B: true
      ¬(¬X ∨ Y) ∨ (¬Y ∨ Z): true
      A /\ !A : false |}]
