type variable = Syntax.variable

type polarity = bool
(** true = positive, false = negative *)

(** By DeMorgan, all formulas of the surface syntax [Syntax.formula] are
    equivalent to forms wherein only variables are negated, and are negated at
    most once.
    Also, desugar implications A => B to conjunctions !A \/ B. *)
type formula =
  | Var of polarity * variable
  | Conj of formula * formula
  | Disj of formula * formula
  | Top
  | Bot

let rec desugar =
  let rec go pol =
    let module S = Syntax in
    function
    | S.Var x -> Var (pol, x)
    | S.Neg a -> go (not pol) a
    | S.Conj (a, b) -> Conj (go pol a, go pol b)
    | S.Disj (a, b) -> Disj (go pol a, go pol b)
    | S.Imp (a, b) -> Conj (go (not pol) a, go pol b)
    | S.Top -> if pol then Top else Bot
    | S.Bot -> if pol then Bot else Top
  in
  go true

(** Negation of a formula. *)
let rec neg = function
  | Var (p, x) -> Var (not p, x)
  | Conj (a, b) -> Disj (neg a, neg b)
  | Disj (a, b) -> Conj (neg a, neg b)
  | Top -> Bot
  | Bot -> Top

(** Proof search for formulas in the classically-derived sequent calculus, using
    a single-sided representation (where the LHS context is empty, ⊢ ∆). *)
let rec prove' venv = function
  | [] -> false
  | a :: ctx -> (
      match a with
      | Var (p, x) ->
          (* --------------
             ⊢ A, ∆, A*, ∆'
          *)
          List.mem (Var (not p, x)) venv (* rule applies *)
          || (* save A to the context, we might see A* and apply this rule later *)
          prove' (Var (p, x) :: venv) ctx
      | Conj (a, b) ->
          (* ⊢ A, ∆   ⊢ B, ∆
             ---------------
               ⊢ A ∧ B, ∆
          *)
          prove' venv (a :: ctx) && prove' venv (b :: ctx)
      | Disj (a, b) ->
          (* ⊢ A, B, ∆
             ----------
             ⊢ A ∨ B, ∆
          *)
          prove' venv (a :: b :: ctx)
      | Bot ->
          (* ⊢ ∆
             ----------
             ⊢ ⊥, ∆
          *)
          prove' venv ctx
      | Top ->
          (* ----------
             ⊢ ⊤, ∆
          *)
          true)

let prove a = prove' [] a
