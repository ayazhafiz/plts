type variable = string

type formula =
  | Var of variable
  | Neg of formula
  | Conj of formula * formula
  | Disj of formula * formula
  | Imp of formula * formula
  | Top
  | Bot
