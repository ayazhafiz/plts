type variable = string

type formula =
  | Var of variable
  | Neg of formula
  | Conj of formula * formula
  | Disj of formula * formula
  | Imp of formula * formula
  | Top
  | Bot

exception Free_var

let rec eval = function
  | Var _ -> raise Free_var
  | Neg a -> not (eval a)
  | Conj (a, b) -> eval a && eval b
  | Disj (a, b) -> eval a || eval b
  | Imp (a, b) -> (not (eval a)) || eval b
  | Top -> true
  | Bot -> false
