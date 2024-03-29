include Ast

exception Free_var

let rec eval = function
  | Var _ -> raise Free_var
  | Neg a -> not (eval a)
  | Conj (a, b) -> eval a && eval b
  | Disj (a, b) -> eval a || eval b
  | Imp (a, b) -> (not (eval a)) || eval b
  | Top -> true
  | Bot -> false

let if_none f = function Some a -> Some a | None -> f

let rec free = function
  | Var x -> Some x
  | Neg a -> free a
  | Conj (a, b) | Disj (a, b) | Imp (a, b) -> free a |> if_none (free b)
  | Top | Bot -> None

let conj_list ?(first = Top) lst =
  List.fold_left (fun a b -> Conj (a, b)) first lst

let parse = Load.parse
let parse_safe = Load.parse_safe
