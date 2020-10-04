(** Grammar *)

type expression =
  | Variable of string
  | Numeral of int
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Divide of expression * expression
  | Negate of expression

type statement =
  | Expression of expression  (** just an expression evaluation *)
  | Definition of string * expression  (** variable definition *)

let expr2str e =
  let rec to_str n e =
    let m, str =
      match e with
      | Variable x -> (3, x)
      | Numeral i -> (3, string_of_int i)
      | Negate e -> (2, "-" ^ to_str 2 e)
      | Times (l, r) -> (1, to_str 1 l ^ " * " ^ to_str 1 r)
      | Divide (l, r) -> (1, to_str 1 l ^ " / " ^ to_str 1 r)
      | Plus (l, r) -> (0, to_str 0 l ^ " + " ^ to_str 0 r)
      | Minus (l, r) -> (0, to_str 0 l ^ " - " ^ to_str 0 r)
    in
    if m < n then "(" ^ str ^ ")" else str
  in
  to_str (-1) e
