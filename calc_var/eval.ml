(** Calculator with variables. *)

open Grammar

let eval env =
  let rec eval = function
    | Variable x -> (
        try List.assoc x env
        with Not_found -> Zoo.error "unknown variable %s" x )
    | Numeral n -> n
    | Plus (l, r) -> eval l + eval r
    | Minus (l, r) -> eval l - eval r
    | Times (l, r) -> eval l * eval r
    | Divide (l, r) ->
        let nr = eval r in
        if nr <> 0 then eval l / nr else Zoo.error "division by zero"
    | Negate e -> -eval e
  in
  eval
