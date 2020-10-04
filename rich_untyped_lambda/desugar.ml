(** Desugars input grammars to the language IR (namely, using de Bruijn indices). *)

let index ~loc x =
  let rec index k = function
    (* This path happens if the variable is free and not a known constant. *)
    | [] -> Zoo.error ~loc "unknown identifier %s" x
    | y :: ys -> if x = y then k else index (k + 1) ys
  in
  index 0

(** Converts an expression of type [Input.expr] to a [Grammar.expr] by replacing
    names in [e] with de Bruijn indices. [xs] is the list of names already in
    scope. *)
let rec expr xs { Zoo.data = e; loc } =
  Zoo.locate ~loc
    ( match e with
    | Input.Var x -> Grammar.Var (index ~loc x xs)
    | Input.Lambda (x, e) -> Grammar.Lambda (x, expr (x :: xs) e)
    | Input.App (e1, e2) -> Grammar.App (expr xs e1, expr xs e2) )
