(** Lambda calculus grammar. *)

type term = term' Zoo.located
(** Lambda terms, with use of de Bruijn indices rather than formal names to
    represent variables. *)

and term' =
  | Var of int
  | Subst of substitution * term
  | Lambda of string * term
  | App of term * term

and substitution = Shift of int | Dot of term * substitution

let mk_var k = Zoo.locate (Var k)

let mk_subst s e = Zoo.locate (Subst (s, e))

let mk_lambda x e = Zoo.locate (Lambda (x, e))

let mk_app e1 e2 = Zoo.locate (App (e1, e2))

(** Identity substitution (no shift). *)
let id_subst = Shift 0

(** Shifts the indices in [e] by [k] places. *)
let shift k e = mk_subst (Shift k) e

(** Composes explicit subtitutions [s] and [t], i.e., we have [subst (compose s t) e = subst s (subst t e)]. *)
let rec compose s t =
  match (s, t) with
  | s, Shift 0 -> s
  | Dot (_, s), Shift m -> compose s (Shift (m - 1))
  | Shift m, Shift n -> Shift (m + n)
  | s, Dot (e, s') -> Dot (mk_subst s e, compose s s')
