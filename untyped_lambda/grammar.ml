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

(** Composes explicit subtitutions [s] and [t],
    i.e., we have [subst (compose s t) e = subst s (subst t e)]. *)
let rec compose s t =
  match (s, t) with
  | s, Shift 0 -> s
  | Dot (_, s), Shift m -> compose s (Shift (m - 1))
  | Shift m, Shift n -> Shift (m + n)
  | s, Dot (e, s') -> Dot (mk_subst s e, compose s s')

(** Applies the explicit substitution [s] to some expression [e].
    This is done lazily; i.e. just enough to expose the outermost constructor of [e]. *)
let subst =
  let rec subst s { Zoo.data = e'; loc } =
    match (s, e') with
    | Shift m, Var k -> Zoo.locate ~loc (Var (k + m))
    | Dot (a, _), Var 0 -> a
    | Dot (_, s), Var k -> subst s (Zoo.locate ~loc (Var (k - 1)))
    | s, Subst (t, e) -> subst s (subst t e)
    | s, Lambda (x, e) ->
        let e = mk_subst (Dot (mk_var 0, compose (Shift 1) s)) e in
        Zoo.locate ~loc (Lambda (x, e))
    | s, App (e1, e2) -> Zoo.locate ~loc (App (mk_subst s e1, mk_subst s e2))
  in
  subst

(** Returns [true] when [Var k] occurs freely in [e]. *)
let rec occurs_freely k { Zoo.data = e; _ } =
  match e with
  | Var m -> m = k
  | Subst (s, e) -> occurs_freely k (subst s e)
  | Lambda (_, e) -> occurs_freely (k + 1) e
  | App (e1, e2) -> occurs_freely k e1 || occurs_freely k e2

(** Returns [true] when [e1] and [e2] are alpha-equivalent; i.e. up to
      renaming of bound variables. Basically, we want to check that all free
      variables are equivalent, and that bound variables are of the same form,
      but we don't care what the bound variables actualy _are_. *)
let alpha_equiv =
  let rec equal e1 e2 =
    match (e1.Zoo.data, e2.Zoo.data) with
    | Var k, Var m -> k = m
    | Subst (s, e), _ -> equal (subst s e) e2
    | _, Subst (s, e) -> equal e1 (subst s e)
    | Lambda (_, e1), Lambda (_, e2) -> equal e1 e2
    | App (e11, e12), App (e21, e22) -> equal e11 e21 && equal e12 e22
    | _, _ -> false
  in
  equal
