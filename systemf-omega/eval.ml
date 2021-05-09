open Language

let emptystore = []

let shift_store store amt = List.map (fun t -> term_shift amt t) store

exception No_rule_applies

let is_val = function
  | TmAbs (_, _, _, _) -> true
  | TmTyAbs (_, _, _, _) -> true
  | _ -> false

let rec small_step ctx = function
  | TmApp (_, TmAbs (_, _, _, body), appV) when is_val appV ->
      term_subst_top appV body
  | TmApp (fi, v1, tm) when is_val v1 -> TmApp (fi, v1, small_step ctx tm)
  | TmApp (fi, t1, t2) -> TmApp (fi, small_step ctx t1, t2)
  | TmTyApp (_, TmTyAbs (_, _, _, body), ty) -> tyterm_subst_top ty body
  | TmTyApp (fi, tm, ty) -> TmTyApp (fi, small_step ctx tm, ty)
  | _ -> raise No_rule_applies

let rec eval ctx store t =
  try
    let t' = small_step ctx t in
    eval ctx store t'
  with No_rule_applies -> (t, store)
