open Language

(*         *)
(* Kinding *)
(*         *)

exception No_rule_applies

let simplify_ty _ctx ty = ty

let rec whnf_small_step ctx = function
  | TmApp (_, TmAbs (_, _, _, body), appV) -> term_subst_top appV body
  | TmApp (fi, t1, t2) -> TmApp (fi, whnf_small_step ctx t1, t2)
  | _ -> raise No_rule_applies

let rec whnf ctx t =
  try
    let t' = whnf_small_step ctx t in
    whnf ctx t'
  with No_rule_applies -> t

let rec wh_tmeq ctx tm1 tm2 =
  let tm1 = whnf ctx tm1 in
  let tm2 = whnf ctx tm2 in
  match (tm1, tm2) with
  | TmVar (_, i, _), TmVar (_, j, _) -> i = j
  | TmAbs (_, p, pTy1, body1), TmAbs (_, _, pTy2, body2) when tyeq ctx pTy1 pTy2
    ->
      let ctx' = addbinding ctx p (VarBind pTy1) in
      wh_tmeq ctx' body1 body2
  | TmApp (_, t11, t12), TmApp (_, t21, t22) ->
      wh_tmeq ctx t11 t21 && wh_tmeq ctx t12 t22
  | s, TmAbs (_, p, pTy, body) | TmAbs (_, p, pTy, body), s ->
      let ctx' = addbinding ctx p (VarBind pTy) in
      wh_tmeq ctx'
        (TmApp (dummyinfo, s, TmVar (dummyinfo, 0, ctxlength ctx')))
        body
  | _ -> false

and tyeq ctx t1 t2 =
  let t1 = simplify_ty ctx t1 in
  let t2 = simplify_ty ctx t2 in
  match (t1, t2) with
  | TyVar (i, _), TyVar (j, _) -> i = j
  | TyPi (v, vTy1, rTy1), TyPi (_, vTy2, rTy2) ->
      tyeq ctx vTy1 vTy2
      &&
      let ctx' = addbinding ctx v (VarBind vTy1) in
      tyeq ctx' rTy1 rTy2
  | TyPiApp (ty1, tm1), TyPiApp (ty2, tm2) ->
      tyeq ctx ty1 ty2 && wh_tmeq ctx tm1 tm2
  | _ -> false

let rec kindeq ctx kn1 kn2 =
  match (kn1, kn2) with
  | KnStar, KnStar -> true
  | KnPi (v, vTy1, rKn1), KnPi (_, vTy2, rKn2) ->
      tyeq ctx vTy1 vTy2
      &&
      let ctx' = addbinding ctx v (VarBind vTy1) in
      kindeq ctx' rKn1 rKn2
  | _ -> false

let rec typeof ctx = function
  | TmVar (_, i, _) -> gettype ctx i
  | TmAbs (_, param, pTy, body) ->
      expect_kindof KnStar ctx pTy;
      let ctx' = addbinding ctx param (VarBind pTy) in
      let tyBody = typeof ctx' body in
      TyPi (param, pTy, tyBody)
  | TmApp (_, left, right) -> (
      let tyL = typeof ctx left in
      let tyR = typeof ctx right in
      match simplify_ty ctx tyL with
      | TyPi (var, varTy, resTy) ->
          if tyeq ctx varTy tyR then termty_subst_top right resTy
          else
            failwith
              (Printf.sprintf "Mismatched parameter type; expected %s, found %s"
                 (string_of_ty ctx varTy)
                 (string_of_ty (addbinding ctx var (VarBind varTy)) tyR))
      | _ -> failwith "Function type expected" )

and kindof ctx = function
  | TyVar (i, _) -> getkind ctx i
  | TyPi (var, varTy, rTy) ->
      expect_kindof KnStar ctx varTy;
      let ctx' = addbinding ctx var (VarBind varTy) in
      expect_kindof KnStar ctx' rTy;
      KnStar
  | TyPiApp (tyFam, tmApp) -> (
      match kindof ctx tyFam with
      | KnPi (_, varTy, rhsKn) ->
          let tyTmApp = typeof ctx tmApp in
          if tyeq ctx varTy tyTmApp then kind_subst_top tmApp rhsKn
          else
            failwith
              (Printf.sprintf
                 "Mismatched type family application; expected %s, found %s"
                 (string_of_ty ctx varTy) (string_of_ty ctx tyTmApp))
      | _ -> failwith "Expected type family kinding" )

and expect_kindof what ctx ty =
  let real = kindof ctx ty in
  if real <> what then
    failwith
      (Printf.sprintf "Expected kind %s; found %s" (string_of_kind ctx what)
         (string_of_kind ctx real))
