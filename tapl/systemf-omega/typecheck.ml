open Language

(*         *)
(* Kinding *)
(*         *)

let rec kindof ctx = function
  | TyVar (i, _) -> getkind ctx i
  | TyFn (ty1, ty2) ->
      expect_kindof KnStar ctx ty1;
      expect_kindof KnStar ctx ty2;
      KnStar
  | TyAll (tyParam, kn, tyBody) ->
      let ctx' = addbinding ctx tyParam (TyVarBind kn) in
      expect_kindof KnStar ctx' tyBody;
      KnStar
  | TyAbs (tyParam, kn, tyBody) ->
      let ctx' = addbinding ctx tyParam (TyVarBind kn) in
      let knBody = kindof ctx' tyBody in
      KnArrow (kn, knBody)
  | TyApp (tyAbs, tyApp) -> (
      let knAbs = kindof ctx tyAbs in
      match knAbs with
      | KnArrow (expKind, resKind) ->
          expect_kindof expKind ctx tyApp;
          resKind
      | _ -> failwith "Expected arrow kind" )

and expect_kindof what ctx ty =
  let real = kindof ctx ty in
  if real <> what then
    failwith
      (Printf.sprintf "Expected kind %s; found %s" (string_of_kind what)
         (string_of_kind real))

(*           *)
(* Typecheck *)
(*           *)

let rec simplify_ty ctx ty =
  let ty =
    match ty with
    | TyApp (ty1, ty2) -> TyApp (simplify_ty ctx ty1, ty2)
    | _ -> ty
  in
  match ty with
  | TyApp (TyAbs (_, _, tyBody), tyApp) ->
      type_subst_top tyApp tyBody |> simplify_ty ctx
  | _ -> ty

let rec tyeq ctx t1 t2 =
  let t1 = simplify_ty ctx t1 in
  let t2 = simplify_ty ctx t2 in
  match (t1, t2) with
  | TyVar (i, _), TyVar (j, _) -> i = j
  | TyFn (t11, t12), TyFn (t21, t22) | TyApp (t11, t12), TyApp (t21, t22) ->
      tyeq ctx t11 t21 && tyeq ctx t12 t22
  | TyAll (tyParam, kn1, tb1), TyAll (_, kn2, tb2)
  | TyAbs (tyParam, kn1, tb1), TyAbs (_, kn2, tb2) ->
      let ctx' = addname ctx tyParam in
      kn1 = kn2 && tyeq ctx' tb1 tb2
  | _ -> false

let rec typeof ctx = function
  | TmVar (_, i, _) -> gettype ctx i
  | TmAbs (_, param, ty, body) ->
      expect_kindof KnStar ctx ty;
      let ctx' = addbinding ctx param (VarBind ty) in
      let tyBody = typeof ctx' body in
      TyFn (ty, type_shift (-1) tyBody)
  | TmApp (_, left, right) -> (
      let tyL = typeof ctx left in
      let tyR = typeof ctx right in
      match simplify_ty ctx tyL with
      | TyFn (tyIn, tyOut) ->
          if tyeq ctx tyIn tyR then tyOut
          else
            failwith
              (Printf.sprintf "Mismatched parameter type; expected %s, found %s"
                 (string_of_ty ctx tyIn) (string_of_ty ctx tyR))
      | _ -> failwith "Function type expected" )
  | TmTyAbs (_, tyParam, kn, body) ->
      let ctx' = addbinding ctx tyParam (TyVarBind kn) in
      let tyBody = typeof ctx' body in
      (* body return type can be dependent on the tyParam. *)
      TyAll (tyParam, kn, tyBody)
  | TmTyApp (_, left, appTy) -> (
      let leftTy = typeof ctx left in
      let appTyKn = kindof ctx appTy in
      match simplify_ty ctx leftTy with
      | TyAll (_, kn, bodyTy) ->
          if kn = appTyKn then type_subst_top appTy bodyTy
          else failwith "Applied type has wrong kind"
      | _ -> failwith "Universal type expected" )
