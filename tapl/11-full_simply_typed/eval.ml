open Language

let rec isnumeric t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, term) -> isnumeric term
  | _ -> false

let rec isval ctx t =
  match t with
  | term when isnumeric term -> true
  | TmTrue _ | TmFalse _ | TmUnit _ | TmString _ | TmFloat _ | TmInert _ -> true
  | TmTag (_, _, term, _) -> isval ctx term
  | TmAbs (_, _, _, _) -> true
  | TmRecord (_, fields) ->
      List.for_all (fun (_, term) -> isval ctx term) fields
  | TmNil _ -> true
  | TmCons (_, t1, t2) when isval ctx t1 && isval ctx t2 -> true
  | _ -> false

exception NoRuleApplies

let rec eval' ctx t =
  match t with
  | TmVar (info, name, _) -> (
      match getbinding info ctx name with
      | TmAbbBinding (term, _) -> term
      | _ -> raise NoRuleApplies )
  | TmApp (_, TmAbs (_, _, _, t12), v2) when isval ctx v2 -> termSubstTop v2 t12
  | TmApp (info, t1, t2) when isval ctx t1 ->
      let t2' = eval' ctx t2 in
      TmApp (info, t1, t2')
  | TmApp (info, t1, t2) ->
      let t1' = eval' ctx t1 in
      TmApp (info, t1', t2)
  | TmIf (_, TmTrue _, thn, _) -> thn
  | TmIf (_, TmFalse _, _, els) -> els
  | TmIf (info, cond, thn, els) ->
      let cond' = eval' ctx cond in
      TmIf (info, cond', thn, els)
  | TmCase (_, TmTag (_, varName, varVal, _), cases) ->
      let _, branch = List.assoc varName cases in
      termSubstTop varVal branch
  | TmCase (info, cond, cases) -> TmCase (info, eval' ctx cond, cases)
  | TmTag (info, name, term, ty) -> TmTag (info, name, eval' ctx term, ty)
  | TmLet (_, _, nameVal, body) -> termSubstTop nameVal body
  | TmFix (_, (TmAbs (_, _, _, body) as term)) as fixedPoint when isval ctx term
    ->
      termSubstTop fixedPoint body
  | TmFix (info, term) -> TmFix (info, eval' ctx term)
  | TmAscribe (_, term, _) when isval ctx term -> term
  | TmAscribe (info, term, ty) -> TmAscribe (info, eval' ctx term, ty)
  | TmRecord (info, terms) ->
      let rec evalNextUnevaled fields =
        match fields with
        | [] -> raise NoRuleApplies
        | (name, term) :: rest when isval ctx term ->
            (name, term) :: evalNextUnevaled rest
        | (name, term) :: rest -> (name, eval' ctx term) :: rest
      in
      TmRecord (info, evalNextUnevaled terms)
  | TmProj (_, (TmRecord (_, fields) as rcd), key) when isval ctx rcd ->
      List.assoc key fields
  | TmProj (info, rcd, key) -> TmProj (info, eval' ctx rcd, key)
  | TmTimesfloat (info, TmFloat (_, f1), TmFloat (_, f2)) ->
      TmFloat (info, f1 *. f2)
  (* One step at a time: first lower f1 to a float, then f2. *)
  | TmTimesfloat (info, (TmFloat (_, _) as f1), f2) ->
      TmTimesfloat (info, f1, eval' ctx f2)
  | TmTimesfloat (info, f1, f2) -> TmTimesfloat (info, eval' ctx f1, f2)
  | TmPlusfloat (info, TmFloat (_, f1), TmFloat (_, f2)) ->
      TmFloat (info, f1 +. f2)
  (* One step at a time: first lower f1 to a float, then f2. *)
  | TmPlusfloat (info, (TmFloat (_, _) as f1), f2) ->
      TmPlusfloat (info, f1, eval' ctx f2)
  | TmPlusfloat (info, f1, f2) -> TmPlusfloat (info, eval' ctx f1, f2)
  | TmSucc (_, TmPred (_, term)) when isnumeric term -> term
  | TmSucc (info, term) -> TmSucc (info, eval' ctx term)
  | TmPred (_, TmSucc (_, term)) when isnumeric term -> term
  | TmPred (info, TmZero _) -> TmZero info
  | TmPred (info, term) -> TmPred (info, eval' ctx term)
  | TmIsZero (info, TmZero _) -> TmTrue info
  | TmIsZero (info, TmSucc (_, term)) when isnumeric term -> TmFalse info
  | TmIsZero (info, term) -> TmIsZero (info, eval' ctx term)
  | TmCons (info, t1, t2) when isval ctx t1 -> TmCons (info, t1, eval' ctx t2)
  | TmCons (info, t1, t2) -> TmCons (info, eval' ctx t1, t2)
  | TmIsNil (info, TmNil _) -> TmTrue info
  | TmIsNil (info, TmCons (_, _, _)) -> TmFalse info
  | TmIsNil (info, term) -> TmIsNil (info, eval' ctx term)
  | TmHead (_, TmCons (_, t1, _rest)) -> t1
  | TmHead (info, term) -> TmHead (info, eval' ctx term)
  | TmTail (_, TmCons (_, _t1, rest)) -> rest
  | TmTail (info, term) -> TmTail (info, eval' ctx term)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval' ctx t in
    eval ctx t'
  with NoRuleApplies -> t

let evalbinding ctx binding =
  match binding with
  | TmAbbBinding (t, ty) -> TmAbbBinding (eval ctx t, ty)
  | b -> b
