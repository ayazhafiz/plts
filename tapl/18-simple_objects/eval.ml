open Language

(* Reference store *)
type store = term list

let emptystore = []

let extend_store store v = (List.length store, store @ [ v ])

let lookup_store store loc = List.nth store loc

let update_store store loc v =
  let rec f toGo store =
    let rest = List.tl store in
    match toGo with 0 -> v :: rest | _ -> List.hd store :: f (toGo - 1) rest
  in
  f loc store

let shift_store store amt = List.map (fun t -> termShift amt t) store

let rec isnumeric t =
  match t with
  | TmZero _ -> true
  | TmSucc (_, term) -> isnumeric term
  | _ -> false

let rec isval ctx t =
  match t with
  | term when isnumeric term -> true
  | TmTrue _ | TmFalse _ | TmUnit _ | TmString _ | TmFloat _ | TmInert _
  | TmLoc _ ->
      true
  | TmVariant (_, _, term) -> isval ctx term
  | TmAbs (_, _, _, _) -> true
  | TmRecord (_, fields) ->
      List.for_all (fun (_, term) -> isval ctx term) fields
  | TmNil _ -> true
  | TmCons (_, t1, t2) when isval ctx t1 && isval ctx t2 -> true
  | _ -> false

exception NoRuleApplies

let rec eval' ctx store t =
  match t with
  | TmVar (info, name, _) -> (
      match getbinding info ctx name with
      | TmAbbBinding (term, _) -> (term, store)
      | _ -> raise NoRuleApplies )
  | TmApp (_, TmAbs (_, _, _, t12), v2) when isval ctx v2 ->
      (termSubstTop v2 t12, store)
  | TmApp (info, t1, t2) when isval ctx t1 ->
      let t2', store' = eval' ctx store t2 in
      (TmApp (info, t1, t2'), store')
  | TmApp (info, t1, t2) ->
      let t1', store' = eval' ctx store t1 in
      (TmApp (info, t1', t2), store')
  | TmIf (_, TmTrue _, thn, _) -> (thn, store)
  | TmIf (_, TmFalse _, _, els) -> (els, store)
  | TmIf (info, cond, thn, els) ->
      let cond', store' = eval' ctx store cond in
      (TmIf (info, cond', thn, els), store')
  | TmCase (_, TmVariant (_, varName, varVal), cases) ->
      let _, branch = List.assoc varName cases in
      (termSubstTop varVal branch, store)
  | TmCase (info, cond, cases) ->
      let cond', store' = eval' ctx store cond in
      (TmCase (info, cond', cases), store')
  | TmVariant (info, name, term) ->
      let term', store' = eval' ctx store term in
      (TmVariant (info, name, term'), store')
  | TmLet (_, _, nameVal, body) when isval ctx nameVal ->
      (termSubstTop nameVal body, store)
  | TmLet (info, name, nameVal, body) ->
      let nameVal', store' = eval' ctx store nameVal in
      (TmLet (info, name, nameVal', body), store')
  | TmFix (_, (TmAbs (_, _, _, body) as term)) as fixedPoint when isval ctx term
    ->
      (termSubstTop fixedPoint body, store)
  | TmFix (info, term) ->
      let term', store' = eval' ctx store term in
      (TmFix (info, term'), store')
  | TmAscribe (_, term, _) when isval ctx term -> (term, store)
  | TmAscribe (info, term, ty) ->
      let term', store' = eval' ctx store term in
      (TmAscribe (info, term', ty), store')
  | TmRecord (info, terms) ->
      let rec evalNextUnevaled fields =
        match fields with
        | [] -> raise NoRuleApplies
        | (name, term) :: rest when isval ctx term ->
            let rest', store' = evalNextUnevaled rest in
            ((name, term) :: rest', store')
        | (name, term) :: rest ->
            let term', store' = eval' ctx store term in
            ((name, term') :: rest, store')
      in
      let terms', store' = evalNextUnevaled terms in
      (TmRecord (info, terms'), store')
  | TmProj (_, (TmRecord (_, fields) as rcd), key) when isval ctx rcd ->
      (List.assoc key fields, store)
  | TmProj (info, rcd, key) ->
      let rcd', store' = eval' ctx store rcd in
      (TmProj (info, rcd', key), store')
  | TmTimesfloat (info, TmFloat (_, f1), TmFloat (_, f2)) ->
      (TmFloat (info, f1 *. f2), store)
  (* One step at a time: first lower f1 to a float, then f2. *)
  | TmTimesfloat (info, (TmFloat (_, _) as f1), f2) ->
      let f2', store' = eval' ctx store f2 in
      (TmTimesfloat (info, f1, f2'), store')
  | TmTimesfloat (info, f1, f2) ->
      let f1', store' = eval' ctx store f1 in
      (TmTimesfloat (info, f1', f2), store')
  | TmPlusfloat (info, TmFloat (_, f1), TmFloat (_, f2)) ->
      (TmFloat (info, f1 +. f2), store)
  (* One step at a time: first lower f1 to a float, then f2. *)
  | TmPlusfloat (info, (TmFloat (_, _) as f1), f2) ->
      let f2', store' = eval' ctx store f2 in
      (TmPlusfloat (info, f1, f2'), store')
  | TmPlusfloat (info, f1, f2) ->
      let f1', store' = eval' ctx store f1 in
      (TmPlusfloat (info, f1', f2), store')
  | TmSucc (_, TmPred (_, term)) when isnumeric term -> (term, store)
  | TmSucc (info, term) ->
      let term', store' = eval' ctx store term in
      (TmSucc (info, term'), store')
  | TmPred (_, TmSucc (_, term)) when isnumeric term -> (term, store)
  | TmPred (info, TmZero _) -> (TmZero info, store)
  | TmPred (info, term) ->
      let term', store' = eval' ctx store term in
      (TmPred (info, term'), store')
  | TmIsZero (info, TmZero _) -> (TmTrue info, store)
  | TmIsZero (info, TmSucc (_, term)) when isnumeric term ->
      (TmFalse info, store)
  | TmIsZero (info, term) ->
      let term', store' = eval' ctx store term in
      (TmIsZero (info, term'), store')
  | TmCons (info, t1, t2) when isval ctx t1 ->
      let t2', store' = eval' ctx store t2 in
      (TmCons (info, t1, t2'), store')
  | TmCons (info, t1, t2) ->
      let t1', store' = eval' ctx store t1 in
      (TmCons (info, t1', t2), store')
  | TmIsNil (info, TmNil _) -> (TmTrue info, store)
  | TmIsNil (info, TmCons (_, _, _)) -> (TmFalse info, store)
  | TmIsNil (info, term) ->
      let term', store' = eval' ctx store term in
      (TmIsNil (info, term'), store')
  | TmHead (_, TmCons (_, t1, _rest)) -> (t1, store)
  | TmHead (info, term) ->
      let term', store' = eval' ctx store term in
      (TmHead (info, term'), store')
  | TmTail (_, TmCons (_, _t1, rest)) -> (rest, store)
  | TmTail (info, term) ->
      let term', store' = eval' ctx store term in
      (TmTail (info, term'), store')
  | TmRef (info, value) when isval ctx value ->
      let where, store' = extend_store store value in
      (TmLoc (info, where), store')
  | TmRef (info, value) ->
      let value', store' = eval' ctx store value in
      (TmRef (info, value'), store')
  | TmDeref (_, TmLoc (_, where)) -> (lookup_store store where, store)
  | TmDeref (info, term) ->
      let term', store' = eval' ctx store term in
      (TmDeref (info, term'), store')
  | TmRefAssign (info, TmLoc (_, where), term) when isval ctx term ->
      let store' = update_store store where term in
      (TmUnit info, store')
  (* One step at a time: first lower location, then the value. *)
  | TmRefAssign (info, (TmLoc _ as refLoc), refVal) ->
      let refVal', store' = eval' ctx store refVal in
      (TmRefAssign (info, refLoc, refVal'), store')
  | TmRefAssign (info, refLoc, refVal) ->
      let refLoc', store' = eval' ctx store refLoc in
      (TmRefAssign (info, refLoc', refVal), store')
  | _ -> raise NoRuleApplies

let rec eval ctx store t =
  try
    let t', store' = eval' ctx store t in
    eval ctx store' t'
  with NoRuleApplies -> (t, store)

let evalbinding ctx store binding =
  match binding with
  | TmAbbBinding (t, ty) ->
      let term', store' = eval ctx store t in
      (TmAbbBinding (term', ty), store')
  | b -> (b, store)
