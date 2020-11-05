open Language

let isval _ctx t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_, _, _, _) -> true
  | _ -> false

exception NoRuleApplies

let rec eval' ctx t =
  match t with
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
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval' ctx t in
    eval ctx t'
  with NoRuleApplies -> t
