open Language

exception NoRuleApplies

let isval _ctx t = match t with TmAbs (_, _, _) -> true | _ -> false

let rec eval' ctx t =
  match t with
  | TmApp (_, TmAbs (_, _, t12), v2) when isval ctx v2 -> termSubstTop v2 t12
  | TmApp (info, t1, t2) when isval ctx t1 ->
      let t2' = eval' ctx t2 in
      TmApp (info, t1, t2')
  | TmApp (info, t1, t2) ->
      let t1' = eval' ctx t1 in
      TmApp (info, t1', t2)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval' ctx t in
    eval ctx t'
  with NoRuleApplies -> t
