open Language

exception NoRuleApplies

let isval _ctx t = match t with TmAbs (_, _, _) -> true | _ -> false

let rec eval' ctx t =
  match t with
  | TmApp (_, TmAbs (_, _, t1), t2) when isval ctx t2 -> termSubstTop t1 t2
  | TmApp (info, t1, t2) when isval ctx t1 -> TmApp (info, t1, eval' ctx t2)
  | TmApp (info, t1, t2) -> TmApp (info, eval' ctx t1, t2)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval' ctx t in
    eval ctx t'
  with NoRuleApplies -> t
