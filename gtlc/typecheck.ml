open Language

let rec consistent t1 t2 =
  t1 = t2
  ||
  match (t1, t2) with
  | _, TUnknown | TUnknown, _ -> true
  | TArrow (s1, s2), TArrow (t1, t2) -> consistent s1 t1 && consistent s2 t2
  | _ -> false

let rec join t1 t2 =
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | _, TUnknown | TUnknown, _ -> TUnknown
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (meet s1 t1, join s2 t2)
    | _ -> failwith "inconsistent"

and meet t1 t2 =
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | t, TUnknown | TUnknown, t -> t
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (join s1 t1, meet s2 t2)
    | _ -> failwith "inconsistent"

let ( >>= ) = Result.bind

let rec elaborate ctx (Just e) =
  match e with
  | Nat n -> Ok (Elab (Nat n, TNat))
  | Bool b -> Ok (Elab (Bool b, TBool))
  | Var x -> (
      match List.assoc_opt x ctx with
      | Some t -> Ok (Elab (Var x, t))
      | None -> Error (x ^ " is not declared"))
  | App (e1, e2) -> (
      elaborate ctx e1 >>= fun (Elab (_, t1) as e1) ->
      elaborate ctx e2 >>= fun (Elab (_, t2) as e2) ->
      let app = App (e1, e2) in
      match t1 with
      | TUnknown -> Ok (Elab (app, TUnknown))
      | TArrow (t, t') ->
          if consistent t2 t then Ok (Elab (app, t'))
          else Error "Argument is not consistent with domain of application"
      | _ -> Error "Cannot apply argument to non-function type")
  | Lam (x, t, e) ->
      elaborate ((x, t) :: ctx) e >>= fun (Elab (_, t') as e) ->
      Ok (Elab (Lam (x, t, e), TArrow (t, t')))
  | If (c, thn, els) ->
      elaborate ctx c >>= fun (Elab (_, tc) as c) ->
      elaborate ctx thn >>= fun (Elab (_, t1) as thn) ->
      elaborate ctx els >>= fun (Elab (_, t2) as els) ->
      if consistent tc TBool then
        if consistent t1 t2 then Ok (Elab (If (c, thn, els), meet t1 t2))
        else Error "Branches differ in types"
      else Error "If condition is not a bool"
