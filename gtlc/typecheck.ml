open Language
open Util

let rec resolve_ty = function
  | TInfer (`Var _) -> failwith "unexpected unresolved inference variable"
  | TInfer (`Resolved t) -> resolve_ty t
  | TNat -> TNat
  | TBool -> TBool
  | TUnknown -> TUnknown
  | TArrow (t1, t2) -> TArrow (resolve_ty t1, resolve_ty t2)

let rec consistent t1 t2 =
  let t1, t2 = (resolve_ty t1, resolve_ty t2) in
  t1 = t2
  ||
  match (t1, t2) with
  | _, TUnknown | TUnknown, _ -> true
  | TArrow (s1, s2), TArrow (t1, t2) -> consistent s1 t1 && consistent s2 t2
  | _ -> false

let rec join t1 t2 =
  let t1, t2 = (resolve_ty t1, resolve_ty t2) in
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | _, TUnknown | TUnknown, _ -> TUnknown
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (meet s1 t1, join s2 t2)
    | _ -> failwith "inconsistent"

and meet t1 t2 =
  let t1, t2 = (resolve_ty t1, resolve_ty t2) in
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | t, TUnknown | TUnknown, t -> t
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (join s1 t1, meet s2 t2)
    | _ -> failwith "inconsistent"

let rec elaborate ctx (Just e) =
  match e with
  | Nat n -> Ok (Elab (Nat n, TNat))
  | Bool b -> Ok (Elab (Bool b, TBool))
  | Var (`Local x | `Global x) -> (
      match List.assoc_opt x ctx with
      | Some (t, true) -> Ok (Elab (Var (`Global x), t))
      | Some (t, false) -> Ok (Elab (Var (`Local x), t))
      | None -> Error (x ^ " is not declared"))
  | App (e1, e2, a) -> (
      elaborate ctx e1 >>= fun (Elab (_, t1) as e1) ->
      elaborate ctx e2 >>= fun (Elab (_, t2) as e2) ->
      let app = App (e1, e2, a) in
      match resolve_ty t1 with
      | TUnknown -> Ok (Elab (app, TUnknown))
      | TArrow (t, t') ->
          if consistent t2 t then Ok (Elab (app, t'))
          else Error "Argument is not consistent with domain of application"
      | _ -> Error "Cannot apply argument to non-function type")
  | Lam (x, t, e) ->
      elaborate ((x, (t, false)) :: ctx) e >>= fun (Elab (_, t') as e) ->
      Ok (Elab (Lam (x, t, e), TArrow (t, t')))
  | If (c, thn, els) ->
      elaborate ctx c >>= fun (Elab (_, tc) as c) ->
      elaborate ctx thn >>= fun (Elab (_, t1) as thn) ->
      elaborate ctx els >>= fun (Elab (_, t2) as els) ->
      if consistent tc TBool then
        if consistent t1 t2 then Ok (Elab (If (c, thn, els), meet t1 t2))
        else Error "Branches differ in types"
      else Error "If condition is not a bool"
