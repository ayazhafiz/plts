open Language
open Util

let rec resolve_ty (Ty (t, i) as ty) =
  match t with
  | TNat | TBool | TUnknown -> ty
  | TInfer (`Var _) -> failwith "unexpected unresolved inference variable"
  | TInfer (`Resolved t) -> resolve_ty (ft t)
  | TRef t -> Ty (TRef (resolve_ty t), i)
  | TArrow (t1, t2) -> Ty (TArrow (resolve_ty t1, resolve_ty t2), i)

let rec consistent (Ty (t1, _)) (Ty (t2, _)) =
  t1 = t2
  ||
  match (t1, t2) with
  | _, TUnknown | TUnknown, _ -> true
  | TArrow (s1, s2), TArrow (t1, t2) -> consistent s1 t1 && consistent s2 t2
  | _ -> false

let rec join (Ty (t1, _) as tt) (Ty (t2, _)) =
  if t1 = t2 then tt
  else
    match (t1, t2) with
    | _, TUnknown | TUnknown, _ -> ft TUnknown
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (meet s1 t1, join s2 t2) |> ft
    | _ -> failwith "inconsistent"

and meet (Ty (t1, _) as tt) (Ty (t2, _)) =
  if t1 = t2 then tt
  else
    match (t1, t2) with
    | t, TUnknown | TUnknown, t -> ft t
    | TArrow (s1, s2), TArrow (t1, t2) -> TArrow (join s1 t1, meet s2 t2) |> ft
    | _ -> failwith "inconsistent"

let tiswap (Ty (_, i)) newt = Ty (newt, i)

let rt (Ty (t, _)) = t

let rec elaborate ctx (Just (e, i)) =
  match e with
  | Nat n -> Ok (Elab (Nat n, i, ft TNat))
  | Bool b -> Ok (Elab (Bool b, i, ft TBool))
  | Var (`Local x | `Global x) -> (
      match List.assoc_opt x ctx with
      | Some (t, true) -> Ok (Elab (Var (`Global x), i, t))
      | Some (t, false) -> Ok (Elab (Var (`Local x), i, t))
      | None -> Error (x ^ " is not declared"))
  | App (e1, e2, a) -> (
      elaborate ctx e1 >>= fun (Elab (_, _, t1) as e1) ->
      elaborate ctx e2 >>= fun (Elab (_, _, t2) as e2) ->
      let app = App (e1, e2, a) in
      match rt t1 with
      | TUnknown -> Ok (Elab (app, i, ft TUnknown))
      | TArrow (t, t') ->
          if consistent t2 t then Ok (Elab (app, i, t'))
          else Error "Argument is not consistent with domain of application"
      | _ -> Error "Cannot apply argument to non-function type")
  | Lam ((x, xi), t, e) ->
      (* Only place where we introduce possibly-inferred types. *)
      let t = resolve_ty t in
      elaborate ((x, (t, false)) :: ctx) e >>= fun (Elab (_, _, t') as e) ->
      let fnty = TArrow (t, t') |> ft in
      Ok (Elab (Lam ((x, xi), t, e), i, fnty))
  | If (c, thn, els) ->
      elaborate ctx c >>= fun (Elab (_, _, tc) as c) ->
      elaborate ctx thn >>= fun (Elab (_, _, t1) as thn) ->
      elaborate ctx els >>= fun (Elab (_, _, t2) as els) ->
      if consistent tc (ft TBool) then
        if consistent t1 t2 then Ok (Elab (If (c, thn, els), i, meet t1 t2))
        else Error "Branches differ in types"
      else Error "If condition is not a bool"
  | Ref e ->
      elaborate ctx e >>= fun (Elab (_, _, t) as e) ->
      Ok (Elab (Ref e, i, ft (TRef t)))
  | RefAssign (e1, e2) -> (
      elaborate ctx e1 >>= fun (Elab (_, _, t1) as e1) ->
      elaborate ctx e2 >>= fun (Elab (_, _, t2) as e2) ->
      match rt t1 with
      | TUnknown -> Ok (Elab (RefAssign (e1, e2), i, ft (TRef t2)))
      | TRef t when consistent t t2 ->
          Ok (Elab (RefAssign (e1, e2), i, ft (TRef t)))
      | _ -> Error "Cannot assign to mis-matched reference type")
  | Deref e -> (
      elaborate ctx e >>= fun (Elab (_, _, t) as e) ->
      match rt t with
      | TUnknown -> Ok (Elab (Deref e, i, ft TUnknown))
      | TRef t -> Ok (Elab (Deref e, i, t))
      | _ -> Error "Cannot derefence non-reference type")
