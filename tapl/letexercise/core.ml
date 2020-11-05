open Syntax
open Support.Error

(* ------------------------   EVALUATION  ------------------------ *)

let isval _ t =
  match t with
  | TmTrue _ -> true
  | TmFalse _ -> true
  | TmAbs (_, _, _, _) -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx t =
  match t with
  | TmApp (_, TmAbs (_, _, _, t12), v2) when isval ctx v2 -> termSubstTop v2 t12
  | TmLet (info, letName, value, inTerm) ->
      if isval ctx value then termSubstTop value inTerm
      else
        let value' = eval1 ctx value in
        TmLet (info, letName, value', inTerm)
  | TmApp (fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (fi, v1, t2')
  | TmApp (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (fi, t1', t2)
  | TmIf (_, TmTrue _, t2, _) -> t2
  | TmIf (_, TmFalse _, _, t3) -> t3
  | TmIf (fi, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (fi, t1', t2, t3)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmLet (_, name, lhs, rhs) ->
      let tyRhs = typeof ctx lhs in
      let ctx' = addbinding ctx name (VarBind tyRhs) in
      typeof ctx' rhs
  | TmAbs (_, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)
  | TmApp (fi, t1, t2) -> (
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match tyT1 with
      | TyArr (tyT11, tyT12) ->
          if tyT2 = tyT11 then tyT12 else error fi "parameter type mismatch"
      | _ -> error fi "arrow type expected" )
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (fi, t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if tyT2 = typeof ctx t3 then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
