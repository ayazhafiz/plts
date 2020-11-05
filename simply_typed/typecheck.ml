open Language
open Util.Error

let rec typeof ctx t =
  match t with
  | TmVar (info, i, _) -> gettype info ctx i
  | TmAbs (_, param, paramTy, def) ->
      let ctx' = addbinding ctx param (VarBinding paramTy) in
      TyArrow (paramTy, typeof ctx' def)
  | TmApp (info, t1, t2) -> (
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match tyT1 with
      | TyArrow (tyT11, tyT12) ->
          if tyT2 = tyT11 then tyT12 else error info "parameter type mismatch!"
      | TyBool -> error info "arrow type expected!" )
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmIf (info, cond, thn, els) ->
      if typeof ctx cond = TyBool then
        let tyThn = typeof ctx thn in
        if tyThn = typeof ctx els then tyThn
        else error info "\"if\" branches have different types!"
      else error info "\"if\" condition not a boolean!"
