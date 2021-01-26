(** Module [typecheck] performs type inference (including constraint) of a
    user-supplied program. This is the "typechecking frontend"; i.e. work that
    must be done before we can begin type simplification. *)

open Language
open Print

let tyvar_of_uid uid = TyVar ("'fresh" ^ string_of_int uid)

let coalesce ty =
  let recursives = ref [] in
  let rec coalesce inProcess ty isPos =
    match ty with
    | STyPrim n -> TyPrim n
    | STyFn (pTy, rTy) ->
        let pTy = coalesce inProcess pTy (not isPos) in
        let rTy = coalesce inProcess rTy isPos in
        TyFn (pTy, rTy)
    | STyRecord fields ->
        TyRecord
          (List.map (fun (f, ty) -> (f, coalesce inProcess ty isPos)) fields)
    | STyVar vs ->
        let polarV = if isPos then Positive vs else Negative vs in
        if List.mem polarV inProcess then (
          match List.assoc_opt polarV !recursives with
          | Some recursive -> recursive
          | None ->
              let freshTy = tyvar_of_uid vs.uid in
              recursives := (polarV, freshTy) :: !recursives;
              freshTy )
        else
          let inProcess = polarV :: inProcess in
          let bounds = if isPos then vs.lower_bounds else vs.upper_bounds in
          let boundTys =
            List.map (fun ty -> coalesce inProcess ty isPos) bounds
          in
          List.fold_left
            (fun all cur ->
              if isPos then TyUnion (all, cur) else TyIntersection (all, cur))
            (tyvar_of_uid vs.uid) boundTys
  in
  coalesce [] ty true

module Ctx = struct
  include Map.Make (String)
end

let freshVar =
  let uid = ref 0 in
  fun level ->
    uid := !uid + 1;
    { uid = !uid; level; lower_bounds = []; upper_bounds = [] }

let h { uid; _ } = uid

let rec level = function
  | STyVar vs -> vs.level
  | STyPrim _ -> 0
  | STyFn (p, r) -> max (level p) (level r)
  | STyRecord fields ->
      List.fold_left max 0 (List.map (fun (_, t) -> level t) fields)

let instantiate atLevel (PolyTy (limit, body)) =
  let freshened = Hashtbl.create 16 in
  let rec go = function
    | t when level t <= limit -> t
    | STyPrim _ as t -> t
    | STyFn (p, r) -> STyFn (go p, go r)
    | STyRecord fields -> STyRecord (List.map (fun (f, t) -> (f, go t)) fields)
    | STyVar vs when Hashtbl.mem freshened (h vs) ->
        STyVar (Hashtbl.find freshened (h vs))
    | STyVar vs ->
        let fresh = freshVar atLevel in
        Hashtbl.add freshened (h vs) fresh;
        fresh.lower_bounds <-
          List.rev vs.lower_bounds |> List.map go |> List.rev;
        fresh.upper_bounds <-
          List.rev vs.upper_bounds |> List.map go |> List.rev;
        STyVar fresh
  in
  go body

let extrude ty atLevel =
  let cache = Hashtbl.create 16 in
  let rec go = function
    | _ when level ty <= atLevel -> ty
    | STyPrim _ -> ty
    | STyFn (p, r) -> STyFn (go p, go r)
    | STyRecord fields ->
        STyRecord (List.map (function f, t -> (f, go t)) fields)
    | STyVar vs when Hashtbl.mem cache (h vs) ->
        STyVar (Hashtbl.find cache (h vs))
    | STyVar vs ->
        let nvs = freshVar atLevel in
        Hashtbl.add cache (h vs) nvs;
        nvs.lower_bounds <- List.map go vs.lower_bounds;
        nvs.upper_bounds <- List.map go vs.upper_bounds;
        vs.lower_bounds <- STyVar nvs :: vs.lower_bounds;
        vs.upper_bounds <- STyVar nvs :: vs.upper_bounds;
        STyVar nvs
  in
  go ty

let constrain s t =
  let rec constr seen s t =
    if List.mem (s, t) seen then ()
    else
      let seen = (s, t) :: seen in
      match (s, t) with
      | STyPrim m, STyPrim n when m = n -> ()
      | STyFn (pS, rS), STyFn (pT, rT) ->
          constr seen pT pS;
          constr seen rS rT
      | STyRecord fS, STyRecord fT ->
          List.iter
            (fun (field, tyT) ->
              match List.assoc_opt field fS with
              | Some tyS -> constr seen tyS tyT
              | None ->
                  failwith
                    (Printf.sprintf "no field \"%s\" in %s" field
                       (string_of_ty (coalesce s))))
            fT
      | STyVar sVs, t when level t <= level s ->
          sVs.upper_bounds <- t :: sVs.upper_bounds;
          List.iter (fun s -> constr seen s t) sVs.lower_bounds
      | s, STyVar rVs when level s <= level t ->
          rVs.lower_bounds <- s :: rVs.lower_bounds;
          List.iter (constr seen s) rVs.upper_bounds
      | STyVar _, t ->
          let tF = extrude t (level s) in
          constr seen s tF
      | s, STyVar _ ->
          let lhs = extrude s (level t) in
          constr seen lhs t
      | _ ->
          failwith
            (Printf.sprintf "cannot constrain %s <: %s"
               (string_of_ty (coalesce s))
               (string_of_ty (coalesce t)))
  in
  constr [] s t

let rec typeLetRhs ctx level is_rec name rhs =
  let rhsTy =
    if is_rec then (
      let exp = STyVar (freshVar (level + 1)) in
      let ctx1 = Ctx.add name (`S exp) ctx in
      let inf = typeTerm ctx1 (level + 1) rhs in
      constrain inf exp;
      exp )
    else typeTerm ctx (level + 1) rhs
  in
  `P (PolyTy (level, rhsTy))

and typeTerm ctx level = function
  | Num _ -> STyPrim "int"
  | Var v -> (
      match Ctx.find_opt v ctx with
      | Some (`S ty) -> ty
      | Some (`P ty) -> instantiate level ty
      | None -> failwith (Printf.sprintf "no variable " ^ v) )
  | Abs (name, body) ->
      let paramTy = STyVar (freshVar level) in
      let ctx1 = Ctx.add name (`S paramTy) ctx in
      STyFn (paramTy, typeTerm ctx1 level body)
  | App (fn, param) ->
      let fnTy = typeTerm ctx level fn in
      let paramTy = typeTerm ctx level param in
      let resTy = STyVar (freshVar level) in
      constrain fnTy (STyFn (paramTy, resTy));
      resTy
  | Record fields ->
      STyRecord (List.map (fun (f, t) -> (f, typeTerm ctx level t)) fields)
  | RecordProject (rcd, field) ->
      let rcdTy = typeTerm ctx level rcd in
      let fieldTy = STyVar (freshVar level) in
      constrain rcdTy (STyRecord [ (field, fieldTy) ]);
      fieldTy
  | Let { is_rec; name; rhs; body } ->
      let rhsTy = typeLetRhs ctx level is_rec name rhs in
      let ctx1 = Ctx.add name rhsTy ctx in
      typeTerm ctx1 level body

let typeBinding ctx { is_rec; name; body } =
  try
    let bodyTy = typeLetRhs ctx 0 is_rec name body in
    let ctx1 = Ctx.add name bodyTy ctx in
    (Ok (name, bodyTy), ctx1)
  with Failure m -> (Error m, ctx)

let typeProgram ctx =
  List.fold_left
    (fun (bindings, ctx) cur ->
      let bCur, ctx1 = typeBinding ctx cur in
      (bindings @ [ bCur ], ctx1))
    ([], ctx)
