open Language

module Ctx = struct
  include Map.Make (String)
end

let freshVar =
  let uid = ref 0 in
  fun level ->
    uid := !uid + 1;
    { uid = !uid; level; lower_bounds = []; upper_bounds = [] }

let rec level = function
  | STyVar vs -> vs.level
  | STyPrim _ -> 0
  | STyFn (p, r) -> max (level p) (level r)
  | STyRecord fields ->
      List.fold_left max 0 (List.map (fun (_, t) -> level t) fields)

let instantiate atLevel (PolyTy (limit, body)) =
  let freshened = ref [] in
  let rec go = function
    | t when level t <= limit -> t
    | STyPrim _ as t -> t
    | STyFn (p, r) -> STyFn (go p, go r)
    | STyRecord fields -> STyRecord (List.map (fun (f, t) -> (f, go t)) fields)
    | STyVar vs when List.mem_assoc vs !freshened ->
        STyVar (List.assoc vs !freshened)
    | STyVar vs ->
        let fresh = freshVar atLevel in
        freshened := (vs, fresh) :: !freshened;
        fresh.lower_bounds <- List.map go vs.lower_bounds;
        fresh.upper_bounds <- List.map go vs.upper_bounds;
        STyVar fresh
  in
  go body

let extrude ty isPos atLevel =
  let cache = ref [] in
  let rec go isPos = function
    | _ when level ty <= atLevel -> ty
    | STyPrim _ -> ty
    | STyFn (p, r) -> STyFn (go (not isPos) p, go isPos r)
    | STyRecord fields ->
        STyRecord (List.map (function f, t -> (f, go isPos t)) fields)
    | STyVar vs when List.mem_assoc vs !cache -> STyVar (List.assoc vs !cache)
    | STyVar vs ->
        let nvs = freshVar atLevel in
        cache := (vs, nvs) :: !cache;
        if isPos then (
          vs.upper_bounds <- STyVar nvs :: vs.upper_bounds;
          nvs.lower_bounds <- List.map (fun t -> go isPos t) vs.lower_bounds )
        else (
          vs.lower_bounds <- STyVar nvs :: vs.lower_bounds;
          nvs.upper_bounds <- List.map (fun t -> go isPos t) vs.upper_bounds );
        STyVar nvs
  in
  go isPos ty

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
              | None -> failwith (Printf.sprintf "no field " ^ field))
            fT
      | (STyVar tyS as s), tyT when level tyT <= level s ->
          tyS.upper_bounds <- tyT :: tyS.upper_bounds;
          List.iter (fun lower -> constr seen lower tyT) tyS.lower_bounds
      | tyS, (STyVar tyT as t) when level tyS <= level t ->
          tyT.lower_bounds <- tyS :: tyT.lower_bounds;
          List.iter (fun upper -> constr seen tyS upper) tyT.upper_bounds
      | (STyVar _ as tyS), tyT ->
          let tyT = extrude tyT false (level tyS) in
          constr seen tyS tyT
      | tyS, (STyVar _ as tyT) ->
          let tyS = extrude tyS true (level tyT) in
          constr seen tyS tyT
      | _ -> failwith "cannot constrain"
  in
  constr [] s t

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

and typeTerm ctx level term =
  match term with
  | Num _ -> STyPrim "int"
  | Var v -> (
      match Ctx.find_opt v ctx with
      | Some (`S ty) -> ty
      | Some (`P ty) -> instantiate level ty
      | None -> failwith (Printf.sprintf "no variable " ^ v) )
  | Abs (name, body) ->
      let paramTy = STyVar (freshVar level) in
      let ctx' = Ctx.add name (`S paramTy) ctx in
      STyFn (paramTy, typeTerm ctx' level body)
  | App (fn, param) ->
      let resTy = STyVar (freshVar level) in
      let fnTy = typeTerm ctx level fn in
      constrain fnTy (STyFn (typeTerm ctx level param, resTy));
      resTy
  | Record fields ->
      STyRecord (List.map (fun (f, t) -> (f, typeTerm ctx level t)) fields)
  | RecordProject (rcd, field) ->
      let fieldTy = STyVar (freshVar level) in
      let rcdTy = typeTerm ctx level rcd in
      constrain rcdTy (STyRecord [ (field, fieldTy) ]);
      fieldTy
  | Let { is_rec; name; rhs; body } ->
      let rhsTy = typeLetRhs ctx level is_rec name rhs in
      let ctx = Ctx.add name rhsTy ctx in
      typeTerm ctx level body

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
