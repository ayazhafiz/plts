open Language
open Typecheck

type compact_ty = {
  vars : var_state list;
  prims : simple_ty list;
  rcd : (string * compact_ty) list option;
  fn : (compact_ty * compact_ty) option;
}
(** Describes a union or intersection with different type components. *)

type compact_ty_scheme = {
  ty : compact_ty;
  rec_vars : (var_state * compact_ty) list;
}

let isEmpty { vars; prims; rcd; fn } =
  List.length vars = 0
  && List.length prims = 0
  && Option.is_none rcd && Option.is_none fn

let empty_compact_ty = { vars = []; prims = []; rcd = None; fn = None }

let merge_opts f = function
  | Some l, Some r -> Some (f l r)
  | (Some _ as l), _ -> l
  | _, (Some _ as r) -> r
  | _ -> None

let sort_rcd rcd = List.sort (fun (f1, _) (f2, _) -> String.compare f1 f2) rcd

let sort_vars vars =
  List.sort (fun (v1, _) (v2, _) -> compare v1.uid v2.uid) vars

let close_over expander items =
  let rec go complete todo =
    if List.length todo = 0 then complete
    else
      let newComplete = complete @ todo in
      let expandTodo = List.concat_map expander todo in
      let newTodo =
        List.filter (function t -> not (List.mem t newComplete)) expandTodo
      in
      go newComplete newTodo
  in
  go [] items

let reduce f = function x :: xs -> Some (List.fold_left f x xs) | [] -> None

let rec merge (isPos : bool) (lhs : compact_ty) (rhs : compact_ty) : compact_ty
    =
  let rcd : (string * compact_ty) list option =
    merge_opts
      (fun fL fR ->
        if isPos then
          (* TODO: why do we do it this way?? *)
          (* Positive (union): only base merge on one record. *)
          List.filter_map
            (fun (f, tyL) ->
              Option.map
                (fun tyR -> (f, merge isPos tyL tyR))
                (List.assoc_opt f fR))
            fL
        else
          (* Negative (intersection): base merge on both records. *)
          let onlyL =
            List.filter (fun (f, _) -> not (List.mem_assoc f fR)) fL
          in
          let mergedR =
            List.map
              (fun (f, tyR) ->
                let cTy =
                  match List.assoc_opt f fL with
                  | Some tyL -> merge isPos tyL tyR
                  | None -> tyR
                in
                (f, cTy))
              fR
          in
          onlyL @ mergedR)
      (lhs.rcd, rhs.rcd)
  in
  let fn =
    merge_opts
      (fun (pL, rL) (pR, rR) -> (merge (not isPos) pL pR, merge isPos rL rR))
      (lhs.fn, rhs.fn)
  in
  {
    vars = lhs.vars @ rhs.vars;
    prims = lhs.prims @ rhs.prims;
    rcd = Option.map sort_rcd rcd;
    fn;
  }

(** Compacts an inferred [simple_ty] into a [compact_ty], which is a more
    accessible representation for the simplification procedures. *)
let compactSimpleTy ty =
  let recursives = ref [] in
  let rec_vars = ref [] in
  (* [parents] remembers the variables whose bounds are being compacted,
      so as to remove spurious cycles such as ?a <: ?b and ?b <: ?a,
      which do not correspond to actual recursive types. *)
  let ecty = empty_compact_ty in
  let rec go parents inProcess isPos = function
    | STyInt as p -> { ecty with prims = [ p ] }
    | STyFn (p, r) ->
        let fn =
          Some (go [] inProcess (not isPos) p, go [] inProcess isPos r)
        in
        { ecty with fn }
    | STyRecord fields ->
        let rcd =
          Some
            ( List.map (fun (f, t) -> (f, go [] inProcess isPos t)) fields
            |> sort_rcd )
        in
        { ecty with rcd }
    | STyVar vs -> (
        let polarV = if isPos then Positive vs else Negative vs in
        if List.mem polarV inProcess then
          if List.mem vs parents then ecty
            (* spurious cycle: ignore the bound *)
          else
            let v =
              match List.assoc_opt polarV !recursives with
              | Some recursive -> recursive
              | None ->
                  let freshTy = freshVar 0 in
                  recursives := (polarV, freshTy) :: !recursives;
                  freshTy
            in
            { ecty with vars = [ v ] }
        else
          let inProcess = polarV :: inProcess in
          let parents = vs :: parents in
          let bounds = if isPos then vs.lower_bounds else vs.upper_bounds in
          let compactBounds =
            List.map (fun t -> go parents inProcess isPos t) bounds
          in
          let bound =
            List.fold_left
              (fun l r -> merge isPos l r)
              { ecty with vars = [ vs ] }
              compactBounds
          in
          match List.assoc_opt polarV !recursives with
          | Some v ->
              rec_vars := (v, bound) :: !rec_vars;
              { ecty with vars = [ v ] }
          | None -> bound )
  in
  { ty = go [] [] true ty; rec_vars = sort_vars !rec_vars }

(** https://github.com/LPTK/simple-sub/blob/febe38e237b3c1a8bdf5dfff22a166159a25c663/shared/src/main/scala/simplesub/TypeSimplifier.scala#L86
    Like [compactSimpleTy], but also make sure to produce a 'canonicalized'
    compact type, that is:

    a type where all co-occurring recursive types are merged, and if they have
    different cycle lengths, we create a new recursive type whose cycle length
    is the LCD of the respective original cycle lengths.

    To do this, while compacting the type we keep track of traversed compact
    types (instead of keeping track of mere single variables, as in
    [compactSimpleTy]).
    This requires an interleaved two-phase process where we first transitively
    merge all bounds of all co-occurring variables in the outer layer of the
    source type, and then traverse the resulting sets of variables further.

    This "unrolling of recursive types until they align" process is akin to
    the powerset construction for turning NFAs into DFAs,
    and can in principle result in an exponentially larger type,
    though in practice the algorithm often result in good simplifications down
    the line. *)
let canonicalizeSimpleTy ty =
  let recursives : ((compact_ty * bool) * var_state) list ref = ref [] in
  let rec_vars : (var_state * compact_ty) list ref = ref [] in
  let ecty = empty_compact_ty in
  (* Turns the outermost layer of a SimpleType into a CompactType, leaving type
     variables untransformed. *)
  let rec go0 isPos = function
    | STyInt as p -> { ecty with prims = [ p ] }
    | STyFn (p, r) ->
        let fn = Some (go0 (not isPos) p, go0 isPos r) in
        { ecty with fn }
    | STyRecord fields ->
        let rcd =
          Some (List.map (fun (f, t) -> (f, go0 isPos t)) fields |> sort_rcd)
        in
        { ecty with rcd }
    | STyVar _ as v ->
        let vars =
          close_over
            (function
              | STyVar vs ->
                  let bounds =
                    if isPos then vs.lower_bounds else vs.upper_bounds
                  in
                  List.filter (function STyVar _ -> true | _ -> false) bounds
              | _ -> [])
            [ v ]
        in
        let vars =
          List.filter_map (function STyVar vs -> Some vs | _ -> None) vars
        in
        { ecty with vars }
  in

  (* Merge the bounds of all type variables of the given CompactType, and
     traverse the result. *)
  let rec go1 inProcess ty isPos =
    if isEmpty ty then ty
    else
      let polarTy = (ty, isPos) in
      if List.mem polarTy inProcess then
        let var =
          match List.assoc_opt polarTy !recursives with
          | Some recursive -> recursive
          | None ->
              let freshTy = freshVar 0 in
              recursives := (polarTy, freshTy) :: !recursives;
              freshTy
        in
        { ecty with vars = [ var ] }
      else
        let bounds =
          List.concat_map
            (function
              | vs ->
                  let bounds =
                    if isPos then vs.lower_bounds else vs.upper_bounds
                  in
                  List.filter_map
                    (function STyVar _ -> None | b -> Some (go0 isPos b))
                    bounds)
            ty.vars
        in
        let bound = Option.value (reduce (merge isPos) bounds) ~default:ecty in
        let res = merge isPos ty bound in
        let inProcess = polarTy :: inProcess in
        let adapted =
          {
            res with
            rcd =
              Option.map
                (List.map (fun (f, t) -> (f, go1 inProcess t isPos)))
                res.rcd;
            fn =
              Option.map
                (fun (p, r) ->
                  (go1 inProcess p (not isPos), go1 inProcess r isPos))
                res.fn;
          }
        in
        match List.assoc_opt polarTy !recursives with
        | Some v ->
            rec_vars := (v, adapted) :: !rec_vars;
            { ecty with vars = [ v ] }
        | None -> adapted
  in
  { ty = go1 [] (go0 true ty) true; rec_vars = sort_vars !rec_vars }
