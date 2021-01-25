(** Module [simplify] simplifies inferred types in a program into a canonical
    form with the minimum number of type variables needed to correctly type an
    expression. *)

open Language
open Typecheck

type compact_ty = {
  vars : var_state list;
  prims : string list;
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

let get_or_update k default mp =
  match List.assoc_opt k !mp with
  | Some v -> v
  | None ->
      mp := (k, default) :: !mp;
      default

let get_or_empty_l k mp =
  List.assoc_opt k !mp |> Option.map (fun v -> !v) |> Option.value ~default:[]

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
    | STyPrim p -> { ecty with prims = [ p ] }
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
          (* spurious cycle: ignore the bound *)
          if List.mem vs parents then ecty
          else
            let v = get_or_update polarV (freshVar 0) recursives in
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
    | STyPrim p -> { ecty with prims = [ p ] }
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
        let var = get_or_update polarTy (freshVar 0) recursives in
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

(** https://github.com/LPTK/simple-sub/blob/febe38e237b3c1a8bdf5dfff22a166159a25c663/shared/src/main/scala/simplesub/TypeSimplifier.scala#L161
    Simplifies a type scheme with the ideas described below.
    See 4.3 of the paper for more details.
    Idea: if a type var 'a always occurs positively (resp. neg) along with some 'b AND vice versa,
         this means that the two are undistinguishable, and they can therefore be unified.
       Ex: ('a & 'b) -> ('a, 'b) is the same as 'a -> ('a, 'a)
       Ex: ('a & 'b) -> 'b -> ('a, 'b) is NOT the same as 'a -> 'a -> ('a, 'a)
         there is no value of 'a that can make 'a -> 'a -> ('a, 'a) <: (a & b) -> b -> (a, b) work
         we'd require 'a :> b | a & b <: a & b, which are NOT valid bounds!
       Ex: 'a -> 'b -> 'a | 'b is the same as 'a -> 'a -> 'a
       Justification: the other var 'b can always be taken to be 'a & 'b (resp. a | b)
         without loss of gen. Indeed, on the pos side we'll have 'a <: 'a & 'b and 'b <: 'a & 'b
         and on the neg side, we'll always have 'a and 'b together, i.e., 'a & 'b
    
    Additional idea: remove variables which always occur both positively AND negatively together
         with some other type.
       This would arise from constraints such as: 'a :> Int <: 'b and 'b <: Int
         (contraints which basically say 'a =:= 'b =:= Int)
       Ex: 'a ∧ Int -> 'a ∨ Int is the same as Int -> Int
       Currently, we only do this for primitive types PrimType.
       In principle it could be done for functions and records, too.
       Note: conceptually, this idea subsumes the simplification that removes variables occurring
           exclusively in positive or negative positions.
         Indeed, if 'a never occurs positively, it's like it always occurs both positively AND
         negatively along with the type Bot, so we can replace it with Bot. *)
let simplifyTy cty =
  (* State accumulated during the analysis phase*)
  let allVars = ref (List.map fst cty.rec_vars) in
  let recVars = ref [] in
  let coOccurences = ref [] in
  (* This will be filled up after the analysis phase, to influence the
     reconstruction phase *)
  let varSubst = ref [] in

  (* Traverses the type, performing the analysis, and returns a thunk to
     reconstruct it later *)
  let rec go ty isPos =
    List.iter
      (fun var ->
        allVars := var :: !allVars;
        let newOccs =
          List.map (fun s -> STyVar s) ty.vars
          @ List.map (fun p -> STyPrim p) ty.prims
        in
        ( match List.assoc_opt (isPos, var) !coOccurences with
        (* compute the intersection of co-occurence *)
        | Some occurs ->
            occurs := List.filter (fun t -> List.mem t newOccs) !occurs
        | None -> coOccurences := ((isPos, var), ref newOccs) :: !coOccurences
        );
        match List.assoc_opt var cty.rec_vars with
        | Some bound ->
            (* if ty is recursive we need to process its bound *)
            if not (List.mem_assoc var !recVars) then (
              let goLater = ref (fun () -> empty_compact_ty) in
              goLater :=
                fun () ->
                  (* we store the fact that we're descending into the bound to
                      avoid infinite recursion *)
                  recVars := (var, !goLater) :: !recVars;
                  (go bound isPos) () )
            else ()
        | None -> ())
      ty.vars;
    let rcd1 = Option.map (List.map (fun (f, t) -> (f, go t isPos))) ty.rcd in
    let fn1 = Option.map (fun (p, r) -> (go p (not isPos), go r isPos)) ty.fn in
    fun () ->
      let newVars =
        List.concat_map
          (fun v ->
            match List.assoc_opt v !varSubst with
            | Some (Some v2) -> [ v2 ]
            | Some None -> []
            | None -> [ v ])
          ty.vars
      in
      {
        vars = newVars;
        prims = ty.prims;
        rcd = Option.map (List.map (fun (f, t) -> (f, t ()))) rcd1;
        fn = Option.map (fun (p, r) -> (p (), r ())) fn1;
      }
  in
  let gone = go cty.ty true in
  (* Simplify away those non-recursive variables that only occur in positive or
     negative positions *)
  List.iter
    (fun var ->
      if not (List.mem_assoc var !recVars) then
        match
          ( List.assoc_opt (true, var) !coOccurences,
            List.assoc_opt (false, var) !coOccurences )
        with
        | Some _, None | None, Some _ -> varSubst := (var, None) :: !varSubst
        | None, None ->
            failwith
              "bad state: variable occurs neither positively nor negatively"
        | _ -> ()
      else ())
    !allVars;
  (* Unify equivalent variables based on polar co-occurrence analysis *)
  let goUnify pol v = function
    | STyPrim _ as prim
      when List.assoc_opt (not pol, v) !coOccurences
           |> Option.map (fun tys -> List.mem prim !tys)
           |> Option.value ~default:false ->
        varSubst := (v, None) :: !varSubst
    | STyVar w
      when (not (w == v))
           && (not (List.mem_assoc w !varSubst))
           (* We avoid merging rec and non-rec vars, because the non-rec one may not be strictly polar *)
           && List.mem_assoc w !recVars = List.mem_assoc v !recVars -> (
        let v_w_coOccur =
          get_or_empty_l (pol, w) coOccurences |> List.mem (STyVar v)
        in
        if v_w_coOccur then
          (* unify w into v *)
          varSubst := (w, Some v) :: !varSubst;
        (* Since w gets unified with v, we need to merge their bounds if they are recursive,
           and otherwise merge the other co-occurrences of v and w from the other polarity.
           For instance,
            consider that if we merge v and w in `(v & w) -> v & x -> w -> x`
            we get `v -> v & x -> v -> x`
            and the old positive co-occ of v, {v,x} should be changed to just {v,x} & {w,v} == {v} *)
        match List.assoc_opt w !recVars with
        | Some b_w ->
            (* if w is recursive, so is v *)
            if List.mem_assoc (not pol, w) !coOccurences then
              failwith "bad state: recursive types must have strict polarity";

            (* w is merged into v, so we forget about it *)
            recVars := List.filter (fun (t, _) -> t <> w) !recVars;
            let b_v = List.assoc v !recVars in
            (* associate the new recursive bound for v we get by merging in w *)
            recVars := (v, fun () -> merge pol (b_v ()) (b_w ())) :: !recVars
        | None ->
            (* w is not recursive and neither is v *)
            (* opposite-polarity coOccurences must be defined for both v and w,
               otherwise we'd already have simplified away the non-rec variables in the first pass *)
            let w_coOccs = List.assoc (not pol, w) !coOccurences in
            let v_coOccs = List.assoc (not pol, v) !coOccurences in
            v_coOccs :=
              List.filter
                (fun ty -> ty = STyVar v || List.mem ty !w_coOccs)
                !v_coOccs )
    | _ -> ()
  in
  let polarities = [ true; false ] in
  let varsToUnify =
    List.filter (fun v -> not (List.mem_assoc v !varSubst)) !allVars
  in
  varsToUnify
  |> List.iter (fun v ->
         polarities
         |> List.iter (fun pol ->
                let toUnify = get_or_empty_l (pol, v) coOccurences in
                List.iter (goUnify pol v) toUnify));

  {
    ty = gone ();
    rec_vars = sort_vars (List.map (fun (k, v) -> (k, v ())) !recVars);
  }

(** https://github.com/LPTK/simple-sub/blob/febe38e237b3c1a8bdf5dfff22a166159a25c663/shared/src/main/scala/simplesub/TypeSimplifier.scala#L284
    Coalesces a [compact_ty_scheme] into a [ty] while performing hash-consing
    to tie recursive type knots a bit tighter, when possible. *)
let coalesceCompactTy cty =
  let rec go inProcess pol ty =
    match List.assoc_opt (ty, pol) inProcess with
    | Some getTy ->
        let res = getTy () in
        res
    | None -> (
        let isRec = ref false in
        let markAsRec () =
          isRec := true;
          let vs = match ty with `Var vs -> vs | _ -> freshVar 0 in
          tyvar_of_uid vs.uid
        in
        let inProcess = ((ty, pol), fun () -> markAsRec ()) :: inProcess in
        match ty with
        | `Var vs ->
            List.assoc_opt vs cty.rec_vars
            |> Option.fold ~none:(tyvar_of_uid vs.uid) ~some:(fun t ->
                   go inProcess pol (`Compact t))
        | `Compact { vars; prims; rcd; fn } ->
            let base, mrg =
              if pol then (TyBottom, fun l r -> TyUnion (l, r))
              else (TyTop, fun l r -> TyIntersection (l, r))
            in
            let vars = List.map (fun v -> go inProcess pol (`Var v)) vars in
            let prims = List.map (fun n -> TyPrim n) prims in
            let rcd =
              rcd
              |> Option.map (fun fields ->
                     TyRecord
                       (List.map
                          (fun (f, t) -> (f, go inProcess pol (`Compact t)))
                          fields))
              |> Option.to_list
            in
            let fn =
              fn
              |> Option.map (fun (p, r) ->
                     let p = go inProcess (not pol) (`Compact p) in
                     let r = go inProcess pol (`Compact r) in
                     TyFn (p, r))
              |> Option.to_list
            in
            let allVariants = vars @ prims @ rcd @ fn in
            Option.value (reduce mrg allVariants) ~default:base )
  in
  go [] true (`Compact cty.ty)
