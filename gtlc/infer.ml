(** Module [Infer] solves inference type variables as their "most informative"
    types, whenever a solution exists.

    The inference algorithm follows that of Siek and Vachharajani, 2008. *)

open Language
open Util

type constr_kind =
  | Cw  (** consistent *)
  | TArgCw
      (** inner arguments of this type constructor must be consistent.
          Deals with "ref" inferences when one of either side is not a direct ref,
          as in "!b" which generates the constraint "Ref b1 ~. b". *)
  | Eq  (** equivalent *)

type constr = ty * constr_kind * ty

let string_of_ck = function Cw -> "~=" | TArgCw -> "~." | Eq -> "="

(** Generates type constraints over a program. *)
let gen_constr freshty ctx e =
  let rec go ctx (Just e) =
    match e with
    | Nat _ -> Ok (TNat, [])
    | Bool _ -> Ok (TBool, [])
    | Var (`Local x | `Global x) -> (
        match List.assoc_opt x ctx with
        | Some t -> Ok (t, [])
        | None -> Error (x ^ " is not declared"))
    | App (Just (Lam (_, _, body)), ref_assign, `DesugaredSeq) ->
        (* Special case for DesugaredSeq as the desugared parameter is unused,
           so no need to constrain it. *)
        go ctx ref_assign >>= fun (_, c1) ->
        go ctx body >>= fun (tbody, c2) ->
        let b = freshty () in
        let constr = ((b, Cw, tbody) :: c1) @ c2 in
        Ok (b, constr)
    | App (e1, e2, _) ->
        go ctx e1 >>= fun (t1, c1) ->
        go ctx e2 >>= fun (t2, c2) ->
        let b = freshty () in
        let c3 = (t1, Cw, TArrow (t2, b)) :: (c1 @ c2) in
        Ok (b, c3)
    | Lam (x, t, e) ->
        go ((x, t) :: ctx) e >>= fun (r, c) -> Ok (TArrow (t, r), c)
    | If (c, thn, els) ->
        go ctx c >>= fun (ct, c1) ->
        go ctx thn >>= fun (thnt, c2) ->
        go ctx els >>= fun (elst, c3) ->
        let b = freshty () in
        let constrs =
          (ct, Cw, TBool) :: (thnt, Cw, b) :: (elst, Cw, b) :: (c1 @ c2 @ c3)
        in
        Ok (b, constrs)
    | Ref e ->
        go ctx e >>= fun (t, c) ->
        let b = freshty () in
        let constrs = (b, Cw, TRef t) :: c in
        Ok (b, constrs)
    | RefAssign (e1, e2) ->
        go ctx e1 >>= fun (t1, c1) ->
        go ctx e2 >>= fun (t2, c2) ->
        let b = freshty () in
        let constrs = (b, Cw, TRef t2) :: (t1, TArgCw, TRef t2) :: (c1 @ c2) in
        Ok (b, constrs)
    | Deref e ->
        go ctx e >>= fun (t, c) ->
        let b = freshty () in
        let constrs = (TRef b, TArgCw, t) :: c in
        Ok (b, constrs)
  in
  go ctx e |> Result.map snd

module Solver
    (*: sig
        val solve : constr list -> (ty TyMap.t, string) Result.t
      end*) =
struct
  exception SolveError of string

  module rec SolveNode : sig
    type solve_node = { kind : kind; mutable contains_vars : bool }

    (** A type solution kind, not a "kind" in the usual sense. *)
    and kind =
      | Prim of [ `Nat | `Bool ]
      | Ref of TyForest.t
      | Arrow of TyForest.t * TyForest.t
      | Unknown
      | Infer of int
  end =
    SolveNode

  and TyForest : sig
    include Forest.F with type elt = SolveNode.solve_node
  end = Forest.Make (struct
    type t = SolveNode.solve_node
  end)

  open SolveNode

  let kind_of_node n = (TyForest.value n).kind

  (** Computes the type associated with a type solution kind without resolution
      of partition representatives. *)
  let rec ty_of_kind = function
    | Prim `Nat -> TNat
    | Prim `Bool -> TBool
    | Ref t -> TRef (ty_of_kind (kind_of_node t))
    | Arrow (t1, t2) ->
        let t1, t2 = (kind_of_node t1, kind_of_node t2) in
        TArrow (ty_of_kind t1, ty_of_kind t2)
    | Unknown -> TUnknown
    | Infer i -> TInfer (`Var i)

  let string_of_kind kind = string_of_ty (ty_of_kind kind)

  let new_unk_node () =
    TyForest.create_node { kind = Unknown; contains_vars = false }

  let node_of_ty_generator () =
    let cache = Hashtbl.create 32 in
    let ivars = ref [] in
    let assoc ty node =
      let node = TyForest.create_node node in
      Hashtbl.add cache ty node;
      node
    in
    let register_ivar t ivar =
      ivars := (t, ivar) :: !ivars;
      ivar
    in
    let retrieve_ivars () = List.rev !ivars in
    let rec go ty =
      if Hashtbl.mem cache ty then Hashtbl.find cache ty
      else
        match ty with
        | TNat -> assoc ty { kind = Prim `Nat; contains_vars = false }
        | TBool -> assoc ty { kind = Prim `Bool; contains_vars = false }
        | TArrow (t1, t2) ->
            let t1, t2 = (go t1, go t2) in
            TyForest.create_node { kind = Arrow (t1, t2); contains_vars = true }
        | TRef t ->
            let t = go t in
            TyForest.create_node { kind = Ref t; contains_vars = true }
        | TInfer (`Var i) as t ->
            assoc ty { kind = Infer i; contains_vars = false }
            |> register_ivar t
        | TInfer (`Resolved _) ->
            failwith "unexpected resolved type before constraint resolution"
        | TUnknown ->
            (* Each instance of an "?" type should be treated as a fresh node in
               the solution forest. Otherwise we may end up with unnecessary
               conflicts. For example, support we have the constraints
                  ? ~= nat, ? ~= bool
               After observing the first constraint, we designate "nat" as the
               representative for the set containing the "?" node. If this "?"
               node is then used for the second constraint, we will fail trying to
               unify "?" and "bool". But both "nat" and "bool" are consistent with
               "?", so we should allow both constraints to exist without affecting
               each other. *)
            new_unk_node ()
    in
    (go, retrieve_ivars)

  (** Translates constraint types into the graph-based forest representation,
      and returns all nodes corresponding to inference variables. *)
  let trans_constrs constrs =
    let node_of_ty, retrieve_ivars = node_of_ty_generator () in
    let constrs =
      List.map (fun (t1, k, t2) -> (node_of_ty t1, k, node_of_ty t2)) constrs
    in
    (constrs, retrieve_ivars ())

  let inference_vars_of_constrs constrs =
    constrs
    |> List.concat_map (fun (a, b) -> [ a; b ])
    |> List.filter (fun n ->
           match kind_of_node n with Infer _ -> true | _ -> false)

  let order u v =
    match (kind_of_node u, kind_of_node v) with
    (* Case 1: type variable is constrained by "?". We want to propagate "?" as a
       solution, so we should mark it as the representative. *)
    | Unknown, Infer _ -> (u, v, true)
    | Infer _, Unknown -> (v, u, true)
    (* Case 2: type variable or "?" is constrained by a proper type. We want to
       propropagate that proper type as the representative. *)
    | _, Unknown | _, Infer _ -> (u, v, true)
    | Unknown, _ | Infer _, _ -> (v, u, true)
    (* Case 3: nothing special to handle *)
    | _ -> (u, v, false)

  let head_until item lst =
    let rec go acc = function
      | [] -> failwith "didn't see item"
      | j :: _ when j = item -> List.rev (j :: acc)
      | j :: rst -> go (j :: acc) rst
    in
    go [] lst

  (** Folds up a node in the forest with the representative of its
      partition (that is, quotients the forest under partitions), and emits
      reified substitutions for inference variables.
      If we discover that there is a cycle in the quotiened forest, the
      inference is unsolvable and we have to error. *)
  let repr_type_of_node node =
    let rec go seen node =
      if List.exists (TyForest.eq node) seen then
        let cycle =
          node :: head_until node seen
          |> List.rev |> List.map kind_of_node |> List.map string_of_kind
          |> String.concat " => "
        in
        raise (SolveError (Printf.sprintf "Cycle during reification: %s" cycle))
      else
        let seen' = node :: seen in
        let kind = kind_of_node node in
        match kind with
        | Prim `Nat -> TNat
        | Prim `Bool -> TBool
        | Ref t -> TRef (go seen' t)
        | Arrow (t1, t2) -> TArrow (go seen' t1, go seen' t2)
        | (Unknown | Infer _) as k ->
            let repr = TyForest.find node in
            let k' = kind_of_node repr in
            if k = k' then ty_of_kind k else go seen' repr
    in
    go [] node

  (** [solve constrs] yields a map of solutions to inference variables. *)
  let solve constrs =
    let constrs, ivars = trans_constrs constrs in
    let rec go = function
      | [] -> ()
      | (x, k, y) :: rst ->
          let u, v = (TyForest.find x, TyForest.find y) in
          let sk = match k with Cw | TArgCw -> Cw | Eq -> Eq in
          let new_constrs =
            if u != v then (
              let u, v, f = order u v in
              TyForest.union_ordered u v f;
              let u, v = (TyForest.value u, TyForest.value v) in
              match (u.kind, k, v.kind) with
              | Arrow (u1, u2), _, Arrow (v1, v2) ->
                  [ (u1, sk, v1); (u2, sk, v2) ]
              | Arrow (u1, u2), _, Unknown ->
                  if u.contains_vars then (
                    u.contains_vars <- false;
                    [ (new_unk_node (), sk, u1); (new_unk_node (), sk, u2) ])
                  else []
              | Unknown, sk, Arrow (v1, v2) ->
                  if v.contains_vars then (
                    v.contains_vars <- false;
                    [ (new_unk_node (), sk, v1); (new_unk_node (), sk, v2) ])
                  else []
              | Ref u1, Cw, Unknown ->
                  if u.contains_vars then (
                    u.contains_vars <- false;
                    [ (new_unk_node (), Cw, u1) ])
                  else []
              | Unknown, Cw, Ref v1 ->
                  if v.contains_vars then (
                    v.contains_vars <- false;
                    [ (new_unk_node (), Cw, v1) ])
                  else []
              | Ref s, TArgCw, Ref t -> [ (s, Cw, t) ]
              | Ref s, (Cw | Eq), Ref t -> [ (s, Eq, t) ]
              | _, _, Infer _ | Infer _, _, _ | Unknown, Cw, _ | _, Cw, Unknown
                ->
                  []
              | s, _, t when ty_of_kind s = ty_of_kind t -> []
              | s, _, t ->
                  raise
                    (SolveError
                       (Printf.sprintf "Unsolvable constraint: %s %s %s"
                          (string_of_kind s) (string_of_ck k) (string_of_kind t))))
            else []
          in
          go (new_constrs @ rst)
    in
    try
      go constrs;
      (* At this point, we want to fold up each node in the forest to the
         representative of their partition, and emit the complete substitutions
         as a result of that process. *)
      let solved_ivars =
        List.map (fun (ti, n) -> (ti, repr_type_of_node n)) ivars
      in
      List.to_seq solved_ivars |> TyMap.of_seq |> Result.ok
    with SolveError msg -> Error msg

  let%expect_test "constraint solving (Running example, under Definition 3)" =
    let constrs =
      [
        (* (? -> nat) -> (nat -> ?) -> nat ~= `b0 -> `b1 *)
        ( TArrow
            (TArrow (TUnknown, TNat), TArrow (TArrow (TNat, TUnknown), TNat)),
          TArrow (TInfer (`Var 0), TInfer (`Var 1)) );
        (* `b1 ~= `b0 -> `b2 *)
        (TInfer (`Var 1), TArrow (TInfer (`Var 0), TInfer (`Var 2)));
        (* Cycle tests *)
        (TInfer (`Var 3), TInfer (`Var 4));
        (TInfer (`Var 4), TInfer (`Var 3));
        (TInfer (`Var 4), TBool);
        (*
           ? -> `t13 ~= ((? -> `t11) -> `t14) -> `t15
           ? ~= (nat -> `t12) -> `t13
           nat -> nat ~= nat -> `t12
           ? -> `t11 ~= nat -> `t14
        *)
        ( TArrow (TUnknown, TInfer (`Var 13)),
          TArrow
            ( TArrow (TArrow (TUnknown, TInfer (`Var 11)), TInfer (`Var 14)),
              TInfer (`Var 15) ) );
        (TUnknown, TArrow (TArrow (TNat, TInfer (`Var 12)), TInfer (`Var 13)));
        (TArrow (TNat, TNat), TArrow (TNat, TInfer (`Var 12)));
        (TArrow (TUnknown, TInfer (`Var 11)), TArrow (TNat, TInfer (`Var 14)));
        (*
           `t21 ~= ref nat
           ref `22 ~= ref nat
           ref (`23 -> nat -> ref `24) ~= ref (bool -> nat -> ref (? -> ?))
           ? ~= ref nat
           ? ~= ref `25
        *)
        (TInfer (`Var 21), TRef TNat);
        (TRef (TInfer (`Var 22)), TRef TNat);
        ( TRef
            (TArrow (TInfer (`Var 23), TArrow (TNat, TRef (TInfer (`Var 24))))),
          TRef
            (TArrow (TBool, TArrow (TNat, TRef (TArrow (TUnknown, TUnknown)))))
        );
        (TUnknown, TRef TNat);
        (TUnknown, TRef (TInfer (`Var 25)));
        (TUnknown, TRef (TArrow (TNat, TNat)));
      ]
      |> List.map (fun (t, s) -> (t, Cw, s))
    in
    let solved =
      solve constrs
      |> Result.map (fun s ->
             s |> TyMap.to_seq
             |> Seq.map (fun (t, u) ->
                    Printf.sprintf "%s ~= %s" (string_of_ty t) (string_of_ty u))
             |> List.of_seq |> String.concat "\n")
      |> function
      | Ok s -> s
      | Error s -> s
    in
    print_string solved;
    [%expect
      {|
      `t0 ~= nat -> nat
      `t1 ~= (nat -> nat) -> nat
      `t2 ~= nat
      `t3 ~= bool
      `t4 ~= bool
      `t11 ~= ?
      `t12 ~= nat
      `t13 ~= ?
      `t14 ~= ?
      `t15 ~= ?
      `t21 ~= ref nat
      `t22 ~= nat
      `t23 ~= bool
      `t24 ~= ? -> ?
      `t25 ~= ? |}]
end

let subst_ty substs =
  let rec go t =
    match t with
    | TNat | TBool | TUnknown -> t
    | TRef t -> TRef (go t)
    | TArrow (t1, t2) -> TArrow (go t1, go t2)
    | TInfer (`Var _) -> TInfer (`Resolved (TyMap.find t substs))
    | TInfer (`Resolved _) ->
        failwith
          "unexpected resolved inference variable before type substitution"
  in
  go

let subst_expr substs =
  let rec go (Just e) =
    let e' =
      match e with
      | Nat _ | Bool _ | Var _ -> e
      | App (e1, e2, a) -> App (go e1, go e2, a)
      | Lam (x, t, e) -> Lam (x, subst_ty substs t, go e)
      | If (e1, e2, e3) -> If (go e1, go e2, go e3)
      | Ref e -> Ref (go e)
      | RefAssign (e1, e2) -> RefAssign (go e1, go e2)
      | Deref e -> Deref (go e)
    in
    Just e'
  in
  go

let infer freshty ctx e =
  gen_constr freshty ctx e >>= fun constrs ->
  Solver.solve constrs >>= fun substs -> Ok (subst_expr substs e)
