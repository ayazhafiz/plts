(** The cast intermediate language. *)

open Util
module L = Language

type ty = L.ty

module rec Expr : sig
  type expr =
    | Nat of int
    | Bool of bool
    | Var of [ `Local of string | `Global of string ]
    | App of elaborated_expr * elaborated_expr
    | Lam of string * ty * elaborated_expr
    | If of elaborated_expr * elaborated_expr * elaborated_expr
    | Ref of elaborated_expr
    | RefAssign of elaborated_expr * elaborated_expr
    | Deref of elaborated_expr
    | Cast of ty * elaborated_expr
    (* The below are only used for evaluation *)
    | Loc of int
    | Builtin of Builtin.builtin

  and elaborated_expr = Elab of expr * ty
end =
  Expr

and Builtin : sig
  class type builtin =
    object
      method eval : Expr.elaborated_expr -> Expr.elaborated_expr

      method name : string
    end
end =
  Builtin

open Expr

let rec allvars (Elab (e, _)) =
  let open S in
  match e with
  | Nat _ | Bool _ | Loc _ | Builtin _ -> empty
  | Var (`Global x | `Local x) -> singleton x
  | App (e1, e2) | RefAssign (e1, e2) -> union (allvars e1) (allvars e2)
  | Lam (x, _, e) -> add x (allvars e)
  | If (e1, e2, e3) -> union (allvars e1) (allvars e2) |> union (allvars e3)
  | Cast (_, e) | Ref e | Deref e -> allvars e

let rec freevars_with_tys (Elab (e, t)) =
  let open SMap in
  let check_dup _ t1 t2 =
    if t1 = t2 then Some t1 else failwith "free var has different types"
  in
  match e with
  | Nat _ | Bool _ | Loc _ | Builtin _ -> empty
  | Var (`Local x) -> singleton x t
  | Var (`Global _) -> empty
  | App (e1, e2) | RefAssign (e1, e2) ->
      union check_dup (freevars_with_tys e1) (freevars_with_tys e2)
  | Lam (x, _, e) -> remove x (freevars_with_tys e)
  | If (c, t, e) ->
      union check_dup (freevars_with_tys c) (freevars_with_tys t)
      |> union check_dup (freevars_with_tys e)
  | Cast (_, e) | Ref e | Deref e -> freevars_with_tys e

let freevars e = freevars_with_tys e |> SMap.to_seq |> Seq.map fst |> S.of_seq

let rec translate (L.Elab (e, t)) =
  match e with
  | L.Nat n -> Elab (Nat n, t)
  | L.Bool b -> Elab (Bool b, t)
  | L.Var x -> Elab (Var x, t)
  | L.Lam (x, tx, e) ->
      let e' = translate e in
      Elab (Lam (x, tx, e'), t)
  | L.App (e1, e2, _) -> (
      let (Elab (_, t1) as e1') = translate e1 in
      let (Elab (_, t2) as e2') = translate e2 in
      match t1 with
      | L.TUnknown ->
          let e1'_ty = L.TArrow (t2, L.TUnknown) in
          Elab (App (Elab (Cast (e1'_ty, e1'), e1'_ty), e2'), L.TUnknown)
      | L.TArrow (t, t') ->
          if t = t2 then Elab (App (e1', e2'), t')
          else Elab (App (e1', Elab (Cast (t, e2'), t)), t')
      | _ -> failwith "unreachable")
  | L.If (c, thn, els) ->
      let (Elab (_, tc) as c) = translate c in
      let (Elab (_, t1) as thn) = translate thn in
      let (Elab (_, t2) as els) = translate els in
      let thn' = if t1 = t then thn else Elab (Cast (t, thn), t) in
      let els' = if t2 = t then els else Elab (Cast (t, els), t) in
      let c' = if tc = L.TBool then c else Elab (Cast (L.TBool, c), L.TBool) in
      Elab (If (c', thn', els'), t)
  | L.Ref e ->
      let (Elab (_, t) as e) = translate e in
      Elab (Ref e, L.TRef t)
  | L.Deref e -> (
      let (Elab (_, t) as e') = translate e in
      match t with
      | L.TUnknown ->
          let ref_unk = L.TRef L.TUnknown in
          Elab (Deref (Elab (Cast (ref_unk, e'), ref_unk)), TUnknown)
      | L.TRef t' -> Elab (Deref e', t')
      | _ -> failwith "unreachable")
  | L.RefAssign (e1, e2) -> (
      let (Elab (_, t1) as e1') = translate e1 in
      let (Elab (_, t2) as e2') = translate e2 in
      match (t1, t2) with
      | L.TUnknown, t2 ->
          Elab
            (RefAssign (Elab (Cast (L.TRef t2, e1'), L.TRef t2), e2'), L.TRef t2)
      | L.TRef t, s when s <> t ->
          Elab (RefAssign (e1', Elab (Cast (t, e2'), t)), L.TRef t)
      | L.TRef t, _ -> Elab (RefAssign (e1', e2'), L.TRef t)
      | _ -> failwith "unreachable")

let pp_expr f =
  let open Format in
  let rec go (Elab (e, _)) =
    match e with
    | Nat n -> pp_print_int f n
    | Bool true -> pp_print_string f "#t"
    | Bool false -> pp_print_string f "#f"
    | Var (`Global x | `Local x) -> pp_print_string f x
    | App (e1, e2) ->
        fprintf f "@[<hov 2>(";
        go e1;
        fprintf f "@ ";
        go e2;
        fprintf f ")@]"
    | Lam (x, t, e) ->
        fprintf f "@[<hov 2>(\\";
        pp_print_string f x;
        fprintf f ": ";
        L.pp_ty f t;
        fprintf f ".@ ";
        go e;
        fprintf f ")@]"
    | If (c, t, e) ->
        fprintf f "@[<hv 0>@[<hov 2>if@ ";
        go c;
        fprintf f "@]@ @[<hov 2>then@ ";
        go t;
        fprintf f "@]@ @[<hov 2>else@ ";
        go e;
        fprintf f "@]@]"
    | Ref e ->
        fprintf f "@[(ref ";
        go e;
        fprintf f ")@]"
    | RefAssign (e1, e2) ->
        fprintf f "@[<hov 2>(";
        go e1;
        fprintf f " <-@ ";
        go e2;
        fprintf f ")@]"
    | Deref e ->
        fprintf f "@[!(";
        go e;
        fprintf f ")@]"
    | Cast (t, e) ->
        fprintf f "@[<hov 2>(<";
        L.pp_ty f t;
        fprintf f ">@,";
        go e;
        fprintf f ")@]"
    | Loc i -> fprintf f "@%d" i
    | Builtin b -> fprintf f "[%s]" b#name
  in
  go

let string_of_expr ?(width = default_width) v =
  with_buffer (fun f -> pp_expr f v) width
