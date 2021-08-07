open Util

(** System F-like source language for this compiler. *)

type ty =
  | TInt
  | TName of string
      (** uninstantiated type bound by a universal quantifier ([All]) *)
  | TArrow of ty * ty
  | TAll of string * ty
  | TTup of ty list

type op = Plus | Minus | Times

type term =
  | Var of string
  | Int of int
  | Fix of {
      name : string;
      param : string;
      param_ty : ty;
      ret_ty : ty;
      body : term;
    }
  | App of term * term
  | TyAbs of string * term
  | TyApp of term * ty
  | Tup of term list
  | Proj of term * int
  | Op of op * term * term
  | If0 of term * term * term
  | Annot of term * ty

(*** Print ***)

let pp_ty f =
  let open Format in
  let rec go = function
    | TInt -> fprintf f "int"
    | TName a -> pp_print_string f a
    | TArrow (t1, t2) ->
        fprintf f "@[<hv 2>(";
        go t1;
        fprintf f " ->@ ";
        go t2;
        fprintf f ")@]"
    | TAll (a, t) ->
        fprintf f "@[<hv 2>∀%s.@," a;
        go t;
        fprintf f "@]"
    | TTup ts ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length ts - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          ts;
        fprintf f ")@]"
  in
  go

let pp_op f =
  let open Format in
  function
  | Plus -> pp_print_string f "+"
  | Minus -> pp_print_string f "-"
  | Times -> pp_print_string f "*"

let pp_term f =
  let open Format in
  let rec go = function
    | Var x -> pp_print_string f x
    | Int i -> pp_print_int f i
    | Fix { name; param; param_ty; ret_ty; body } ->
        fprintf f "@[<hv 2>(fix %s(%s: " name param;
        pp_ty f param_ty;
        fprintf f "): ";
        pp_ty f ret_ty;
        fprintf f ".@ ";
        go body;
        fprintf f ")@]"
    | App (e1, e2) ->
        fprintf f "@[<hov 2>(";
        go e1;
        fprintf f "@ ";
        go e2;
        fprintf f ")@]"
    | TyAbs (a, e) ->
        fprintf f "@[<hov 2>Λ%s.@ " a;
        go e;
        fprintf f "@]"
    | TyApp (e, t) ->
        fprintf f "@[<hv 2>";
        go e;
        fprintf f "<@,";
        pp_ty f t;
        fprintf f ">@]"
    | Tup es ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length es - 1 in
        List.iteri
          (fun i e ->
            go e;
            if i <> lasti then fprintf f ",@ ")
          es;
        fprintf f ")@]"
    | Proj (e, i) ->
        fprintf f "@[";
        go e;
        fprintf f ".%d@]" i
    | Op (op, e1, e2) ->
        fprintf f "@[<hov 2>(";
        go e1;
        fprintf f " ";
        pp_op f op;
        fprintf f "@ ";
        go e2;
        fprintf f ")@]"
    | If0 (test, then', else') ->
        fprintf f "@[<hov>(if0@;<1 2>@[";
        go test;
        fprintf f "@]@ then@;<1 2>@[";
        go then';
        fprintf f "@]@ else@;<1 2>@[";
        go else';
        fprintf f "@])@]"
    | Annot (e, t) ->
        fprintf f "@[<hov 2>(";
        go e;
        fprintf f ":@ ";
        pp_ty f t;
        fprintf f ")@]"
  in
  go

let string_of_term e = with_buffer (fun f -> pp_term f e) 40

let string_of_ty t = with_buffer (fun f -> pp_ty f t) 40

(*** Typecheck ***)

exception TyErr of string

let tyerr what = raise (TyErr ("F type error: " ^ what))

let rec ftv = function
  | TInt -> SSet.empty
  | TName a -> SSet.singleton a
  | TArrow (t1, t2) -> SSet.union (ftv t1) (ftv t2)
  | TTup ts -> List.fold_left SSet.union SSet.empty (List.map ftv ts)
  | TAll (a, t) -> SSet.remove a (ftv t)

let rec freshen a used = if SSet.mem a used then freshen (a ^ "'") used else a

let check_wf t tctx =
  if not (SSet.subset (ftv t) (SSet.of_list tctx)) then
    tyerr (Printf.sprintf "%s is malformed" (string_of_ty t))

let tysub a ta t =
  let rec go subs = function
    | TInt -> TInt
    | TName a -> (
        match List.assoc_opt a subs with Some t -> t | None -> TName a)
    | TArrow (t1, t2) -> TArrow (go subs t1, go subs t2)
    | TTup ts -> TTup (List.map (go subs) ts)
    | TAll (a', b) ->
        if a = a' then TAll (a', b)
        else if SSet.mem a' (ftv ta) then
          (* Something like (∀a'. a)[a/a']. Naive substitution would result in (∀a'. a'),
             transforming the constant quanitifier at a' in the outer scope to the identity.
             Find a fresh name a'->a'' inside the binder. *)
          let a'' = freshen a' (SSet.union (ftv b) (ftv ta)) in
          TAll (a'', go ((a', TName a'') :: subs) b)
        else TAll (a', go subs b)
  in
  go [ (a, ta) ] t

let rec tyof tctx vctx =
  let se = string_of_term in
  let st = string_of_ty in
  let sprintf = Printf.sprintf in
  function
  | Annot (e, t) ->
      let ety = tyof tctx vctx e in
      if ety = t then t
      else tyerr (sprintf "%s checks as %s not %s" (se e) (st ety) (st t))
  | Var x -> (
      match List.assoc_opt x vctx with
      | Some t -> t
      | None -> tyerr (sprintf "undeclared variable %s" x))
  | Int _ -> TInt
  | Fix { name; param; param_ty; ret_ty; body } ->
      check_wf param_ty tctx;
      check_wf ret_ty tctx;
      let fn_ty = TArrow (param_ty, ret_ty) in
      let body_ty =
        tyof tctx ((name, fn_ty) :: (param, param_ty) :: vctx) body
      in
      if body_ty = ret_ty then fn_ty
      else
        tyerr (sprintf "function checks as %s not %s" (st body_ty) (st ret_ty))
  | App (e1, e2) -> (
      match tyof tctx vctx e1 with
      | TArrow (t1, t2) ->
          let e2_ty = tyof tctx vctx e2 in
          if e2_ty = t1 then t2
          else tyerr (sprintf "argument %s must be of type %s" (se e2) (st t1))
      | _ -> tyerr (sprintf "application target %s is not a function" (se e1)))
  | TyAbs (a, e) ->
      let t = tyof (a :: tctx) vctx e in
      TAll (a, t)
  | TyApp (e, ta) -> (
      check_wf ta tctx;
      match tyof tctx vctx e with
      | TAll (a, t) -> tysub a ta t
      | _ ->
          tyerr
            (sprintf "type application target %s is not universally quantified"
               (se e)))
  | Tup es -> TTup (List.map (tyof tctx vctx) es)
  | Proj (e, i) -> (
      match tyof tctx vctx e with
      | TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some t -> t
          | None ->
              tyerr
                (sprintf "tuple type of %s cannot be indexed at \"%d\"" (se e) i)
          )
      | _ -> tyerr (sprintf "projection target %s is not a tuple" (se e)))
  | Op (_, e1, e2) as op -> (
      match (tyof tctx vctx e1, tyof tctx vctx e2) with
      | TInt, TInt -> TInt
      | _ ->
          tyerr (sprintf "both arguments in operation %s must be ints" (se op)))
  | If0 (test, then', else') -> (
      match
        (tyof tctx vctx test, tyof tctx vctx then', tyof tctx vctx else')
      with
      | TInt, tt, te when tt = te -> tt
      | TInt, tt, te ->
          tyerr
            (sprintf "then/else branches of if0 have different types (%s/%s)"
               (st tt) (st te))
      | _ -> tyerr (sprintf "if0 test %s must be an int" (se test)))
