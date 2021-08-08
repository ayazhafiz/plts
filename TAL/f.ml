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

let tyerr what = raise (TyErr ("F type error: " ^ what))

let rec ftv = function
  | TInt -> SSet.empty
  | TName a -> SSet.singleton a
  | TArrow (t1, t2) -> SSet.union (ftv t1) (ftv t2)
  | TTup ts -> List.fold_left SSet.union SSet.empty (List.map ftv ts)
  | TAll (a, t) -> SSet.remove a (ftv t)

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

let rec elab tctx vctx =
  let se = string_of_term in
  let st = string_of_ty in
  let sprintf = Printf.sprintf in
  let annot e t = (Annot (e, t), t) in
  function
  | Annot (e, t) ->
      let e', ety = elab tctx vctx e in
      if ety = t then annot e' t
      else tyerr (sprintf "%s checks as %s not %s" (se e) (st ety) (st t))
  | Var x -> (
      match List.assoc_opt x vctx with
      | Some t -> annot (Var x) t
      | None -> tyerr (sprintf "undeclared variable %s" x))
  | Int i -> annot (Int i) TInt
  | Fix ({ name; param; param_ty; ret_ty; body } as f) ->
      check_wf param_ty tctx;
      check_wf ret_ty tctx;
      let fn_ty = TArrow (param_ty, ret_ty) in
      let body', body_ty =
        elab tctx ((name, fn_ty) :: (param, param_ty) :: vctx) body
      in
      if body_ty = ret_ty then annot (Fix { f with body = body' }) ret_ty
      else
        tyerr (sprintf "function checks as %s not %s" (st body_ty) (st ret_ty))
  | App (e1, e2) -> (
      match elab tctx vctx e1 with
      | e1', TArrow (t1, t2) ->
          let e2', e2_ty = elab tctx vctx e2 in
          if e2_ty = t1 then annot (App (e1', e2')) t2
          else tyerr (sprintf "argument %s must be of type %s" (se e2) (st t1))
      | _ -> tyerr (sprintf "application target %s is not a function" (se e1)))
  | TyAbs (a, e) ->
      let e', t = elab (a :: tctx) vctx e in
      annot (TyAbs (a, e')) (TAll (a, t))
  | TyApp (e, ta) -> (
      check_wf ta tctx;
      match elab tctx vctx e with
      | e', TAll (a, t) -> annot (TyApp (e', ta)) (tysub a ta t)
      | _ ->
          tyerr
            (sprintf "type application target %s is not universally quantified"
               (se e)))
  | Tup es ->
      let es', tys = List.map (elab tctx vctx) es |> List.split in
      annot (Tup es') (TTup tys)
  | Proj (e, i) -> (
      match elab tctx vctx e with
      | e', TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some t -> annot (Proj (e', i)) t
          | None ->
              tyerr
                (sprintf "tuple type of %s cannot be indexed at \"%d\"" (se e) i)
          )
      | _ -> tyerr (sprintf "projection target %s is not a tuple" (se e)))
  | Op (o, e1, e2) as op -> (
      match (elab tctx vctx e1, elab tctx vctx e2) with
      | (e1', TInt), (e2', TInt) -> annot (Op (o, e1', e2')) TInt
      | _ ->
          tyerr (sprintf "both arguments in operation %s must be ints" (se op)))
  | If0 (test, then', else') -> (
      match
        (elab tctx vctx test, elab tctx vctx then', elab tctx vctx else')
      with
      | (test', TInt), (t', tt), (e', te) when tt = te ->
          annot (If0 (test', t', e')) tt
      | (_, TInt), (_, tt), (_, te) ->
          tyerr
            (sprintf "then/else branches of if0 have different types (%s/%s)"
               (st tt) (st te))
      | _ -> tyerr (sprintf "if0 test %s must be an int" (se test)))

let elaborate e =
  let e', _ = elab [] [] e in
  e'

(*** Eval ***)

let rec fvs =
  let open SSet in
  function
  | Var x -> singleton x
  | Int _ -> empty
  | Fix { name; param; body; _ } -> fvs body |> remove name |> remove param
  | App (e1, e2) -> union (fvs e1) (fvs e2)
  | TyAbs (_, e) -> fvs e
  | TyApp (e, _) -> fvs e
  | Tup ts -> List.fold_left union empty (List.map fvs ts)
  | Proj (e, _) -> fvs e
  | Op (_, e1, e2) -> union (fvs e1) (fvs e2)
  | If0 (e1, e2, e3) -> fvs e1 |> union (fvs e2) |> union (fvs e3)
  | Annot (e, _) -> fvs e

let subst x e tm =
  let rec go subs tm =
    match tm with
    | Var y -> ( match List.assoc_opt y subs with Some e -> e | None -> Var y)
    | Int _ -> tm
    | Fix { name; param; param_ty; ret_ty; body } ->
        if x = name || x = param then tm
        else
          let fvs_e = fvs e in
          let used = SSet.union fvs_e (fvs body) in
          let vs = [ name; param ] in
          let vs', esubs =
            List.map
              (fun v ->
                if SSet.mem v fvs_e then
                  let v' = freshen v used in
                  (v', Some (v, Var v'))
                else (v, None))
              vs
            |> List.split
          in
          let esubs = List.filter_map Fun.id esubs in
          let name', param' =
            match vs' with [ n; p ] -> (n, p) | _ -> failwith "unreachable"
          in
          Fix
            {
              name = name';
              param = param';
              param_ty;
              ret_ty;
              body = go (esubs @ subs) body;
            }
    | App (e1, e2) -> App (go subs e1, go subs e2)
    | TyAbs (a, e) -> TyAbs (a, go subs e)
    | TyApp (e, t) -> TyApp (go subs e, t)
    | Tup ts -> Tup (List.map (go subs) ts)
    | Proj (e, i) -> Proj (go subs e, i)
    | Op (op, e1, e2) -> Op (op, go subs e1, go subs e2)
    | If0 (e1, e2, e3) -> If0 (go subs e1, go subs e2, go subs e3)
    | Annot (e, t) -> Annot (go subs e, t)
  in
  go [ (x, e) ] tm

let rec isvalue = function
  | Int _ | Fix _ | TyAbs _ -> true
  | Tup ts -> List.for_all isvalue ts
  | _ -> false

let evalerr what = raise (EvalErr ("F eval error: " ^ what))

let rec step = function
  | App ((Fix { name; param; body; _ } as f), e2) when isvalue e2 ->
      subst param e2 body |> subst name f
  | App (e1, e2) when isvalue e1 -> App (e1, step e2)
  | TyApp (TyAbs (_, body), _) -> body
  | TyApp (e, t) -> TyApp (step e, t)
  | Tup es ->
      let rec do1 = function
        | [] -> []
        | e :: rst when isvalue e -> e :: do1 rst
        | e :: rst -> step e :: rst
      in
      Tup (do1 es)
  | Proj (Tup es, i) -> List.nth es (i - 1)
  | Proj (e, i) -> Proj (step e, i)
  | Op (op, Int i, Int j) ->
      Int (match op with Plus -> i + j | Minus -> i - j | Times -> i * j)
  | Op (op, e1, e2) when isvalue e1 -> Op (op, e1, step e2)
  | Op (op, e1, e2) -> Op (op, step e1, e2)
  | If0 (Int 0, e1, _) -> e1
  | If0 (Int _, _, e2) -> e2
  | If0 (t, e1, e2) -> If0 (step t, e1, e2)
  | Annot (e, _) -> e
  | t -> evalerr (Printf.sprintf "term %s is stuck" (string_of_term t))

let rec eval tm = if isvalue tm then tm else eval (step tm)
