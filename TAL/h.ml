open Util

type ty =
  | TName of string
  | TInt
  | TFn of { typarams : string list; params : ty list }
  | TTup of ty list
  | TExist of string * ty

type annot_val = Annot of value * ty

and value =
  | VVar of string
  | VInt of int
  | VTup of annot_val list
  | VTyApp of annot_val * ty
  | VPack of ty * annot_val * ty
      (** pack [t1, v] as ∃a.t2   (t1 is the true a) *)

and heap_value =
  | Code of { typarams : string list; params : (string * ty) list; body : term }

and decl =
  | DeclVal of string * annot_val  (** x = v *)
  | DeclProj of string * annot_val * int  (** x = v.i *)
  | DeclOp of string * op * annot_val * annot_val  (** x = v R v *)
  | DeclUnpack of string * string * annot_val  (** [a, x] = unpack v *)

and term =
  | Let of decl * term  (** let d in e *)
  | App of annot_val * annot_val list  (** v<ts>(vs) *)
  | If0 of annot_val * term * term
  | Halt of ty * annot_val

and program = Prog of (string * heap_value) list (* letrecs *) * term

let tyaeq t1 t2 =
  let nexti =
    let i = ref 0 in
    fun () ->
      incr i;
      !i
  in
  let rec go s1 s2 = function
    | TName x1, TName x2 -> (
        match (List.assoc_opt x1 s1, List.assoc_opt x2 s2) with
        | Some n1, Some n2 -> n1 = n2
        | None, None -> x1 = x2
        | _ -> false)
    | TInt, TInt -> true
    | TFn { typarams = tp1; params = p1 }, TFn { typarams = tp2; params = p2 }
      ->
        if
          List.length tp1 <> List.length tp2 || List.length p1 <> List.length p2
        then false
        else
          let tp_maps = List.map (fun _ -> nexti ()) tp1 in
          let s1' = List.combine tp1 tp_maps @ s1 in
          let s2' = List.combine tp2 tp_maps @ s2 in
          List.for_all (go s1' s2') (List.combine p1 p2)
    | TTup t1, TTup t2 ->
        List.length t1 = List.length t2
        && List.for_all (go s1 s2) (List.combine t1 t2)
    | TExist (a1, t1), TExist (a2, t2) ->
        let ti = nexti () in
        go ((a1, ti) :: s1) ((a2, ti) :: s2) (t1, t2)
    | _ -> false
  in
  go [] [] (t1, t2)

(*** Print ***)

let pp_ty f =
  let open Format in
  let rec go = function
    | TInt -> fprintf f "int"
    | TName a -> pp_print_string f a
    | TFn { typarams; params } ->
        fprintf f "@[<hov 2>(";
        if List.length typarams <> 0 then (
          fprintf f "∀<";
          let lasti = List.length typarams - 1 in
          List.iteri
            (fun i t ->
              go (TName t);
              if i <> lasti then fprintf f ",@ ")
            typarams;
          fprintf f ">.@,");
        fprintf f "(";
        let lasti = List.length params - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          params;
        fprintf f ")@ -> void)@]"
    | TTup ts ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length ts - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          ts;
        fprintf f ")@]"
    | TExist (a, t) ->
        fprintf f "@[<hov 2>(∃%s.@," a;
        go t;
        fprintf f ")@]"
  in
  go

let rec pp_annot f (Annot (v, _)) = pp_value f v

and pp_value f =
  let open Format in
  let go = function
    | VVar x -> pp_print_string f x
    | VInt i -> pp_print_int f i
    | VTup es ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length es - 1 in
        List.iteri
          (fun i e ->
            pp_annot f e;
            if i <> lasti then fprintf f ",@ ")
          es;
        fprintf f ")@]"
    | VTyApp (v, t) ->
        fprintf f "@[<hov 2>";
        pp_annot f v;
        fprintf f "<@,";
        pp_ty f t;
        fprintf f ">@]"
    | VPack (t1, v, t2) ->
        fprintf f "@[<hov 2>(pack [";
        pp_ty f t1;
        fprintf f ",@ ";
        pp_annot f v;
        fprintf f "]@ as ";
        pp_ty f t2;
        fprintf f ")@]"
  in
  go

and pp_heap_value f =
  let open Format in
  function
  | Code { typarams; params; body } ->
      fprintf f "@[<hov 2>(code";
      if List.length typarams <> 0 then (
        fprintf f "<";
        let lasti = List.length typarams - 1 in
        List.iteri
          (fun i t ->
            pp_print_string f t;
            if i <> lasti then fprintf f ",@ ")
          typarams;
        fprintf f ">.@,");
      fprintf f "(";
      let lasti = List.length params - 1 in
      List.iteri
        (fun i (p, t) ->
          fprintf f "@[<hov 2>%s:@ " p;
          pp_ty f t;
          fprintf f "@]";
          if i <> lasti then fprintf f ",@ ")
        params;
      fprintf f ").@ ";
      pp_term f body;
      fprintf f ")@]"

and pp_decl f =
  let open Format in
  function
  | DeclVal (s, v) ->
      fprintf f "@[<hov 2>%s =@ " s;
      pp_annot f v;
      fprintf f "@]"
  | DeclProj (s, v, i) ->
      fprintf f "@[<hov 2>%s =@ " s;
      pp_annot f v;
      fprintf f ".%d@]" i
  | DeclOp (s, op, v1, v2) ->
      fprintf f "@[<hov 2>%s =@ @[<hov 2>" s;
      pp_annot f v1;
      fprintf f " ";
      pp_op f op;
      fprintf f "@ ";
      pp_annot f v2;
      fprintf f "@]@]"
  | DeclUnpack (a, x, v) ->
      fprintf f "@[<hov 2>[%s, %s] =@ @[<hov 2>unpack@ " a x;
      pp_annot f v;
      fprintf f "@]@]"

and pp_term f =
  let open Format in
  let rec go = function
    | Let (d, e) ->
        fprintf f "@[<hov>(let@;<1 2>@[";
        pp_decl f d;
        fprintf f "@]@ in@;<1 2>@[";
        go e;
        fprintf f "@])@]"
    | App (v, params) ->
        fprintf f "@[<hov 2>(";
        pp_annot f v;
        fprintf f "(";
        let lasti = List.length params - 1 in
        List.iteri
          (fun i p ->
            pp_annot f p;
            if i <> lasti then fprintf f ",@ ")
          params;
        fprintf f "))@]"
    | If0 (test, then', else') ->
        fprintf f "@[<hov>(if0@;<1 2>@[";
        pp_annot f test;
        fprintf f "@]@ then@;<1 2>@[";
        go then';
        fprintf f "@]@ else@;<1 2>@[";
        go else';
        fprintf f "@])@]"
    | Halt (t, v) ->
        fprintf f "@[<hov 2>halt<@,";
        pp_ty f t;
        fprintf f ">@,";
        pp_annot f v;
        fprintf f "@]"
  in
  go

and pp_program f =
  let open Format in
  function
  | Prog (defs, e) ->
      fprintf f "@[<hov>letrec@;<1 2>@[<v>";
      List.iter
        (fun (n, hv) ->
          fprintf f "@[<hov 2>%s ->@ " n;
          pp_heap_value f hv;
          fprintf f "@]@,")
        defs;
      fprintf f "@]@ in@;<1 2>@[";
      pp_term f e;
      fprintf f "@]@]"

let string_of_annot e = with_buffer (fun f -> pp_annot f e) 80

let string_of_value e = with_buffer (fun f -> pp_value f e) 80

let string_of_term e = with_buffer (fun f -> pp_term f e) 80

let string_of_ty t = with_buffer (fun f -> pp_ty f t) 80

let string_of_program p = with_buffer (fun f -> pp_program f p) 80

(*** Typecheck ***)

let tyerr what = raise (TyErr ("H type error: " ^ what))

let rec ftv = function
  | TInt -> SSet.empty
  | TName a -> SSet.singleton a
  | TTup ts -> List.fold_left SSet.union SSet.empty (List.map ftv ts)
  | TFn { typarams; params } ->
      let ftvs = SSet.(List.fold_left union empty (List.map ftv params)) in
      SSet.(diff ftvs (of_list typarams))
  | TExist (a, t) -> SSet.remove a (ftv t)

let tysub a ta t =
  let rec go subs = function
    | TInt -> TInt
    | TName a -> (
        match List.assoc_opt a subs with Some t -> t | None -> TName a)
    | TTup ts -> TTup (List.map (go subs) ts)
    | TFn { typarams; params } ->
        if List.mem a typarams then TFn { typarams; params }
        else if List.exists (fun a' -> SSet.mem a' (ftv ta)) typarams then
          (* Something like (∀(a', b'). a)[a/a']. Naive substitution would result in (∀a'. a'),
             transforming the constant quanitifier at a' in the outer scope to the identity.
             Find a fresh name a'->a'' inside the binder. *)
          let ftv_ta = ftv ta in
          let ftv_body =
            SSet.(List.fold_left union empty (List.map ftv params))
          in
          let used_vars = SSet.union ftv_ta ftv_body in
          let typarams', newsubs =
            List.map
              (fun a' ->
                if SSet.mem a' ftv_ta then
                  let a'' = freshen a' used_vars in
                  (a'', Some (a', TName a''))
                else (a', None))
              typarams
            |> List.split
          in
          let newsubs = List.filter_map Fun.id newsubs in
          TFn
            {
              typarams = typarams';
              params = List.map (go (newsubs @ subs)) params;
            }
        else TFn { typarams; params = List.map (go subs) params }
    | TExist (a', t) ->
        if a = a' then TExist (a', t)
        else if SSet.mem a' (ftv ta) then
          let a'' = freshen a' (SSet.union (ftv ta) (ftv t)) in
          TExist (a'', go ((a', TName a'') :: subs) t)
        else TExist (a', go subs t)
  in
  go [ (a, ta) ] t

let sa = string_of_annot

let sv = string_of_value

let se = string_of_term

let st = string_of_ty

let sprintf = Printf.sprintf

let ty_wf t tctx =
  if not (SSet.subset (ftv t) (SSet.of_list tctx)) then
    tyerr (Printf.sprintf "%s is malformed" (string_of_ty t))

let rec tyof_annot tctx vctx (Annot (v, t)) =
  let vty = tyof tctx vctx v in
  if tyaeq vty t then t
  else tyerr (sprintf "%s checks as %s not %s" (sv v) (st vty) (st t))

and tyof tctx vctx = function
  | VVar x -> (
      match List.assoc_opt x vctx with
      | Some t -> t
      | None -> tyerr (sprintf "undeclared variable %s" x))
  | VInt _ -> TInt
  | VTup vs -> TTup (List.map (tyof_annot tctx vctx) vs)
  | VTyApp (v, o) -> (
      ty_wf o tctx;
      match tyof_annot tctx vctx v with
      | TFn { typarams = a :: bs; params = ts } ->
          TFn { typarams = bs; params = List.map (fun t -> tysub a o t) ts }
      | TFn _ ->
          tyerr
            (sprintf "type application target %s has no type parameters" (sa v))
      | _ ->
          tyerr (sprintf "type application target %s is not a function" (sa v)))
  | VPack (t1, v, TExist (a, t2)) ->
      ty_wf t1 tctx;
      let v_reifyty = tyof_annot tctx vctx v in
      let v_expectty = tysub a t1 t2 in
      if tyaeq v_expectty v_reifyty then TExist (a, t2)
      else
        tyerr
          (sprintf "declared package type %s does not match checked type %s"
             (st v_expectty) (st v_reifyty))
  | VPack _ -> tyerr "package must be an existential type"

and tyof_heap_val vctx = function
  | Code { typarams; params; body } ->
      (* well-formedness of types of parameters should be closed under this fn *)
      List.iter (fun (_, t) -> ty_wf t typarams) params;
      let fn_ty = TFn { typarams; params = List.map snd params } in
      let vctx' = params @ vctx in
      term_wf typarams vctx' body;
      fn_ty

and term_wf tctx vctx = function
  | Let (DeclVal (x, v), e) ->
      let vty = tyof_annot tctx vctx v in
      term_wf tctx ((x, vty) :: vctx) e
  | Let (DeclProj (x, v, i), e) -> (
      match tyof_annot tctx vctx v with
      | TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some ti -> term_wf tctx ((x, ti) :: vctx) e
          | None ->
              tyerr
                (sprintf "tuple type of %s cannot be indexed at \"%d\"" (sa v) i)
          )
      | _ -> tyerr (sprintf "projection target %s is not a tuple" (sa v)))
  | Let (DeclOp (x, _, v1, v2), e) -> (
      match (tyof_annot tctx vctx v1, tyof_annot tctx vctx v2) with
      | TInt, TInt -> term_wf tctx ((x, TInt) :: vctx) e
      | _ ->
          tyerr
            (sprintf "both arguments %s, %s of operation must be ints" (sa v1)
               (sa v2)))
  | Let (DeclUnpack (a, x, v), e) -> (
      match tyof_annot tctx vctx v with
      | TExist (a', t') ->
          let t = if a = a' then t' else tysub a' (TName a) t' in
          term_wf (a :: tctx) ((x, t) :: vctx) e
      | _ ->
          tyerr
            (sprintf
               "%s cannot be unpacked because it is not an existential type"
               (sa v)))
  | App (fn, vargs) -> (
      match tyof_annot tctx vctx fn with
      | TFn { typarams = []; params } ->
          if List.length params <> List.length vargs then
            tyerr
              (sprintf "expected %d formal arguments in application to %s"
                 (List.length params) (sa fn));
          List.iter2
            (fun arg param_ty ->
              let arg_ty = tyof_annot tctx vctx arg in
              if not (tyaeq arg_ty param_ty) then
                tyerr
                  (sprintf
                     "in application to %s, expected argument %s to be of type \
                      %s (found type %s)"
                     (sa fn) (sa arg) (st param_ty) (st arg_ty)))
            vargs params
      | TFn _ as fnty ->
          tyerr
            (sprintf "application to %s (%s) must have no free type variables"
               (sa fn) (st fnty))
      | _ -> tyerr (sprintf "application target %s is not a function" (sa fn)))
  | If0 (test, then', else') ->
      if not (tyaeq (tyof_annot tctx vctx test) TInt) then
        tyerr (sprintf "if0 test %s must be an int" (sa test));
      term_wf tctx vctx then';
      term_wf tctx vctx else'
  | Halt (t, v) ->
      if not (tyaeq (tyof_annot tctx vctx v) t) then
        tyerr (sprintf "halting value %s is not of type %s" (sa v) (st t))

and check_program (Prog (defs, e)) =
  let assume_ctx =
    List.map
      (fun (f, Code { typarams; params; body = _ }) ->
        (f, TFn { typarams; params = List.map snd params }))
      defs
  in
  if SSet.of_list (List.map fst defs) |> SSet.cardinal <> List.length defs then
    tyerr "duplicate letrec definitions in program";
  List.iter (fun (_, ty) -> ty_wf ty []) assume_ctx;
  List.iter2
    (fun (_, code) (f, ty) ->
      let infer = tyof_heap_val assume_ctx code in
      if not (tyaeq infer ty) then
        tyerr
          (sprintf "%s expected to have type %s, found %s" f (st ty) (st infer)))
    defs assume_ctx;
  term_wf [] assume_ctx e

(*** Eval ***)

let rec fvs_a (Annot (v, _)) = fvs_v v

and fvs_v =
  let open SSet in
  let go = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VTup vs -> List.fold_left union empty (List.map fvs_a vs)
    | VPack (_, v, _) -> fvs_a v
    | VTyApp (v, _) -> fvs_a v
  in
  go

and fvs_hv =
  let open SSet in
  function
  | Code { typarams = _; params; body } ->
      let bound = of_list (List.map fst params) in
      diff (fvs_t body) bound

and fvs_t =
  let open SSet in
  let rec go = function
    | Let (DeclVal (x, v), e) -> union (fvs_a v) (remove x (go e))
    | Let (DeclProj (x, v, _), e) -> union (fvs_a v) (remove x (go e))
    | Let (DeclOp (x, _, v1, v2), e) ->
        union (fvs_a v1) (fvs_a v2) |> union (remove x (go e))
    | Let (DeclUnpack (_, x, v), e) -> union (fvs_a v) (remove x (go e))
    | App (v, vs) -> List.fold_left union empty (List.map fvs_a (v :: vs))
    | If0 (v1, t2, t3) -> fvs_a v1 |> union (go t2) |> union (go t3)
    | Halt (_, v) -> fvs_a v
  in
  go

let subst x e =
  let rec go_a subs (Annot (v, t)) = Annot (go_v subs v, t)
  and go_v subs tm =
    match tm with
    | VVar y -> (
        match List.assoc_opt y subs with Some e -> e | None -> VVar y)
    | VInt _ -> tm
    | VTup vs -> VTup (List.map (go_a subs) vs)
    | VTyApp (v, t) -> VTyApp (go_a subs v, t)
    | VPack (t1, v, t2) -> VPack (t1, go_a subs v, t2)
  and go_t subs tm =
    let do_let y body create_let =
      if x = y then tm
      else if SSet.mem y (fvs_v e) then
        let y' = freshen y (SSet.union (fvs_v e) (fvs_t body)) in
        create_let y' (go_t ((y, VVar y') :: subs) body)
      else create_let y (go_t subs body)
    in
    match tm with
    | Let (DeclVal (y, v), body) ->
        do_let y body (fun y' body' -> Let (DeclVal (y', go_a subs v), body'))
    | Let (DeclProj (y, v, i), body) ->
        do_let y body (fun y' body' ->
            Let (DeclProj (y', go_a subs v, i), body'))
    | Let (DeclOp (y, op, e1, e2), body) ->
        do_let y body (fun y' body' ->
            Let (DeclOp (y', op, go_a subs e1, go_a subs e2), body'))
    | Let (DeclUnpack (a, y, v), body) ->
        do_let y body (fun y' body' ->
            Let (DeclUnpack (a, y', go_a subs v), body'))
    | App (v, vs) -> App (go_a subs v, List.map (go_a subs) vs)
    | If0 (v, t1, t2) -> If0 (go_a subs v, go_t subs t1, go_t subs t2)
    | Halt (t, v) -> Halt (t, go_a subs v)
  in
  go_t [ (x, e) ]

let evalerr what = raise (EvalErr ("H eval error: " ^ what))

let unwrap_value = function
  | VVar x -> evalerr ("unresolved variable " ^ x)
  | v -> v

let step defs = function
  | Let (DeclVal (x, Annot (v, _)), body) -> subst x v body
  | Let (DeclProj (x, Annot (VTup vs, _), i), body) ->
      let (Annot (vi, _)) = List.nth vs (i - 1) in
      subst x vi body
  | Let (DeclOp (x, op, Annot (VInt i, _), Annot (VInt j, _)), body) ->
      subst x (VInt (do_op op i j)) body
  | Let (DeclUnpack (_, x, Annot (VPack (_, Annot (v, _), _), _)), body) ->
      subst x v body
  | App (Annot (VVar f, _), args) ->
      let (Code { params; body; _ }) = List.assoc f defs in
      let args = List.map (fun (Annot (v, _)) -> v) args in
      List.fold_left2 (fun body (p, _) a -> subst p a body) body params args
  | If0 (Annot (VInt 0, _), t2, _) -> t2
  | If0 (Annot (VInt _, _), _, t3) -> t3
  | t -> evalerr (sprintf "term %s is stuck" (se t))

let rec eval defs = function
  | Halt (_, Annot (v, _)) -> unwrap_value v
  | e -> eval defs (step defs e)

let eval_top (Prog (defs, body)) = eval defs body

(*** Translation from C ***)

let rec trans_ty = function
  | C.TName x -> TName x
  | C.TInt -> TInt
  | C.TFn { typarams; params } ->
      TFn { typarams; params = List.map trans_ty params }
  | C.TTup tys -> TTup (List.map trans_ty tys)
  | C.TExist (a, t) -> TExist (a, trans_ty t)

let rec trans_annot_val fresh hoist = function
  | C.Annot (v, t) -> Annot (trans_val fresh hoist v, trans_ty t)

and trans_val fresh hoist = function
  | C.VVar x -> VVar x
  | C.VInt i -> VInt i
  | C.VTup vs -> VTup (List.map (trans_annot_val fresh hoist) vs)
  | C.VTyApp (v, t) -> VTyApp (trans_annot_val fresh hoist v, trans_ty t)
  | C.VPack (t1, v, t2) ->
      VPack (trans_ty t1, trans_annot_val fresh hoist v, trans_ty t2)
  | C.VFix { name; typarams; params; body } ->
      let fname = fresh ("_" ^ name) in
      let params' = List.map (fun (p, t) -> (p, trans_ty t)) params in
      let body' = trans_term fresh hoist body |> subst name (VVar fname) in
      let code = Code { typarams; params = params'; body = body' } in
      hoist fname code;
      VVar fname

and trans_decl f h = function
  | C.DeclVal (x, v) -> DeclVal (x, trans_annot_val f h v)
  | C.DeclProj (x, v, i) -> DeclProj (x, trans_annot_val f h v, i)
  | C.DeclOp (x, op, v1, v2) ->
      DeclOp (x, op, trans_annot_val f h v1, trans_annot_val f h v2)
  | C.DeclUnpack (a, x, v) -> DeclUnpack (a, x, trans_annot_val f h v)

and trans_term f h = function
  | C.Let (d, t) -> Let (trans_decl f h d, trans_term f h t)
  | C.App (v, vs) ->
      App (trans_annot_val f h v, List.map (trans_annot_val f h) vs)
  | C.If0 (v, t1, t2) ->
      If0 (trans_annot_val f h v, trans_term f h t1, trans_term f h t2)
  | C.Halt (t, v) -> Halt (trans_ty t, trans_annot_val f h v)

let all_names =
  let open C in
  let open SSet in
  let rec goa = function Annot (v, t) -> union (gov v) (goty t)
  and gov = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VTup ts -> List.fold_left union empty (List.map goa ts)
    | VFix { name; typarams; params; body } ->
        add name (of_list typarams)
        |> union (of_list (List.map fst params))
        |> union (got body)
    | VTyApp (v, t) -> union (goa v) (goty t)
    | VPack (t1, v, t2) -> union (goty t1) (goty t2) |> union (goa v)
  and god = function
    | DeclVal (x, v) | DeclProj (x, v, _) -> add x (goa v)
    | DeclOp (x, _, v1, v2) -> union (goa v1) (goa v2) |> add x
    | DeclUnpack (a, x, v) -> goa v |> add a |> add x
  and got = function
    | Let (d, t) -> union (god d) (got t)
    | App (v, vs) -> List.fold_left union (goa v) (List.map goa vs)
    | If0 (v, t1, t2) -> union (got t1) (got t2) |> union (goa v)
    | Halt (t, v) -> union (goty t) (goa v)
  and goty = function
    | TInt -> empty
    | TName x -> singleton x
    | TFn { typarams; params } ->
        union (of_list typarams)
          (List.fold_left union empty (List.map goty params))
    | TTup ts -> List.fold_left union empty (List.map goty ts)
    | TExist (a, t) -> add a (goty t)
  in
  got

let trans_top u =
  let fresh = fresh_generator (all_names u) in
  let defs = ref [] in
  let hoist f code = defs := (f, code) :: !defs in
  let e = trans_term fresh hoist u in
  let defs = List.rev !defs in
  Prog (defs, e)
