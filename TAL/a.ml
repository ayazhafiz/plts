open Util

type iflag = Init | Uninit

type ty =
  | TName of string
  | TInt
  | TFn of { typarams : string list; params : ty list }
  | TTup of (ty * iflag) list
  | TExist of string * ty

type annot_val = Annot of value * ty

and value =
  | VVar of string
  | VInt of int
  | VTyApp of annot_val * ty
  | VPack of ty * annot_val * ty
      (** pack [t1, v] as ∃a.t2   (t1 is the true a) *)

and heap_value =
  | HCode of {
      typarams : string list;
      params : (string * ty) list;
      body : term;
    }
  | HTup of annot_val list

and decl =
  | DeclVal of string * annot_val  (** x = v *)
  | DeclProj of string * annot_val * int  (** x = v.i *)
  | DeclOp of string * op * annot_val * annot_val  (** x = v R v *)
  | DeclUnpack of string * string * annot_val  (** [a, x] = unpack v *)
  | DeclTupMalloc of string * ty list
  | DeclTupInit of string * annot_val * int * annot_val
(* x = v1[i] <- v2 *)

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
        && List.for_all2
             (fun (t1, i1) (t2, i2) -> i1 = i2 && go s1 s2 (t1, t2))
             t1 t2
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
          (fun i (t, iflag) ->
            if iflag = Uninit then fprintf f "!";
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
  | HCode { typarams; params; body } ->
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
  | HTup vs ->
      fprintf f "@[<hov 2>(";
      let lasti = List.length vs - 1 in
      List.iteri
        (fun i e ->
          pp_annot f e;
          if i <> lasti then fprintf f ",@ ")
        vs;
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
  | DeclTupMalloc (x, ts) ->
      fprintf f "@[<hov 2>%s =@ malloc@[<hov 2><" x;
      let lasti = List.length ts - 1 in
      List.iteri
        (fun i t ->
          pp_ty f t;
          if i <> lasti then fprintf f ",@ ")
        ts;
      fprintf f ">@]@]"
  | DeclTupInit (x, v1, i, v2) ->
      fprintf f "@[<hov 2>%s =@ " x;
      pp_annot f v1;
      fprintf f ".%d <- " i;
      pp_annot f v2;
      fprintf f "@]"

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

let string_of_heap_value e = with_buffer (fun f -> pp_heap_value f e) 80

let string_of_term e = with_buffer (fun f -> pp_term f e) 80

let string_of_ty t = with_buffer (fun f -> pp_ty f t) 80

let string_of_program p = with_buffer (fun f -> pp_program f p) 80

(*** Typecheck ***)

let tyerr what = raise (TyErr ("H type error: " ^ what))

let rec ftv = function
  | TInt -> SSet.empty
  | TName a -> SSet.singleton a
  | TTup ts ->
      List.map fst ts |> List.map ftv |> List.fold_left SSet.union SSet.empty
  | TFn { typarams; params } ->
      let ftvs = SSet.(List.fold_left union empty (List.map ftv params)) in
      SSet.(diff ftvs (of_list typarams))
  | TExist (a, t) -> SSet.remove a (ftv t)

let tysub a ta t =
  let rec go subs = function
    | TInt -> TInt
    | TName a -> (
        match List.assoc_opt a subs with Some t -> t | None -> TName a)
    | TTup ts -> TTup (List.map (fun (t, i) -> (go subs t, i)) ts)
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

let sh = string_of_heap_value

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
  | HCode { typarams; params; body } ->
      (* well-formedness of types of parameters should be closed under this fn *)
      List.iter (fun (_, t) -> ty_wf t typarams) params;
      let fn_ty = TFn { typarams; params = List.map snd params } in
      let vctx' = params @ vctx in
      term_wf typarams vctx' body;
      fn_ty
  | HTup vs -> TTup (List.map (fun v -> (tyof_annot [] vctx v, Init)) vs)

and term_wf tctx vctx = function
  | Let (DeclVal (x, v), e) ->
      let vty = tyof_annot tctx vctx v in
      term_wf tctx ((x, vty) :: vctx) e
  | Let (DeclProj (x, v, i), e) -> (
      match tyof_annot tctx vctx v with
      | TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some (ti, Init) -> term_wf tctx ((x, ti) :: vctx) e
          | Some (_, Uninit) ->
              tyerr (sprintf "tuple %s is not initialized at index %d" (sa v) i)
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
  | Let (DeclTupMalloc (x, ts), e) ->
      List.iter (fun t -> ty_wf t tctx) ts;
      let tupty = TTup (List.map (fun t -> (t, Uninit)) ts) in
      term_wf tctx ((x, tupty) :: vctx) e
  | Let (DeclTupInit (x, v1, i, v2), e) -> (
      match tyof_annot tctx vctx v1 with
      | TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some (t, _) ->
              if tyaeq t (tyof_annot tctx vctx v2) then
                let fields' =
                  List.mapi
                    (fun j (t, iflag) ->
                      if j = i - 1 then (t, Init) else (t, iflag))
                    ts
                in
                term_wf tctx ((x, TTup fields') :: vctx) e
              else
                tyerr
                  (sprintf "%s cannot initialize %s.%d, which expects type %s"
                     (sa v2) (sa v1) i (st t))
          | None ->
              tyerr (sprintf "%d is out of range of the tuple %s" i (sa v1)))
      | _ ->
          tyerr
            (sprintf "%s cannot be projected because it is not a tuple" (sa v1))
      )
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
    List.filter_map
      (function
        | f, HCode { typarams; params; body = _ } ->
            Some (f, TFn { typarams; params = List.map snd params })
        | _, HTup _ -> None)
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
    | VPack (_, v, _) -> fvs_a v
    | VTyApp (v, _) -> fvs_a v
  in
  go

and fvs_hv =
  let open SSet in
  function
  | HCode { typarams = _; params; body } ->
      let bound = of_list (List.map fst params) in
      diff (fvs_t body) bound
  | HTup vs -> List.fold_left union empty (List.map fvs_a vs)

and fvs_t =
  let open SSet in
  let rec go = function
    | Let (DeclVal (x, v), e) -> union (fvs_a v) (remove x (go e))
    | Let (DeclProj (x, v, _), e) -> union (fvs_a v) (remove x (go e))
    | Let (DeclOp (x, _, v1, v2), e) ->
        union (fvs_a v1) (fvs_a v2) |> union (remove x (go e))
    | Let (DeclUnpack (_, x, v), e) -> union (fvs_a v) (remove x (go e))
    | Let (DeclTupMalloc (x, _), e) -> remove x (go e)
    | Let (DeclTupInit (x, v1, _, v2), e) ->
        union (fvs_a v1) (fvs_a v2) |> union (remove x (go e))
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
    | Let (DeclTupMalloc (y, ts), body) ->
        do_let y body (fun y' body' -> Let (DeclTupMalloc (y', ts), body'))
    | Let (DeclTupInit (y, v1, i, v2), body) ->
        do_let y body (fun y' body' ->
            Let (DeclTupInit (y', go_a subs v1, i, go_a subs v2), body'))
    | App (v, vs) -> App (go_a subs v, List.map (go_a subs) vs)
    | If0 (v, t1, t2) -> If0 (go_a subs v, go_t subs t1, go_t subs t2)
    | Halt (t, v) -> Halt (t, go_a subs v)
  in
  go_t [ (x, e) ]

let subst_v x e v =
  match subst x e (App (Annot (v, TName "dummy"), [])) with
  | App (Annot (v', TName "dummy"), _) -> v'
  | _ -> failwith "bad subst"

let evalerr what = raise (EvalErr ("H eval error: " ^ what))

let unwrap_value = function
  | VVar x -> evalerr ("unresolved variable " ^ x)
  | v -> v

type rt_heap_val = RTup of value IntMap.t

let print_val = function
  | RTup fields ->
      let fields =
        IntMap.to_seq fields |> List.of_seq
        |> List.map (fun (i, v) -> sprintf "%d: %s" i (sv v))
      in
      sprintf "(%s)" (String.concat ", " fields)

type rt_heap = (string * rt_heap_val) list

let heap_subst x v heap =
  List.map
    (fun (n, hv) ->
      let n' = match v with VVar v when n = x -> v | _ -> n in
      let hv' = match hv with RTup vs -> RTup (IntMap.map (subst_v x v) vs) in
      (n', hv'))
    heap

let print_heap h =
  String.concat "; "
    (List.map (fun (x, hv) -> sprintf "%s => %s" x (print_val hv)) h)

let heapfind x heap hint =
  match List.assoc_opt x heap with
  | Some v -> v
  | None ->
      evalerr
        (sprintf "%s not found in heap %s (in %s)" x (print_heap heap) hint)

let step defs heap = function
  | Let (DeclVal (x, Annot (v, _)), body) ->
      (subst x v body, heap_subst x v heap)
  | Let (DeclProj (x, Annot (VVar tup, _), i), body) as dec -> (
      let (RTup fields) = heapfind tup heap (se dec) in
      match IntMap.find_opt i fields with
      | Some vi -> (subst x vi body, heap_subst x vi heap)
      | None ->
          evalerr
            (sprintf "field %d does not exist on %s" i (print_val (RTup fields)))
      )
  | Let (DeclOp (x, op, Annot (VInt i, _), Annot (VInt j, _)), body) ->
      let v = VInt (do_op op i j) in
      (subst x v body, heap_subst x v heap)
  | Let (DeclUnpack (_, x, Annot (VPack (_, Annot (v, _), _), _)), body) ->
      (subst x v body, heap_subst x v heap)
  | Let (DeclTupMalloc (x, _), body) ->
      let heap' = (x, RTup IntMap.empty) :: heap in
      (body, heap')
  | Let (DeclTupInit (x, Annot (VVar tup, _), i, Annot (v, _)), body) ->
      let (RTup fields) = List.assoc tup heap in
      let x_tup = RTup (IntMap.add i v fields) in
      let heap' = (x, x_tup) :: heap in
      (body, heap')
  | App (Annot (VVar f, _), args) -> (
      match List.assoc f defs with
      | HCode { params; body; _ } ->
          let body', heap' =
            List.fold_left2
              (fun (body, heap) (p, _) (Annot (rawv, _)) ->
                (subst p rawv body, heap_subst p rawv heap))
              (body, heap) params args
          in
          (body', heap')
      | hv -> evalerr (sprintf "cannot call %s" (sh hv)))
  | If0 (Annot (VInt 0, _), t2, _) -> (t2, heap)
  | If0 (Annot (VInt _, _), _, t3) -> (t3, heap)
  | t -> evalerr (sprintf "term %s is stuck" (se t))

let rec eval defs heap i = function
  | Halt (_, Annot (v, _)) -> unwrap_value v
  | e ->
      if i = 2015 then failwith (sprintf "stuck at %s\n\n" (se e))
      else
        let e', heap' = step defs heap e in
        eval defs heap' (i + 1) e'

let eval_top (Prog (defs, body)) = eval defs [] 0 body

(*** Translation from C ***)

let rec trans_ty = function
  | H.TName x -> TName x
  | H.TInt -> TInt
  | H.TFn { typarams; params } ->
      TFn { typarams; params = List.map trans_ty params }
  | H.TTup tys -> TTup (List.map (fun t -> (trans_ty t, Init)) tys)
  | H.TExist (a, t) -> TExist (a, trans_ty t)

let seq_decls = List.fold_right (fun d e -> Let (d, e))

let rec trans_aval fresh = function
  | H.Annot (H.VVar x, t) -> ([], Annot (VVar x, trans_ty t))
  | H.Annot (H.VInt i, t) -> ([], Annot (VInt i, trans_ty t))
  | H.Annot (H.VTyApp (v, o), t) ->
      let d, v' = trans_aval fresh v in
      (d, Annot (VTyApp (v', trans_ty o), trans_ty t))
  | H.Annot (H.VPack (t, v, t'), t'') ->
      let d, v' = trans_aval fresh v in
      (d, Annot (VPack (trans_ty t, v', trans_ty t'), trans_ty t''))
  | H.Annot (H.VTup us, t) ->
      let ts = List.map (fun (H.Annot (_, t)) -> trans_ty t) us in
      let dss, vs = List.map (trans_aval fresh) us |> List.split in
      let ds1 = List.concat dss in
      let y0 = fresh "y" in
      let ttup0 = TTup (List.map (fun t -> (t, Uninit)) ts) in
      let malloc = DeclTupMalloc (y0, ts) in
      let rec goinit i yi_1 ttupi_1 = function
        | vi :: vrest ->
            let yi = fresh "y" in
            let ttupi =
              TTup
                (List.mapi
                   (fun j t -> if j + 1 <= i then (t, Init) else (t, Uninit))
                   ts)
            in
            let dec_i = DeclTupInit (yi, Annot (VVar yi_1, ttupi_1), i, vi) in
            if vrest = [] then ([ dec_i ], yi)
            else
              let dec_rest, yn = goinit (i + 1) yi ttupi vrest in
              (dec_i :: dec_rest, yn)
        | [] -> (* empty tuple type *) ([], y0)
      in
      let ds2, yn = goinit 1 y0 ttup0 vs in
      let ds = ds1 @ [ malloc ] @ ds2 in
      (ds, Annot (VVar yn, trans_ty t))

and trans_heap fresh = function
  | H.Code { typarams; params; body } ->
      let params' = List.map (fun (p, t) -> (p, trans_ty t)) params in
      let body' = trans_term fresh body in
      HCode { typarams; params = params'; body = body' }

and trans_decl fresh = function
  | H.DeclVal (x, v) ->
      let d, v' = trans_aval fresh v in
      d @ [ DeclVal (x, v') ]
  | H.DeclProj (x, v, i) ->
      let d, v' = trans_aval fresh v in
      d @ [ DeclProj (x, v', i) ]
  | H.DeclOp (x, op, v1, v2) ->
      let d1, v1' = trans_aval fresh v1 in
      let d2, v2' = trans_aval fresh v2 in
      d1 @ d2 @ [ DeclOp (x, op, v1', v2') ]
  | H.DeclUnpack (a, x, v) ->
      let d, v' = trans_aval fresh v in
      d @ [ DeclUnpack (a, x, v') ]

and trans_term fresh = function
  | H.Let (d, e) -> seq_decls (trans_decl fresh d) (trans_term fresh e)
  | H.App (v, vs) ->
      let d, v' = trans_aval fresh v in
      let ds, vs' = List.map (trans_aval fresh) vs |> List.split in
      seq_decls (List.concat (d :: ds)) (App (v', vs'))
  | H.If0 (v, t1, t2) ->
      let d, v' = trans_aval fresh v in
      seq_decls d (If0 (v', trans_term fresh t1, trans_term fresh t2))
  | H.Halt (t, v) ->
      let d, v' = trans_aval fresh v in
      seq_decls d (Halt (trans_ty t, v'))

let all_names =
  let open H in
  let open SSet in
  let rec goa = function Annot (v, t) -> union (gov v) (goty t)
  and gov = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VTup ts -> List.fold_left union empty (List.map goa ts)
    | VTyApp (v, t) -> union (goa v) (goty t)
    | VPack (t1, v, t2) -> union (goty t1) (goty t2) |> union (goa v)
  and goh = function
    | Code { typarams; params; body } ->
        of_list typarams
        |> union (of_list (List.map fst params))
        |> union (got body)
  and goprog = function
    | Prog (defs, e) ->
        let defs_names = List.map (fun (f, heap) -> add f (goh heap)) defs in
        List.fold_left union empty defs_names |> union (got e)
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
  goprog

let trans_top (H.Prog (defs, e) as u) =
  let fresh = fresh_generator (all_names u) in
  let defs' = List.map (fun (x, h) -> (x, trans_heap fresh h)) defs in
  let e' = trans_term fresh e in
  Prog (defs', e')
