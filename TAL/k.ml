open Util

type ty =
  | TName of string
  | TInt
  | TFn of { typarams : string list; body : ty list }  (** ∀<as>.(ts)->void *)
  | TTup of ty list

type value =
  | VVar of string
  | VInt of int
  | VFix of {
      name : string;
      typarams : string list;
      params : (string * ty) list;
      body : term;
    }
  | VTup of value list
  | VAnnot of value * ty

and decl =
  | DeclVal of string * value  (** x = v *)
  | DeclProj of string * value * int  (** x = v.i *)
  | DeclOp of string * op * value * value  (** x = v R v *)

and term =
  | Let of decl * term  (** let d in e *)
  | App of value * ty list * value list  (** v<ts>(vs) *)
  | If0 of value * term * term
  | Halt of ty * value

(*** Print ***)

let pp_ty f =
  let open Format in
  let rec go = function
    | TInt -> fprintf f "int"
    | TName a -> pp_print_string f a
    | TFn { typarams; body } ->
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
        let lasti = List.length body - 1 in
        List.iteri
          (fun i t ->
            go t;
            if i <> lasti then fprintf f ",@ ")
          body;
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
  in
  go

let pp_op f =
  let open Format in
  function
  | Plus -> pp_print_string f "+"
  | Minus -> pp_print_string f "-"
  | Times -> pp_print_string f "*"

let rec pp_value f =
  let open Format in
  let rec go = function
    | VVar x -> pp_print_string f x
    | VInt i -> pp_print_int f i
    | VFix { name; typarams; params; body } ->
        let header =
          if String.index_opt name '_' = Some 0 then "λ"
          else sprintf "fix\n        %s" name
        in
        fprintf f "@[<hov 2>(%s" header;
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
    | VTup es ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length es - 1 in
        List.iteri
          (fun i e ->
            go e;
            if i <> lasti then fprintf f ",@ ")
          es;
        fprintf f ")@]"
    | VAnnot (v, t) ->
        fprintf f "@[<hov 2>(";
        go v;
        fprintf f ":@ ";
        pp_ty f t;
        fprintf f ")@]"
  in
  go

and pp_decl f =
  let open Format in
  function
  | DeclVal (s, v) ->
      fprintf f "@[<hov 2>%s =@ " s;
      pp_value f v;
      fprintf f "@]"
  | DeclProj (s, v, i) ->
      fprintf f "@[<hov 2>%s =@ " s;
      pp_value f v;
      fprintf f ".%d@]" i
  | DeclOp (s, op, v1, v2) ->
      fprintf f "@[<hov 2>%s =@ @[<hov 2>" s;
      pp_value f v1;
      fprintf f " ";
      pp_op f op;
      fprintf f "@ ";
      pp_value f v2;
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
    | App (v, tyargs, params) ->
        fprintf f "@[<hov 2>(";
        pp_value f v;
        if List.length tyargs <> 0 then (
          pp_print_string f "<";
          let lasti = List.length tyargs - 1 in
          List.iteri
            (fun i t ->
              pp_ty f t;
              if i <> lasti then fprintf f ",@ ")
            tyargs;
          fprintf f ">@,");
        fprintf f "(";
        let lasti = List.length params - 1 in
        List.iteri
          (fun i p ->
            pp_value f p;
            if i <> lasti then fprintf f ",@ ")
          params;
        fprintf f "))@]"
    | If0 (test, then', else') ->
        fprintf f "@[<hov>(if0@;<1 2>@[";
        pp_value f test;
        fprintf f "@]@ then@;<1 2>@[";
        go then';
        fprintf f "@]@ else@;<1 2>@[";
        go else';
        fprintf f "@])@]"
    | Halt (t, v) ->
        fprintf f "@[<hov 2>halt<@,";
        pp_ty f t;
        fprintf f ">@,";
        pp_value f v;
        fprintf f "@]"
  in
  go

let string_of_value e = with_buffer (fun f -> pp_value f e) 80

let string_of_term e = with_buffer (fun f -> pp_term f e) 80

let string_of_ty t = with_buffer (fun f -> pp_ty f t) 80

(*** Typecheck ***)

exception TyErr of string

let tyerr what = raise (TyErr ("K type error: " ^ what))

let rec ftv = function
  | TInt -> SSet.empty
  | TName a -> SSet.singleton a
  | TTup ts -> List.fold_left SSet.union SSet.empty (List.map ftv ts)
  | TFn { typarams; body } ->
      let ftvs = SSet.(List.fold_left union empty (List.map ftv body)) in
      SSet.(diff ftvs (of_list typarams))

let tysub a ta t =
  let rec go subs = function
    | TInt -> TInt
    | TName a -> (
        match List.assoc_opt a subs with Some t -> t | None -> TName a)
    | TTup ts -> TTup (List.map (go subs) ts)
    | TFn { typarams; body } ->
        if List.mem a typarams then TFn { typarams; body }
        else if List.exists (fun a' -> SSet.mem a' (ftv ta)) typarams then
          (* Something like (∀(a', b'). a)[a/a']. Naive substitution would result in (∀a'. a'),
             transforming the constant quanitifier at a' in the outer scope to the identity.
             Find a fresh name a'->a'' inside the binder. *)
          let ftv_ta = ftv ta in
          let ftv_body =
            SSet.(List.fold_left union empty (List.map ftv body))
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
            { typarams = typarams'; body = List.map (go (newsubs @ subs)) body }
        else TFn { typarams; body = List.map (go subs) body }
  in
  go [ (a, ta) ] t

let sv = string_of_value

let se = string_of_term

let st = string_of_ty

let sprintf = Printf.sprintf

let ty_wf t tctx =
  if not (SSet.subset (ftv t) (SSet.of_list tctx)) then
    tyerr (Printf.sprintf "%s is malformed" (string_of_ty t))

let rec tyof tctx vctx = function
  | VAnnot (v, t) ->
      let vty = tyof tctx vctx v in
      if vty = t then t
      else tyerr (sprintf "%s checks as %s not %s" (sv v) (st vty) (st t))
  | VVar x -> (
      match List.assoc_opt x vctx with
      | Some t -> t
      | None -> tyerr (sprintf "undeclared variable %s" x))
  | VInt _ -> TInt
  | VFix { name; typarams; params; body } ->
      List.iter (fun (_, t) -> ty_wf t (typarams @ tctx)) params;
      let fn_ty = TFn { typarams; body = List.map snd params } in
      let tctx' = typarams @ tctx in
      let vctx' = (name, fn_ty) :: params @ vctx in
      term_wf tctx' vctx' body;
      fn_ty
  | VTup vs -> TTup (List.map (tyof tctx vctx) vs)

and term_wf tctx vctx = function
  | Let (DeclVal (x, v), e) ->
      let vty = tyof tctx vctx v in
      term_wf tctx ((x, vty) :: vctx) e
  | Let (DeclProj (x, v, i), e) -> (
      match tyof tctx vctx v with
      | TTup ts -> (
          match List.nth_opt ts (i - 1) with
          | Some ti -> term_wf tctx ((x, ti) :: vctx) e
          | None ->
              tyerr
                (sprintf "tuple type of %s cannot be indexed at \"%d\"" (sv v) i)
          )
      | _ -> tyerr (sprintf "projection target %s is not a tuple" (sv v)))
  | Let (DeclOp (x, _, v1, v2), e) -> (
      match (tyof tctx vctx v1, tyof tctx vctx v2) with
      | TInt, TInt -> term_wf tctx ((x, TInt) :: vctx) e
      | _ ->
          tyerr
            (sprintf "both arguments %s, %s of operation must be ints" (sv v1)
               (sv v2)))
  | App (fn, tyargs, vargs) -> (
      List.iter (fun t -> ty_wf t tctx) tyargs;
      match tyof tctx vctx fn with
      | TFn { typarams; body } ->
          if List.length typarams <> List.length tyargs then
            tyerr
              (sprintf "expected %d type arguments in application to %s"
                 (List.length typarams) (sv fn));
          if List.length body <> List.length vargs then
            tyerr
              (sprintf "expected %d formal arguments in application to %s"
                 (List.length body) (sv fn));
          let typaram_insts = List.combine typarams tyargs in
          List.iter2
            (fun arg param_ty ->
              let arg_ty = tyof tctx vctx arg in
              let inst_param_ty =
                List.fold_left
                  (fun pty (tp, ta) -> tysub tp ta pty)
                  param_ty typaram_insts
              in
              if arg_ty <> inst_param_ty then
                tyerr
                  (sprintf
                     "in application to %s, expected argument %s to be of type \
                      %s (found type %s)"
                     (sv fn) (sv arg) (st inst_param_ty) (st arg_ty)))
            vargs body
      | _ -> tyerr (sprintf "application target %s is not a function" (sv fn)))
  | If0 (test, then', else') ->
      if tyof tctx vctx test <> TInt then
        tyerr (sprintf "if0 test %s must be an int" (sv test));
      term_wf tctx vctx then';
      term_wf tctx vctx else'
  | Halt (t, v) ->
      if tyof tctx vctx v <> t then
        tyerr (sprintf "halting value %s is not of type %s" (sv v) (st t))

(*** Translation from F ***)

let rec trans_ty = function
  | F.TName x -> TName x
  | F.TInt -> TInt
  | F.TArrow (t1, t2) ->
      TFn { typarams = []; body = [ trans_ty t1; trans_ty_cont t2 ] }
  | F.TAll (a, t) -> TFn { typarams = [ a ]; body = [ trans_ty_cont t ] }
  | F.TTup ts -> TTup (List.map trans_ty ts)

and trans_ty_cont t = TFn { typarams = []; body = [ trans_ty t ] }

let reify_cont fresh k inty =
  let cont = fresh "cont" in
  let v = fresh "v" in
  let body = k (VAnnot (VVar v, inty)) in
  VAnnot
    ( VFix { name = cont; typarams = []; params = [ (v, inty) ]; body },
      TFn { typarams = []; body = [ inty ] } )

let rec trans_exp fresh e k =
  match e with
  | F.Annot (F.Var y, t) ->
      let kt = trans_ty t in
      k (VAnnot (VVar y, kt))
  | F.Annot (F.Int i, t) ->
      let kt = trans_ty t in
      k (VAnnot (VInt i, kt))
  | F.Annot
      (F.Fix { name = x; param = x1; param_ty = t1; ret_ty = t2; body = e }, t)
    ->
      let kt = trans_ty t in
      let kt1 = trans_ty t1 in
      let kct2 = trans_ty_cont t2 in
      let c = fresh "c" in
      let kfix =
        VFix
          {
            name = x;
            typarams = [];
            params = [ (x1, kt1); (c, kct2) ];
            body =
              trans_exp fresh e (fun v ->
                  App (VAnnot (VVar c, kct2), [], [ v ]));
          }
      in
      k (VAnnot (kfix, kt))
  | F.Annot (F.App (ut1, ut2), t) ->
      let kapp_out_t = trans_ty t in
      trans_exp fresh ut1 (fun x1 ->
          trans_exp fresh ut2 (fun x2 ->
              App (x1, [], [ x2; reify_cont fresh k kapp_out_t ])))
  | F.Annot (F.TyAbs (a, (F.Annot (_, t) as ut)), t') ->
      let kct = trans_ty_cont t in
      let kt' = trans_ty_cont t' in
      let c = fresh "c" in
      let f = fresh "_" in
      let kfix =
        VFix
          {
            name = f;
            typarams = [ a ];
            params = [ (c, kct) ];
            body =
              trans_exp fresh ut (fun v ->
                  App (VAnnot (VVar c, kct), [], [ v ]));
          }
      in
      k (VAnnot (kfix, kt'))
  | F.Annot (F.TyApp (ut, o), t') ->
      let ko = trans_ty o in
      let ktyapp_out_t = trans_ty t' in
      trans_exp fresh ut (fun x ->
          App (x, [ ko ], [ reify_cont fresh k ktyapp_out_t ]))
  | F.Annot (F.Tup uts, t) ->
      let kt = trans_ty t in
      let rec go ktup = function
        | [] -> ktup []
        | uti :: uts ->
            trans_exp fresh uti (fun v -> go (fun vs -> ktup (v :: vs)) uts)
      in
      go (fun vs -> k (VAnnot (VTup vs, kt))) uts
  | F.Annot (F.Proj (ut, i), t') ->
      let y = fresh "y" in
      let kt' = trans_ty t' in
      trans_exp fresh ut (fun x ->
          Let (DeclProj (y, x, i), k (VAnnot (VVar y, kt'))))
  | F.Annot (F.Op (op, e1, e2), _) ->
      let y = fresh "y" in
      trans_exp fresh e1 (fun x1 ->
          trans_exp fresh e2 (fun x2 ->
              Let (DeclOp (y, op, x1, x2), k (VAnnot (VVar y, TInt)))))
  | F.Annot (F.If0 (e1, e2, e3), _) ->
      trans_exp fresh e1 (fun x ->
          If0 (x, trans_exp fresh e2 k, trans_exp fresh e3 k))
  | _ -> failwith "unelaborated F expression"

let all_names =
  let open SSet in
  let rec gotm = function
    | F.Annot (e, t) -> union (gotm e) (goty t)
    | F.Var x -> singleton x
    | F.Int _ -> empty
    | F.Fix { name; param; param_ty; ret_ty; body } ->
        gotm body
        |> union (goty ret_ty)
        |> union (goty param_ty)
        |> add param |> add name
    | F.App (e1, e2) -> union (gotm e1) (gotm e2)
    | F.TyAbs (a, e) -> add a (gotm e)
    | F.TyApp (e, t) -> union (gotm e) (goty t)
    | F.Tup ts -> List.fold_left union empty (List.map gotm ts)
    | F.Proj (e, _) -> gotm e
    | F.Op (_, e1, e2) -> union (gotm e1) (gotm e2)
    | F.If0 (e1, e2, e3) -> gotm e1 |> union (gotm e2) |> union (gotm e3)
  and goty = function
    | F.TInt -> empty
    | F.TName x -> singleton x
    | F.TArrow (t1, t2) -> union (goty t1) (goty t2)
    | F.TAll (a, t) -> add a (goty t)
    | F.TTup ts -> List.fold_left union empty (List.map goty ts)
  in
  gotm

let trans_top u =
  let fresh =
    let used = ref (all_names u) in
    fun hint ->
      let rec gen i =
        let cand = if i = 0 then hint else hint ^ string_of_int i in
        if SSet.mem cand !used then gen (i + 1)
        else (
          used := SSet.add cand !used;
          cand)
      in
      gen 0
  in
  match u with
  | F.Annot (_, t) ->
      let kt = trans_ty t in
      trans_exp fresh u (fun x -> Halt (kt, x))
  | _ -> failwith "unelaborated F expression"

(*** Eval ***)

let rec fvs_v =
  let open SSet in
  let rec go = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VFix { name; typarams = _; params; body } ->
        let bound = add name (of_list (List.map fst params)) in
        diff (fvs_t body) bound
    | VTup vs -> List.fold_left union empty (List.map go vs)
    | VAnnot (v, _) -> go v
  in
  go

and fvs_t =
  let open SSet in
  let rec go = function
    | Let (DeclVal (x, v), e) -> union (fvs_v v) (remove x (go e))
    | Let (DeclProj (x, v, _), e) -> union (fvs_v v) (remove x (go e))
    | Let (DeclOp (x, _, v1, v2), e) ->
        union (fvs_v v1) (fvs_v v2) |> union (remove x (go e))
    | App (v, _, vs) -> List.fold_left union empty (List.map fvs_v (v :: vs))
    | If0 (v1, t2, t3) -> fvs_v v1 |> union (go t2) |> union (go t3)
    | Halt (_, v) -> fvs_v v
  in
  go

let subst x e =
  let rec go_v subs tm =
    match tm with
    | VVar y -> (
        match List.assoc_opt y subs with Some e -> e | None -> VVar y)
    | VInt _ -> tm
    | VFix { name; typarams; params; body } ->
        if x = name || List.exists (fun (p, _) -> p = x) params then tm
        else
          let fvs_e = fvs_v e in
          let used = SSet.union fvs_e (fvs_t body) in
          let vs = name :: List.map fst params in
          let vs', esubs =
            List.map
              (fun v ->
                if SSet.mem v fvs_e then
                  let v' = freshen v used in
                  (v', Some (v, VVar v'))
                else (v, None))
              vs
            |> List.split
          in
          let esubs = List.filter_map Fun.id esubs in
          let name', params' =
            match vs' with n :: p -> (n, p) | _ -> failwith "unreachable"
          in
          let params' = List.map2 (fun p (_, t) -> (p, t)) params' params in
          VFix
            {
              name = name';
              typarams;
              params = params';
              body = go_t (esubs @ subs) body;
            }
    | VTup vs -> VTup (List.map (go_v subs) vs)
    | VAnnot (v, t) -> VAnnot (go_v subs v, t)
  and go_t subs tm =
    let do_let y body create_let =
      if x = y then tm
      else if SSet.mem y (fvs_v e) then
        let y' = freshen y (SSet.union (fvs_v e) (fvs_t body)) in
        create_let y' [ (y, VVar y') ]
      else create_let y []
    in
    match tm with
    | Let (DeclVal (y, v), body) ->
        do_let y body (fun y' esubs ->
            Let (DeclVal (y', go_v subs v), go_t (esubs @ subs) body))
    | Let (DeclProj (y, v, i), body) ->
        do_let y body (fun y' esubs ->
            Let (DeclProj (y', go_v subs v, i), go_t (esubs @ subs) body))
    | Let (DeclOp (y, op, e1, e2), body) ->
        do_let y body (fun y' esubs ->
            Let
              ( DeclOp (y', op, go_v subs e1, go_v subs e2),
                go_t (esubs @ subs) body ))
    | App (v, ts, vs) -> App (go_v subs v, ts, List.map (go_v subs) vs)
    | If0 (v, t1, t2) -> If0 (go_v subs v, go_t subs t1, go_t subs t2)
    | Halt (t, v) -> Halt (t, go_v subs v)
  in
  go_t [ (x, e) ]

let evalerr what = raise (EvalErr ("K eval error: " ^ what))

let step = function
  | Let (DeclVal (x, v), body) -> subst x v body
  | Let (DeclProj (x, VTup vs, i), body) -> subst x (List.nth vs (i - 1)) body
  | Let (DeclOp (x, op, VInt i, VInt j), body) ->
      subst x
        (VInt (match op with Plus -> i + j | Minus -> i - j | Times -> i * j))
        body
  | App ((VFix { name; params; body; _ } as f), _, vs) ->
      let paramsubs = List.map fst params |> List.map2 (fun v p -> (p, v)) vs in
      let subs = (name, f) :: paramsubs in
      List.fold_left (fun body (x, v) -> subst x v body) body subs
  | If0 (VInt 0, t2, _) -> t2
  | If0 (VInt _, _, t3) -> t3
  | t -> evalerr (sprintf "term %s is stuck" (se t))

let rec unwrap_value = function
  | VVar x -> evalerr ("unresolved variable " ^ x)
  | VAnnot (v, _) -> unwrap_value v
  | v -> v

let rec eval = function Halt (_, v) -> unwrap_value v | e -> eval (step e)
