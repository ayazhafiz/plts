open Util

type ty =
  | TName of string
  | TInt
  | TFn of { typarams : string list; params : ty list }
      (** ∀<as>.(ts)->void *)
  | TTup of ty list
  | TExist of string * ty

type annot_val = Annot of value * ty

and value =
  | VVar of string
  | VInt of int
  | VFix of {
      name : string;
      typarams : string list;
      params : (string * ty) list;
      body : term;
    }
  | VTup of annot_val list
  | VTyApp of annot_val * ty
  | VPack of ty * annot_val * ty
      (** pack [t1, v] as ∃a.t2   (t1 is the true a) *)

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
    | VFix { name; typarams; params; body } ->
        let header =
          if String.index_opt name '_' = Some 0 then "λ"
          else sprintf "fix %s" name
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

let string_of_annot e = with_buffer (fun f -> pp_annot f e) 80

let string_of_value e = with_buffer (fun f -> pp_value f e) 80

let string_of_term e = with_buffer (fun f -> pp_term f e) 80

let string_of_ty t = with_buffer (fun f -> pp_ty f t) 80

(*** Typecheck ***)

let tyerr what = raise (TyErr ("C type error: " ^ what))

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
  | VFix { name; typarams; params; body } ->
      (* well-formedness of types of parameters should be closed under this fn *)
      List.iter (fun (_, t) -> ty_wf t typarams) params;
      let fn_ty = TFn { typarams; params = List.map snd params } in
      let vctx' = (name, fn_ty) :: params @ vctx in
      term_wf typarams vctx' body;
      fn_ty
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

(*** Translation from K ***)

let rec trans_ty fresh = function
  | K.TName a -> TName a
  | K.TInt -> TInt
  | K.TFn { typarams; body } ->
      (* Becomes a closure type, which is a package consisting of the closure
         itself and its environment [b].
         C[∀[α⃗].(τ1,...,τn) → void] => ∃β.⟨∀[α⃗].(β,C[τ1],...,C[τn]) → void,β⟩ *)
      let trans_params = List.map (trans_ty fresh) body in
      let b = fresh "b" in
      TExist
        (b, TTup [ TFn { typarams; params = TName b :: trans_params }; TName b ])
  | K.TTup ts -> TTup (List.map (trans_ty fresh) ts)

let rec reduce_ty_app = function
  | fn, [] -> fn
  | (Annot (_, TFn { typarams = a :: bs; params }) as fn), o :: os ->
      let fn' = VTyApp (fn, o) in
      let tfn' = TFn { typarams = bs; params = List.map (tysub a o) params } in
      reduce_ty_app (Annot (fn', tfn'), os)
  | f, _ ->
      tyerr
        (sprintf
           "%s cannot accept a type application because it is not universally \
            quanitified"
           (sa f))

let kfv v =
  let open K in
  let rm lst x = List.filter (fun (y, _) -> x <> y) lst in
  let uniq lst =
    let rec go = function
      | [], all -> all
      | (x, t) :: rst, all ->
          if List.exists (fun (x', t') -> x = x' && t <> t') all then
            failwith (sprintf "same variable %s has different types" x)
          else if List.exists (fun (x', _) -> x = x') all then go (rst, all)
          else go (rst, all @ [ (x, t) ])
    in
    go (lst, [])
  in
  let rec gov = function
    | VAnnot (VVar x, t) -> [ (x, t) ]
    | VAnnot (VInt _, _) -> []
    | VAnnot (VFix { name; typarams = _; params; body }, _) ->
        List.fold_left rm (got body) (name :: List.map fst params)
    | VAnnot (VTup vs, _) -> List.concat_map gov vs
    | VAnnot (VAnnot _, _) -> failwith "nested annotation"
    | v -> failwith (sprintf "unannotated value %s" (K.string_of_value v))
  and got = function
    | Let (DeclVal (x, v), e) | Let (DeclProj (x, v, _), e) ->
        gov v @ rm (got e) x
    | Let (DeclOp (x, _, v1, v2), e) -> gov v1 @ gov v2 @ rm (got e) x
    | App (v, _, vs) -> List.concat_map gov (v :: vs)
    | If0 (v1, t2, t3) -> gov v1 @ got t2 @ got t3
    | Halt (_, v) -> gov v
  in
  uniq (gov v)

let kftv v =
  let open K in
  let open SSet in
  let rec gov = function
    | VVar _ | VInt _ -> empty
    | VTup vs -> List.fold_left union empty (List.map gov vs)
    | VFix { name = _; typarams; params; body } ->
        let params_ftvs = List.map (fun (_, t) -> ftv t) params in
        diff
          (union (List.fold_left union empty params_ftvs) (got body))
          (of_list typarams)
    | VAnnot (v, t) ->
        let ftvv = gov v in
        let ftvt = ftv t in
        union ftvv ftvt
  and got = function
    | Let (DeclVal (_, v), e) | Let (DeclProj (_, v, _), e) ->
        union (gov v) (got e)
    | Let (DeclOp (_, _, v1, v2), e) -> union (gov v1) (gov v2) |> union (got e)
    | App (v, ts, vs) ->
        let base = List.fold_left union empty (List.map ftv ts) in
        List.fold_left union base (List.map gov (v :: vs))
    | If0 (v1, t2, t3) -> gov v1 |> union (got t2) |> union (got t3)
    | Halt (t, v) -> union (ftv t) (gov v)
  in
  gov v |> to_seq |> List.of_seq

let rec trans_aval fresh = function
  | K.VAnnot (K.VVar x, t) -> Annot (VVar x, trans_ty fresh t)
  | K.VAnnot (K.VInt i, t) -> Annot (VInt i, trans_ty fresh t)
  | K.VAnnot (K.VTup vs, t) ->
      Annot (VTup (List.map (trans_aval fresh) vs), trans_ty fresh t)
  | K.VAnnot (K.VFix { name = x; typarams = tas; params; body = e }, t) as akfix
    ->
      let yos = kfv akfix in
      let bs = kftv akfix in
      let bnames = List.map (fun b -> TName b) bs in
      let os = K.TTup (List.map snd yos) in
      let t_env = trans_ty fresh os in
      let fix_params = List.map (fun (_, kt) -> trans_ty fresh kt) params in
      let t_rawcode =
        TFn { typarams = bs @ tas; params = t_env :: fix_params }
      in
      let t_code = TFn { typarams = tas; params = t_env :: fix_params } in
      let z_code = fresh "z_code" in
      let z_env = fresh "z_env" in
      let v_code =
        let cfix =
          VFix
            {
              name = z_code;
              typarams = bs @ tas;
              params =
                (z_env, t_env)
                :: List.map (fun (x, t) -> (x, trans_ty fresh t)) params;
              body =
                (let x_clos =
                   Annot
                     ( VTup
                         [
                           reduce_ty_app (Annot (VVar z_code, t_rawcode), bnames);
                           Annot (VVar z_env, t_env);
                         ],
                       TTup [ t_code; t_env ] )
                 in
                 let pkg_t = trans_ty fresh t in
                 let x_pkg = VPack (t_env, x_clos, pkg_t) in
                 let yis = List.mapi (fun i (y, _) -> (y, i + 1)) yos in
                 let z_t_env = Annot (VVar z_env, t_env) in
                 let body =
                   List.fold_right
                     (fun (y, i) e' -> Let (DeclProj (y, z_t_env, i), e'))
                     yis (trans_exp fresh e)
                 in
                 Let (DeclVal (x, Annot (x_pkg, pkg_t)), body));
            }
        in
        Annot (cfix, t_rawcode)
      in
      let v_env =
        Annot
          ( VTup (List.map (fun (y, o) -> Annot (VVar y, trans_ty fresh o)) yos),
            t_env )
      in
      let clos_t = TTup [ t_code; t_env ] in
      let clos = VTup [ reduce_ty_app (v_code, bnames); v_env ] in
      let pkg_t = trans_ty fresh t in
      let pkg = VPack (t_env, Annot (clos, clos_t), pkg_t) in
      Annot (pkg, pkg_t)
  | K.VAnnot (K.VAnnot _, _) ->
      tyerr "attempting to translate nested K annotation"
  | _ -> tyerr "attempting to translate unannoated K value"

and trans_dec fresh = function
  | K.DeclVal (x, v) -> DeclVal (x, trans_aval fresh v)
  | K.DeclProj (x, v, i) -> DeclProj (x, trans_aval fresh v, i)
  | K.DeclOp (x, op, v1, v2) ->
      DeclOp (x, op, trans_aval fresh v1, trans_aval fresh v2)

and trans_exp fresh = function
  | K.Let (d, e) -> Let (trans_dec fresh d, trans_exp fresh e)
  | K.If0 (v, e1, e2) ->
      If0 (trans_aval fresh v, trans_exp fresh e1, trans_exp fresh e2)
  | K.Halt (t, v) -> Halt (trans_ty fresh t, trans_aval fresh v)
  | K.App ((K.VAnnot (_, t) as ut), os, vs) -> (
      match trans_ty fresh t with
      | TExist (y, (TTup [ t_code; TName y' ] as pkg)) when y = y' ->
          let z = fresh "z" in
          let z_pkg = Annot (VVar z, pkg) in
          let z_code = fresh "z_code" in
          let z_env = fresh "z_env" in
          let fn =
            reduce_ty_app
              (Annot (VVar z_code, t_code), List.map (trans_ty fresh) os)
          in
          let env_arg = Annot (VVar z_env, TName y) in
          let args = List.map (trans_aval fresh) vs in
          Let
            ( DeclUnpack (y, z, trans_aval fresh ut),
              Let
                ( DeclProj (z_code, z_pkg, 1),
                  Let (DeclProj (z_env, z_pkg, 2), App (fn, env_arg :: args)) )
            )
      | _ -> tyerr "attempting to translate malformed application")
  | K.App _ -> tyerr "attempting to translate unannotated application target"

let all_names =
  let open K in
  let open SSet in
  let rec gov = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VTup ts -> List.fold_left union empty (List.map gov ts)
    | VFix { name; typarams; params; body } ->
        add name (of_list typarams)
        |> union (of_list (List.map fst params))
        |> union (got body)
    | VAnnot (v, t) -> union (gov v) (goty t)
  and god = function
    | DeclVal (x, v) | DeclProj (x, v, _) -> add x (gov v)
    | DeclOp (x, _, v1, v2) -> union (gov v1) (gov v2) |> add x
  and got = function
    | Let (d, t) -> union (god d) (got t)
    | App (v, ts, vs) ->
        let r = List.fold_left union (gov v) (List.map goty ts) in
        List.fold_left union r (List.map gov vs)
    | If0 (v, t1, t2) -> union (got t1) (got t2) |> union (gov v)
    | Halt (t, v) -> union (goty t) (gov v)
  and goty = function
    | TInt -> empty
    | TName x -> singleton x
    | TFn { typarams; body } ->
        union (of_list typarams)
          (List.fold_left union empty (List.map goty body))
    | TTup ts -> List.fold_left union empty (List.map goty ts)
  in
  got

let trans_top e =
  let fresh = fresh_generator (all_names e) in
  trans_exp fresh e

(*** Eval ***)

let rec fvs_a (Annot (v, _)) = fvs_v v

and fvs_v =
  let open SSet in
  let go = function
    | VVar x -> singleton x
    | VInt _ -> empty
    | VFix { name; typarams = _; params; body } ->
        let bound = add name (of_list (List.map fst params)) in
        diff (fvs_t body) bound
    | VTup vs -> List.fold_left union empty (List.map fvs_a vs)
    | VPack (_, v, _) -> fvs_a v
    | VTyApp (v, _) -> fvs_a v
  in
  go

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

let evalerr what = raise (EvalErr ("K eval error: " ^ what))

let step = function
  | Let (DeclVal (x, Annot (v, _)), body) -> subst x v body
  | Let (DeclProj (x, Annot (VTup vs, _), i), body) ->
      let (Annot (vi, _)) = List.nth vs (i - 1) in
      subst x vi body
  | Let (DeclOp (x, op, Annot (VInt i, _), Annot (VInt j, _)), body) ->
      subst x (VInt (do_op op i j)) body
  | Let (DeclUnpack (_, x, Annot (VPack (_, Annot (v, _), _), _)), body) ->
      subst x v body
  | App (Annot ((VFix { name; params; body; _ } as f), _), vs) ->
      let paramsubs =
        List.map fst params |> List.map2 (fun (Annot (v, _)) p -> (p, v)) vs
      in
      let subs = (name, f) :: paramsubs in
      List.fold_left (fun body (x, v) -> subst x v body) body subs
  | If0 (Annot (VInt 0, _), t2, _) -> t2
  | If0 (Annot (VInt _, _), _, t3) -> t3
  | t -> evalerr (sprintf "term %s is stuck" (se t))

let unwrap_value = function
  | VVar x -> evalerr ("unresolved variable " ^ x)
  | v -> v

let rec eval = function
  | Halt (_, Annot (v, _)) -> unwrap_value v
  | e -> eval (step e)
