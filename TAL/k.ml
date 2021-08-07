open Util

type ty =
  | TName of string
  | TInt
  | TFn of { typarams : string list; body : ty list }  (** ∀<as>.(ts)->void *)
  | TTup of ty list

type op = Plus | Minus | Times

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

let rec freshen a used = if SSet.mem a used then freshen (a ^ "'") used else a

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
                     "in application to %s,expected argument %s to be of type \
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
  | F.TName n -> TName n
  | F.TInt -> TInt
  | F.TArrow (t1, t2) ->
      (* Idea: translate a function f: A -> B into a continuation-based function
         given by f: (A, B -> void) -> void. Where as in F we would do something
         like [f(x: A): B = h(x) in f 1], this will now become
         [f(x, cont) = (let x' = h(x) in cont(x)) in f(1, \x. halt<B>x)]. *)
      TFn { typarams = []; body = [ trans_ty t1; trans_cont t2 ] }
  | F.TAll (a, t) -> TFn { typarams = [ a ]; body = [ trans_cont t ] }
  | F.TTup ts -> TTup (List.map trans_ty ts)

(** Makes a type a parameter to a continuation. *)
and trans_cont t = TFn { typarams = []; body = [ trans_ty t ] }

let rec trans_exp fresh tctx vctx u k =
  match u with
  | F.Var y -> App (k, [], [ VVar y ])
  | F.Int i -> App (k, [], [ VInt i ])
  | F.Tup us ->
      (* Kexp[⟨u1 ,...,un ⟩ ]k = Kexp[u1](λx1:K[τ1].
                                   ...
                                     Kexp[un](λxn:K[τn].
                                       k((x1, ..., xn)))) *)
      let kts = List.map (F.tyof tctx vctx) us |> List.map trans_ty in
      let xs = List.map (fun _ -> fresh "x") us in
      let tup = VTup (List.map (fun x -> VVar x) xs) in
      let final = App (k, [], [ tup ]) in
      List.fold_right2
        (fun u (x, t) body ->
          trans_exp fresh tctx vctx u
            (VFix { name = fresh "_"; typarams = []; params = [ (x, t) ]; body }))
        us (List.combine xs kts) final
  | F.Fix { name = x; param = x1; param_ty = t1; ret_ty = t2; body = e } ->
      (* Kexp[(fixx(x1:τ1):τ2.e) ]k = k((fix x(x1:K[τ1], c:Kcont[τ2]).Kexp[e]c ) ) *)
      let c = fresh "c" in
      let vctx' = (x, F.TArrow (t1, t2)) :: (x1, t1) :: vctx in
      let k_body = trans_exp fresh tctx vctx' e (VVar "c") in
      App
        ( k,
          [],
          [
            VFix
              {
                name = x;
                typarams = [];
                params = [ (x1, trans_ty t1); (c, trans_cont t2) ];
                body = k_body;
              };
          ] )
  | F.App (u1, u2) ->
      (* Kexp[(u1^t1 u2^t2)]k = Kexp[u1](λx1:K[τ1].
                                  Kexp[u2](λx2:K[τ2].
                                    x1(x2,k))) *)
      let kt1 = F.tyof tctx vctx u1 |> trans_ty in
      let kt2 = F.tyof tctx vctx u2 |> trans_ty in
      let x1 = fresh "x1" in
      let x2 = fresh "x2" in
      trans_exp fresh tctx vctx u1
        (VFix
           {
             name = fresh "_";
             typarams = [];
             params = [ (x1, kt1) ];
             body =
               trans_exp fresh tctx vctx u2
                 (VFix
                    {
                      name = fresh "_";
                      typarams = [];
                      params = [ (x2, kt2) ];
                      body = App (VVar x1, [], [ VVar x2; k ]);
                    });
           })
  | F.TyAbs (a, u) ->
      (* Kexp[(Λα.u^τ)]k = k(λ[α](c:Kcont[τ]).Kexp[u]c) *)
      let t = F.tyof (a :: tctx) vctx u in
      let c = fresh "c" in
      App
        ( k,
          [],
          [
            VFix
              {
                name = fresh "_";
                typarams = [ a ];
                params = [ (c, trans_cont t) ];
                body = trans_exp fresh tctx vctx u (VVar c);
              };
          ] )
  | F.TyApp (u, o) ->
      (* Kexp[(u^τ[σ])]k = Kexp[u](λx:K[τ]. x[K[σ]](k)) *)
      let t = F.tyof tctx vctx u in
      let x = fresh "x" in
      trans_exp fresh tctx vctx u
        (VFix
           {
             name = fresh "_";
             typarams = [];
             params = [ (x, trans_ty t) ];
             body = App (VVar x, [ trans_ty o ], [ k ]);
           })
  | F.Proj (u, i) ->
      (* Kexp[(u^t).i]k = Kexp[u](\x:K[t]. let y = x.i in k(y)) *)
      let t = F.tyof tctx vctx u in
      let x = fresh "x" in
      let y = fresh "y" in
      trans_exp fresh tctx vctx u
        (VFix
           {
             name = fresh "_";
             typarams = [];
             params = [ (x, trans_ty t) ];
             body = Let (DeclProj (y, VVar x, i), App (k, [], [ VVar y ]));
           })
  | F.Op (op, e1, e2) ->
      let x1 = fresh "x1" in
      let x2 = fresh "x2" in
      let y = fresh "y" in
      let op =
        match op with F.Plus -> Plus | F.Minus -> Minus | F.Times -> Times
      in
      trans_exp fresh tctx vctx e1
        (VFix
           {
             name = fresh "_";
             typarams = [];
             params = [ (x1, TInt) ];
             body =
               trans_exp fresh tctx vctx e2
                 (VFix
                    {
                      name = fresh "_";
                      typarams = [];
                      params = [ (x2, TInt) ];
                      body =
                        Let
                          ( DeclOp (y, op, VVar x1, VVar x2),
                            App (k, [], [ VVar y ]) );
                    });
           })
  | F.If0 (e1, e2, e3) ->
      let x = fresh "x" in
      trans_exp fresh tctx vctx e1
        (VFix
           {
             name = fresh "_";
             typarams = [];
             params = [ (x, TInt) ];
             body =
               If0
                 ( VVar x,
                   trans_exp fresh tctx vctx e2 k,
                   trans_exp fresh tctx vctx e3 k );
           })
  | Annot (e, _) -> trans_exp fresh tctx vctx e k

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
  (* Kprog[u^t] = Kexp[u](λx:K[τ].halt[K[τ]]x) *)
  let t = F.tyof [] [] u in
  let k_t = trans_ty t in
  let toplevel_cont =
    VFix
      {
        name = fresh "tl_halt";
        typarams = [];
        params = [ (fresh "x", k_t) ];
        body = Halt (k_t, VVar "x");
      }
  in
  trans_exp fresh [] [] u toplevel_cont
