open Util

module type Int = sig
  type t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( = ) : t -> t -> bool

  val of_ocaml_int : int -> t

  val string_of : t -> string
end

module TAL (Int : Int) = struct
  (*** Syntax: Figure 13 (page 20) of the paper ***)
  type iflag = Uninit | Init

  type label = Lab of string

  type register = R of int

  and register_file_ty = (register * ty) list

  and ty =
    | TName of string
    | TInt
    | TCall of string list * register_file_ty  (** ∀[α⃗].Γ *)
    | TTup of (ty * iflag) list
    | TExist of string * ty

  type heap_type = (label * ty) list

  type word_value =
    | WVLabel of label
    | WVInt of Int.t
    | WVJunk of ty
    | WVTyApp of word_value * ty
    | WVPack of ty * word_value * ty

  type small_value =
    | SVReg of register
    | SVWord of word_value
    | SVTyApp of small_value * ty
    | SVPack of ty * small_value * ty

  type instr =
    | Add of register * register * small_value
    | Bnz of register * small_value
    | Ld of register * register * int  (** load at (heaped) tuple index *)
    | Malloc of register * ty list
    | Mov of register * small_value
    | Mul of register * register * small_value
    | St of register * int * register  (** store at (heaped) tuple index *)
    | Sub of register * register * small_value
    | Unpack of string * register * small_value

  type instr_seq = Seq of instr * instr_seq | Jmp of small_value | Halt of ty

  let seq = List.fold_right (fun i r -> Seq (i, r))

  type heap_value =
    | HVTup of word_value list
    | HVCode of string list * register_file_ty * instr_seq

  type heap = (label * heap_value) list

  type register_file = (register * word_value) list

  type program = P of (heap * register_file * instr_seq)

  let rec ftv_ty =
    let open SSet in
    function
    | TName n -> singleton n
    | TInt -> empty
    | TCall (tps, ps) ->
        let ftv_ps =
          List.fold_left union empty (List.map (fun (_, t) -> ftv_ty t) ps)
        in
        diff ftv_ps (of_list tps)
    | TTup ts ->
        List.fold_left union empty (List.map (fun (t, _) -> ftv_ty t) ts)
    | TExist (x, t) -> remove x (ftv_ty t)

  let rec ftv_wv =
    let open SSet in
    function
    | WVLabel _ -> empty
    | WVInt _ -> empty
    | WVJunk t -> ftv_ty t
    | WVTyApp (wv, t) -> union (ftv_wv wv) (ftv_ty t)
    | WVPack (t1, wv, t2) -> union (ftv_ty t1) (ftv_ty t2) |> union (ftv_wv wv)

  let rec ftv_sv =
    let open SSet in
    function
    | SVReg _ -> empty
    | SVWord wv -> ftv_wv wv
    | SVTyApp (sv, t) -> union (ftv_sv sv) (ftv_ty t)
    | SVPack (t1, v, t2) -> union (ftv_ty t1) (ftv_ty t2) |> union (ftv_sv v)

  (*** Pretty printing ***)

  let pp_r f =
    let open Format in
    function R i -> fprintf f "r%d" i

  let pp_l f =
    let open Format in
    function Lab l -> fprintf f "%s" l

  let pp_ty f =
    let open Format in
    let rec go = function
      | TInt -> fprintf f "int"
      | TName a -> pp_print_string f a
      | TCall (typarams, params) ->
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
            (fun i (r, t) ->
              pp_r f r;
              fprintf f ": ";
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

  let rec pp_wv f =
    let open Format in
    function
    | WVLabel l -> pp_l f l
    | WVInt i -> pp_print_string f Int.(string_of i)
    | WVJunk t ->
        fprintf f "?";
        pp_ty f t
    | WVTyApp (v, t) ->
        fprintf f "@[<hov 2>";
        pp_wv f v;
        fprintf f "<@,";
        pp_ty f t;
        fprintf f ">@]"
    | WVPack (t1, v, t2) ->
        fprintf f "@[<hov 2>(pack [";
        pp_ty f t1;
        fprintf f ",@ ";
        pp_wv f v;
        fprintf f "]@ as ";
        pp_ty f t2;
        fprintf f ")@]"

  let rec pp_sv f =
    let open Format in
    function
    | SVReg r -> pp_r f r
    | SVWord w -> pp_wv f w
    | SVTyApp (v, t) ->
        fprintf f "@[<hov 2>";
        pp_sv f v;
        fprintf f "<@,";
        pp_ty f t;
        fprintf f ">@]"
    | SVPack (t1, v, t2) ->
        fprintf f "@[<hov 2>(pack [";
        pp_ty f t1;
        fprintf f ",@ ";
        pp_sv f v;
        fprintf f "]@ as ";
        pp_ty f t2;
        fprintf f ")@]"

  let pp_i f i =
    let open Format in
    fprintf f "@[<hov 2>";
    (match i with
    | Add (rd, rs, v) ->
        fprintf f "add ";
        pp_r f rd;
        fprintf f ",@ ";
        pp_r f rs;
        fprintf f ",@ ";
        pp_sv f v
    | Sub (rd, rs, v) ->
        fprintf f "sub ";
        pp_r f rd;
        fprintf f ",@ ";
        pp_r f rs;
        fprintf f ",@ ";
        pp_sv f v
    | Mul (rd, rs, v) ->
        fprintf f "mul ";
        pp_r f rd;
        fprintf f ",@ ";
        pp_r f rs;
        fprintf f ",@ ";
        pp_sv f v
    | Bnz (r, v) ->
        fprintf f "bnz ";
        pp_r f r;
        fprintf f ",@ ";
        pp_sv f v
    | Ld (rd, rs, i) ->
        fprintf f "ld ";
        pp_r f rd;
        fprintf f ",@ ";
        pp_r f rs;
        fprintf f ".%d" i
    | St (rd, i, rs) ->
        fprintf f "ld ";
        pp_r f rd;
        fprintf f ".%d,@ " i;
        pp_r f rs
    | Mov (rd, v) ->
        fprintf f "mov ";
        pp_r f rd;
        fprintf f ",@ ";
        pp_sv f v
    | Malloc (rd, ts) ->
        fprintf f "malloc ";
        pp_r f rd;
        fprintf f "@,[@[<hov 2>";
        let lasti = List.length ts - 1 in
        List.iteri
          (fun i t ->
            pp_ty f t;
            if i <> lasti then fprintf f ",@ ")
          ts;
        fprintf f "@]]"
    | Unpack (a, rd, v) ->
        fprintf f "unpack[@[<hov 2>%s,@ " a;
        pp_r f rd;
        fprintf f "@]],@ ";
        pp_sv f v);
    fprintf f "@]"

  let rec pp_is f =
    let open Format in
    function
    | Seq (i, i') ->
        pp_i f i;
        fprintf f "@,";
        pp_is f i'
    | Jmp v ->
        fprintf f "@[<hov 2>jmp ";
        pp_sv f v;
        fprintf f "@]"
    | Halt t ->
        fprintf f "@[<hov 2>halt[@[<hov 2>";
        pp_ty f t;
        fprintf f "@]]@]"

  let pp_rfile f rf =
    let open Format in
    let lasti = List.length rf - 1 in
    fprintf f "@[<hov 2>(";
    List.iteri
      (fun i (r, t) ->
        fprintf f "@[<hov 2>";
        pp_r f r;
        fprintf f ":@ ";
        pp_ty f t;
        fprintf f "@]";
        if i <> lasti then fprintf f ",@ ")
      rf;
    fprintf f ")@]"

  let pp_hv f =
    let open Format in
    function
    | HVTup ws ->
        fprintf f "@[<hov 2>(";
        let lasti = List.length ws - 1 in
        List.iteri
          (fun i e ->
            pp_wv f e;
            if i <> lasti then fprintf f ",@ ")
          ws;
        fprintf f ")@]"
    | HVCode (typarams, rfile, i) ->
        fprintf f "@[<v 2>@[<hov 2>code";
        if List.length typarams <> 0 then (
          fprintf f "<";
          let lasti = List.length typarams - 1 in
          List.iteri
            (fun i t ->
              pp_print_string f t;
              if i <> lasti then fprintf f ",@ ")
            typarams;
          fprintf f ">@,");
        pp_rfile f rfile;
        fprintf f ".@]@,@[<v>";
        pp_is f i;
        fprintf f "@]@]"

  let pp_prog f =
    let open Format in
    function
    | P (h, _, i) ->
        fprintf f "@[<v>";
        List.iter
          (fun (Lab l, hv) ->
            fprintf f "@[<v 2>%s:@,@[" l;
            pp_hv f hv;
            fprintf f "@]@]@,")
          h;
        fprintf f "@[<v 4>__entry:@,@[<v>";
        pp_is f i;
        fprintf f "@]@]@]"

  let swv e = with_buffer (fun f -> pp_wv f e) 80

  let ssv e = with_buffer (fun f -> pp_sv f e) 80

  let shv e = with_buffer (fun f -> pp_hv f e) 80

  let si e = with_buffer (fun f -> pp_i f e) 80

  let sl e = with_buffer (fun f -> pp_l f e) 80

  let sty e = with_buffer (fun f -> pp_ty f e) 80

  let sr e = with_buffer (fun f -> pp_r f e) 80

  let srfile e = with_buffer (fun f -> pp_rfile f e) 80

  let sprog p = with_buffer (fun f -> pp_prog f p) 80

  let sprintf = Printf.sprintf

  (*** Evalution (7.2) ***)

  let ftv_subs subs =
    List.fold_left SSet.union SSet.empty
      (List.map (fun (_, t) -> ftv_ty t) subs)

  let rec tysubst_ty subs =
    let ftv_subs = ftv_subs subs in
    function
    | TName n -> (
        match List.assoc_opt n subs with Some t -> t | None -> TName n)
    | TInt -> TInt
    | TCall (tas, tys) ->
        let ftv_tys =
          List.fold_left SSet.union SSet.empty
            (List.map (fun (_, t) -> ftv_ty t) tys)
        in
        let freshen =
          let used = ref (SSet.union ftv_subs ftv_tys) in
          fun t ->
            let t' = freshen t !used in
            used := SSet.add t' !used;
            t'
        in
        let tas' = List.map freshen tas in
        let subs' = List.map2 (fun ta ta' -> (ta, TName ta')) tas tas' @ subs in
        TCall (tas', List.map (fun (r, t) -> (r, tysubst_ty subs' t)) tys)
    | TTup ts -> TTup (List.map (fun (t, i) -> (tysubst_ty subs t, i)) ts)
    | TExist (x, ty) ->
        let used = SSet.union ftv_subs (ftv_ty ty) in
        let x' = freshen x used in
        let subs' = (x, TName x') :: subs in
        TExist (x', tysubst_ty subs' ty)

  let rec tysubst_wv subs = function
    | WVLabel l -> WVLabel l
    | WVInt i -> WVInt i
    | WVJunk t -> WVJunk (tysubst_ty subs t)
    | WVTyApp (v, t) -> WVTyApp (tysubst_wv subs v, tysubst_ty subs t)
    | WVPack (t1, v, t2) ->
        WVPack (tysubst_ty subs t1, tysubst_wv subs v, tysubst_ty subs t2)

  let rec tysubst_sv subs = function
    | SVReg r -> SVReg r
    | SVWord w -> SVWord (tysubst_wv subs w)
    | SVTyApp (v, t) -> SVTyApp (tysubst_sv subs v, tysubst_ty subs t)
    | SVPack (t1, v, t2) ->
        SVPack (tysubst_ty subs t1, tysubst_sv subs v, tysubst_ty subs t2)

  let rec tysubst_instrs subs = function
    | Seq (i, iseq) ->
        let i', subs' =
          match i with
          | Add (rd, rs, v) -> (Add (rd, rs, tysubst_sv subs v), subs)
          | Bnz (r, v) -> (Bnz (r, tysubst_sv subs v), subs)
          | Ld (rd, rs, i) -> (Ld (rd, rs, i), subs)
          | Malloc (r, ts) -> (Malloc (r, List.map (tysubst_ty subs) ts), subs)
          | Mov (r, v) -> (Mov (r, tysubst_sv subs v), subs)
          | Mul (rd, rs, v) -> (Mul (rd, rs, tysubst_sv subs v), subs)
          | St (rd, i, rs) -> (St (rd, i, rs), subs)
          | Sub (rd, rs, v) -> (Sub (rd, rs, tysubst_sv subs v), subs)
          | Unpack (a, r, v) ->
              let v' = tysubst_sv subs v in
              let used = SSet.union (ftv_subs subs) (ftv_sv v) in
              let a' = freshen a used in
              let subs' = (a, TName a') :: subs in
              (Unpack (a', r, v'), subs')
        in
        Seq (i', tysubst_instrs subs' iseq)
    | Jmp v -> Jmp (tysubst_sv subs v)
    | Halt t -> Halt (tysubst_ty subs t)

  let tysubst_rfile subs = List.map (fun (r, t) -> (r, tysubst_ty subs t))

  let evalerr what = raise (EvalErr ("TAL eval error: " ^ what))

  let rec eval_r rf = function
    | SVReg r -> List.assoc r rf
    | SVWord w -> w
    | SVTyApp (v, ty) -> WVTyApp (eval_r rf v, ty)
    | SVPack (t, v, t') -> WVPack (t, eval_r rf v, t')

  let call_target word =
    let rec unwrap tyargs = function
      | WVTyApp (word, ty) -> unwrap (ty :: tyargs) word
      | WVLabel lab -> (lab, tyargs)
      | _ -> evalerr "not a callable target"
    in
    unwrap [] word

  let get_code heap label =
    match List.assoc_opt label heap with
    | Some (HVCode (tps, ps, b)) -> (tps, ps, b)
    | Some _ -> evalerr "not code"
    | None -> evalerr "label is unbound"

  let get_tup heap label =
    match List.assoc_opt label heap with
    | Some (HVTup fields) -> fields
    | Some _ -> evalerr "not tuple"
    | None -> evalerr "label is unbound"

  let int_of_word = function WVInt i -> i | _ -> evalerr "not an int"

  let label_of_word = function WVLabel l -> l | _ -> evalerr "not a label"

  let pack_of_word = function
    | WVPack (t, w, t') -> (t, w, t')
    | _ -> evalerr "not a package"

  let step (P (h, rf, i)) =
    match i with
    | Jmp v ->
        let lab, tyargs = call_target (eval_r rf v) in
        let typarams, _, instrs = get_code h lab in
        let instrs' = tysubst_instrs (List.combine typarams tyargs) instrs in
        P (h, rf, instrs')
    | Seq (Add (rd, rs, v), i') ->
        let n = int_of_word (List.assoc rs rf) in
        let m = int_of_word (eval_r rf v) in
        let rf' = (rd, WVInt Int.(n + m)) :: rf in
        P (h, rf', i')
    | Seq (Sub (rd, rs, v), i') ->
        let n = int_of_word (List.assoc rs rf) in
        let m = int_of_word (eval_r rf v) in
        let rf' = (rd, WVInt Int.(n - m)) :: rf in
        P (h, rf', i')
    | Seq (Mul (rd, rs, v), i') ->
        let n = int_of_word (List.assoc rs rf) in
        let m = int_of_word (eval_r rf v) in
        let rf' = (rd, WVInt Int.(n * m)) :: rf in
        P (h, rf', i')
    | Seq (Bnz (r, v), i') ->
        let n = int_of_word (List.assoc r rf) in
        if Int.(n = of_ocaml_int 0) then P (h, rf, i')
        else
          let lab, tyargs = call_target (eval_r rf v) in
          let typarams, _, i'' = get_code h lab in
          let i''sub = tysubst_instrs (List.combine typarams tyargs) i'' in
          P (h, rf, i''sub)
    | Seq (Ld (rd, rs, idx), i') ->
        let l = label_of_word (List.assoc rs rf) in
        let fields = get_tup h l in
        let wi = List.nth fields idx in
        let rf' = (rd, wi) :: rf in
        P (h, rf', i')
    | Seq (Malloc (rd, tys), i') ->
        let l =
          Lab (freshen "l" (SSet.of_list (List.map (fun (Lab l, _) -> l) h)))
        in
        let tup = HVTup (List.map (fun t -> WVJunk t) tys) in
        let h' = (l, tup) :: h in
        let rf' = (rd, WVLabel l) :: rf in
        P (h', rf', i')
    | Seq (Mov (rd, v), i') ->
        let rf' = (rd, eval_r rf v) :: rf in
        P (h, rf', i')
    | Seq (St (rd, idx, rs), i') ->
        let l = label_of_word (List.assoc rd rf) in
        let fields = get_tup h l in
        let init = List.assoc rs rf in
        let fields' =
          List.mapi (fun n vi -> if n = idx then init else vi) fields
        in
        let h' = (l, HVTup fields') :: h in
        P (h', rf, i')
    | Seq (Unpack (a, rd, v), i') ->
        let t, w, _ = pack_of_word (eval_r rf v) in
        let i'sub = tysubst_instrs [ (a, t) ] i' in
        let rf' = (rd, w) :: rf in
        P (h, rf', i'sub)
    | Halt _ -> evalerr "unreachable"

  let rec eval = function
    | P (_, r, Halt _) -> List.assoc (R 1) r
    | p -> eval (step p)

  (*** Typecheck (7.3) ***)

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
      | TCall (tp1, p1), TCall (tp2, p2) ->
          let p1, p2 = (List.map snd p1, List.map snd p2) in
          if
            List.length tp1 <> List.length tp2
            || List.length p1 <> List.length p2
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

  let subtype s t =
    if tyaeq s t then true
    else
      match (s, t) with
      | TTup s, TTup t ->
          List.length s = List.length t
          && List.for_all2
               (fun (si, sflag) (ti, tflag) ->
                 tyaeq si ti && (sflag = tflag || sflag = Init))
               s t
      | _ -> false

  let rfile_simpl rf =
    let rec go seen = function
      | [] -> []
      | (x, t) :: rest ->
          if List.mem x seen then go seen rest
          else (x, t) :: go (x :: seen) rest
    in
    go [] rf

  let subrfile f g =
    let f, g = (rfile_simpl f, rfile_simpl g) in
    List.for_all
      (fun (r, t) ->
        match List.assoc_opt r f with Some t' -> subtype t' t | None -> false)
      g

  let tyerr what = raise (TyErr ("TAL type error: " ^ what))

  let check_type tctx t =
    if not (SSet.subset (ftv_ty t) (SSet.of_list tctx)) then
      tyerr (Printf.sprintf "%s not well-formed" (sty t))

  let check_htype hctx = List.iter (fun (_, ti) -> check_type [] ti) hctx

  let check_rftype tctx rf = List.iter (fun (_, ti) -> check_type tctx ti) rf

  let rec check_wval hctx tctx = function
    | WVLabel l -> (
        match List.assoc_opt l hctx with
        | Some t -> t
        | None -> tyerr (sprintf "label %s does not exist" (sl l)))
    | WVInt _ -> TInt
    | WVTyApp (w, t) -> (
        check_type tctx t;
        match check_wval hctx tctx w with
        | TCall (a :: bs, rf) -> TCall (bs, tysubst_rfile [ (a, t) ] rf)
        | t ->
            tyerr
              (sprintf "%s cannot be type-applied because it has type %s"
                 (swv w) (sty t)))
    | WVPack (t, w, TExist (a, t')) as pack ->
        check_type tctx t;
        let wty = check_wval hctx tctx w in
        let wtyexp = tysubst_ty [ (a, t) ] t' in
        if tyaeq wty wtyexp then TExist (a, t')
        else
          tyerr
            (sprintf "package %s declared existentially as %s but checked as %s"
               (swv pack) (sty wtyexp) (sty wty))
    | WVPack _ as pack ->
        tyerr (sprintf "package %s must have existential type" (swv pack))
    | WVJunk ty -> ty

  let rec check_sv hctx tctx rctx = function
    | SVReg r -> (
        match List.assoc_opt r rctx with
        | Some t -> t
        | None -> tyerr (sprintf "%s does not exist" (sr r)))
    | SVWord w -> check_wval hctx tctx w
    | SVTyApp (v, t) -> (
        check_type tctx t;
        match check_sv hctx tctx rctx v with
        | TCall (a :: bs, rf) -> TCall (bs, tysubst_rfile [ (a, t) ] rf)
        | t ->
            tyerr
              (sprintf "%s cannot be type-applied because it has type %s"
                 (ssv v) (sty t)))
    | SVPack (t, w, TExist (a, t')) as pack ->
        check_type tctx t;
        let wty = check_sv hctx tctx rctx w in
        let wtyexp = tysubst_ty [ (a, t) ] t' in
        if tyaeq wty wtyexp then TExist (a, t')
        else
          tyerr
            (sprintf "package %s declared existentially as %s but checked as %s"
               (ssv pack) (sty wtyexp) (sty wty))
    | SVPack _ as pack ->
        tyerr (sprintf "package %s must have existential type" (ssv pack))

  let expect_reg_ty rctx r t =
    match List.assoc_opt r rctx with
    | Some t' ->
        if not (subtype t' t) then
          tyerr
            (sprintf "register %s has type %s, expected %s" (sr r) (sty t')
               (sty t))
    | None -> tyerr (sprintf "register %s does not exist" (sr r))

  let expect_sv_ty hctx tctx rctx sv ty =
    let t = check_sv hctx tctx rctx sv in
    if not (subtype t ty) then
      tyerr (sprintf "%s has type %s, expected %s" (ssv sv) (sty t) (sty ty))

  let expect_subrfile f g =
    if not (subrfile f g) then
      tyerr (sprintf "%s is not subrfile of %s" (srfile f) (srfile g))

  let rec check_instr hctx tctx rctx = function
    | Seq ((Add (rd, rs, v) | Sub (rd, rs, v) | Mul (rd, rs, v)), i) ->
        expect_reg_ty rctx rs TInt;
        expect_sv_ty hctx tctx rctx v TInt;
        check_instr hctx tctx ((rd, TInt) :: rctx) i
    | Seq (Bnz (r, v), i) -> (
        expect_reg_ty rctx r TInt;
        match check_sv hctx tctx rctx v with
        | TCall ([], rctx') ->
            expect_subrfile rctx rctx';
            check_instr hctx tctx rctx i
        | t -> tyerr (sprintf "%s, of type %s, is not callable" (ssv v) (sty t))
        )
    | Seq ((Ld (rd, rs, idx) as ld), i) -> (
        match List.assoc_opt rs rctx with
        | Some (TTup ts) -> (
            match List.nth_opt ts idx with
            | Some (t, Init) -> check_instr hctx tctx ((rd, t) :: rctx) i
            | Some (t, Uninit) ->
                tyerr (sprintf "%s refers to uninitialized %s" (si ld) (sty t))
            | None ->
                tyerr
                  (sprintf "field %d does not exist on %s" idx (sty (TTup ts))))
        | Some _ ->
            tyerr
              (sprintf "cannot load from %s because it is not a tuple type"
                 (sr rs))
        | None -> tyerr (sprintf "%s does not exist" (sr rs)))
    | Seq (Malloc (rd, ts), i) ->
        List.iter (check_type tctx) ts;
        let t = TTup (List.map (fun t -> (t, Uninit)) ts) in
        check_instr hctx tctx ((rd, t) :: rctx) i
    | Seq (Mov (rd, v), i) ->
        let t = check_sv hctx tctx rctx v in
        check_instr hctx tctx ((rd, t) :: rctx) i
    | Seq (St (rd, idx, rs), i) -> (
        match List.assoc_opt rd rctx with
        | Some (TTup ts) -> (
            match List.nth_opt ts idx with
            | Some (ti, _) ->
                expect_reg_ty rctx rs ti;
                let ts' =
                  List.mapi
                    (fun idx' (t, f) ->
                      if idx = idx' then (t, Init) else (t, f))
                    ts
                in
                check_instr hctx tctx ((rd, TTup ts') :: rctx) i
            | None ->
                tyerr
                  (sprintf "field %d does not exist on %s" idx (sty (TTup ts))))
        | Some _ ->
            tyerr
              (sprintf "cannot store into %s because it is not a tuple type"
                 (sr rd))
        | None -> tyerr (sprintf "%s does not exist" (sr rd)))
    | Seq (Unpack (a, rd, v), i) -> (
        match check_sv hctx tctx rctx v with
        | TExist (a', t) ->
            let i' = tysubst_instrs [ (a, TName a') ] i in
            check_instr hctx tctx ((rd, t) :: rctx) i'
        | _ ->
            tyerr
              (sprintf "cannot unpack %s because it is not existential" (ssv v))
        )
    | Jmp v -> (
        match check_sv hctx tctx rctx v with
        | TCall ([], rctx') -> expect_subrfile rctx rctx'
        | _ -> tyerr (sprintf "%s is not callable" (ssv v)))
    | Halt t -> expect_reg_ty rctx (R 1) t

  let check_hval hctx = function
    | HVTup wis ->
        TTup
          (List.map
             (function
               | WVJunk t -> (t, Uninit) | wv -> (check_wval hctx [] wv, Init))
             wis)
    | HVCode (typarams, rf, instrs) ->
        check_rftype typarams rf;
        check_instr hctx typarams rf instrs;
        TCall (typarams, rf)

  let check_heap hctx heap =
    check_htype hctx;
    List.iter
      (fun (l, hi) ->
        let exp_ti = List.assoc l hctx in
        let real_ti = check_hval hctx hi in
        if not (tyaeq exp_ti real_ti) then
          tyerr
            (sprintf "%s checked as %s but expected %s" (shv hi) (sty real_ti)
               (sty exp_ti)))
      heap

  let check_regs hctx rfile =
    List.map (fun (r, w) -> (r, check_wval hctx [] w)) rfile

  let check_prog (P (h, r, i)) =
    let hctx =
      List.map
        (function
          | l, HVCode (typarams, params, _) -> (l, TCall (typarams, params))
          | _, HVTup _ ->
              tyerr "cannot check tuples on program entry"
              (* doing so would require type inference b/c circular heap reference *))
        h
    in
    check_heap hctx h;
    let rctx = check_regs hctx r in
    check_instr hctx [] rctx i

  (*** Translation (7.4) ***)

  let trans_iflag = function A.Init -> Init | A.Uninit -> Uninit

  let rec trans_ty = function
    | A.TName a -> TName a
    | A.TInt -> TInt
    | A.TFn { typarams; params } ->
        TCall (typarams, List.mapi (fun i t -> (R (i + 1), trans_ty t)) params)
    | A.TTup ts ->
        TTup (List.map (fun (t, f) -> (trans_ty t, trans_iflag f)) ts)
    | A.TExist (a, t) -> TExist (a, trans_ty t)

  let rec trans_val gam = function
    | A.VVar x -> List.assoc x gam
    | A.VInt i -> SVWord (WVInt (Int.of_ocaml_int i))
    | A.VTyApp (A.Annot (v, _), o) -> SVTyApp (trans_val gam v, trans_ty o)
    | A.VPack (t1, A.Annot (v, _), t2) ->
        SVPack (trans_ty t1, trans_val gam v, trans_ty t2)

  let rec trans_wval = function
    | A.VVar _ -> failwith "cannot translate var into word"
    | A.VInt i -> WVInt (Int.of_ocaml_int i)
    | A.VTyApp (A.Annot (v, _), o) -> WVTyApp (trans_wval v, trans_ty o)
    | A.VPack (t1, A.Annot (v, _), t2) ->
        WVPack (trans_ty t1, trans_wval v, trans_ty t2)

  let rec trans_exp freshr freshl gam tctx rf = function
    | A.Let (A.DeclVal (x, A.Annot (u, t)), e) ->
        let r = freshr () in
        let gam' = (x, SVReg r) :: gam in
        let rf' = (r, trans_ty t) :: rf in
        let h, i = trans_exp freshr freshl gam' tctx rf' e in
        (h, Seq (Mov (r, trans_val gam u), i))
    | A.Let (A.DeclProj (x, Annot (u, TTup tis), idx), e) ->
        let r = freshr () in
        let gam' = (x, SVReg r) :: gam in
        let ti, _ = List.nth tis (idx - 1) in
        let rf' = (r, trans_ty ti) :: rf in
        let h, i = trans_exp freshr freshl gam' tctx rf' e in
        let mov = Mov (r, trans_val gam u) in
        let ld = Ld (r, r, idx - 1) in
        (h, Seq (mov, Seq (ld, i)))
    | A.Let (A.DeclProj _, _) -> failwith "projection of non-tuple"
    | A.Let (A.DeclOp (x, op, A.Annot (v1, _), A.Annot (v2, _)), e) ->
        let r = freshr () in
        let mov = Mov (r, trans_val gam v1) in
        let arith =
          let v2' = trans_val gam v2 in
          match op with
          | Plus -> Add (r, r, v2')
          | Minus -> Sub (r, r, v2')
          | Times -> Mul (r, r, v2')
        in
        let gam' = (x, SVReg r) :: gam in
        let rf' = (r, TInt) :: rf in
        let h, i = trans_exp freshr freshl gam' tctx rf' e in
        (h, Seq (mov, Seq (arith, i)))
    | A.Let (A.DeclUnpack (a, x, A.Annot (u, A.TExist (a', t))), e) ->
        let r = freshr () in
        let gam' = (x, SVReg r) :: gam in
        let t' = A.tysub a' (A.TName a) t in
        let tctx' = a :: tctx in
        let rf' = (r, trans_ty t') :: rf in
        let h, i = trans_exp freshr freshl gam' tctx' rf' e in
        (h, Seq (Unpack (a, r, trans_val gam u), i))
    | A.Let (A.DeclUnpack _, _) -> failwith "unpack of non-existential package"
    | A.Let (A.DeclTupMalloc (x, ts), e) ->
        let ts = List.map trans_ty ts in
        let r = freshr () in
        let ttup = TTup (List.map (fun t -> (t, Uninit)) ts) in
        let gam' = (x, SVReg r) :: gam in
        let rf' = (r, ttup) :: rf in
        let h, i = trans_exp freshr freshl gam' tctx rf' e in
        (h, Seq (Malloc (r, ts), i))
    | A.Let (A.DeclTupInit (x, A.Annot (u, A.TTup ts), idx, A.Annot (v, _)), e)
      ->
        let t' =
          TTup
            (List.mapi
               (fun i (t, f) ->
                 (trans_ty t, if i = idx - 1 then Init else trans_iflag f))
               ts)
        in
        let r = freshr () in
        let r' = freshr () in
        let gam' = (x, SVReg r) :: gam in
        let rf' = (r, t') :: rf in
        let movr = Mov (r, trans_val gam u) in
        let movr' = Mov (r', trans_val gam v) in
        let st = St (r, idx - 1, r') in
        let h, i = trans_exp freshr freshl gam' tctx rf' e in
        (h, Seq (movr, Seq (movr', Seq (st, i))))
    | A.Let (A.DeclTupInit _, _) -> failwith "init of non-tuple"
    | A.App (A.Annot (v, _), vis) ->
        let r0 = freshr () in
        let ris = List.map (fun _ -> freshr ()) vis in
        let mov0 = Mov (r0, trans_val gam v) in
        let movis =
          List.map2
            (fun ri (A.Annot (vi, _)) -> Mov (ri, trans_val gam vi))
            ris vis
        in
        let movargs = List.mapi (fun i ri -> Mov (R (i + 1), SVReg ri)) ris in
        let jmp = Jmp (SVReg r0) in
        ([], seq ((mov0 :: movis) @ movargs) jmp)
    | A.If0 (A.Annot (v, _), e1, e2) ->
        let h1, i1 = trans_exp freshr freshl gam tctx rf e1 in
        let h2, i2 = trans_exp freshr freshl gam tctx rf e2 in
        let l = freshl () in
        let r = freshr () in
        let h = HVCode (tctx, rf, i2) in
        let heap' = h1 @ h2 @ [ (l, h) ] in
        let mov = Mov (r, trans_val gam v) in
        let lfn =
          List.fold_left
            (fun fn a -> SVTyApp (fn, TName a))
            (SVWord (WVLabel l)) tctx
        in
        let bnz = Bnz (r, lfn) in
        (heap', seq [ mov; bnz ] i1)
    | A.Halt (t, A.Annot (v, _)) ->
        let mov = Mov (R 1, trans_val gam v) in
        let halt = Halt (trans_ty t) in
        ([], Seq (mov, halt))

  let make_freshr regs =
    let maxr = List.fold_left (fun n (R m) -> max n m) 0 regs in
    let r = ref maxr in
    fun () ->
      incr r;
      R !r

  let make_freshl labs =
    let gen = fresh_generator (SSet.of_list labs) in
    fun () -> Lab (gen "_l")

  let trans_hval freshl gam = function
    | A.HCode { typarams; params; body } ->
        let argregs = List.mapi (fun i _ -> R (i + 1)) params in
        let rf = List.map2 (fun r (_, t) -> (r, trans_ty t)) argregs params in
        let param_reg_map =
          List.map2 (fun r (p, _) -> (p, SVReg r)) argregs params
        in
        let gam' = param_reg_map @ gam in
        let freshr = make_freshr argregs in
        let h, i = trans_exp freshr freshl gam' typarams rf body in
        (h, HVCode (typarams, rf, i))
    | A.HTup vs ->
        let fields = List.map (fun (A.Annot (v, _)) -> trans_wval v) vs in
        ([], HVTup fields)

  let trans_prog = function
    | A.Prog (defs, e) ->
        let deflabels = List.map (fun (x, _) -> Lab x) defs in
        let gam =
          List.map2 (fun (x, _) l -> (x, SVWord (WVLabel l))) defs deflabels
        in
        let freshl = make_freshl (List.map fst defs) in
        let heapis, his =
          List.map (trans_hval freshl gam) (List.map snd defs) |> List.split
        in
        let heape, i = trans_exp (make_freshr []) freshl gam [] [] e in
        let hroot = List.map2 (fun li hi -> (li, hi)) deflabels his in
        let h = List.concat ((hroot :: heapis) @ [ heape ]) in
        P (h, [], i)
end
