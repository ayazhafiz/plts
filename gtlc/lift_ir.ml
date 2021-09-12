(** Lambda-Lifting and Linearization pass *)

module C = Cast_ir
module CE = Cast_ir.Expr
open Util

module ClosConv = struct
  open Language

  type ty = Language.ty

  type closed_expr =
    | Nat of int
    | Bool of bool
    | Var of string
    | App of elaborated * elaborated
    | Lam of (string * ty) list * elaborated
    | If of elaborated * elaborated * elaborated
    | Cast of ty * elaborated

  and elaborated = Elab of closed_expr * ty

  let rec apply_fvs base base_ty fvs =
    match (base_ty, fvs) with
    | _, [] -> base
    | TArrow (_, ty'), (fv, t) :: fvs' ->
        apply_fvs (Elab (App (base, Elab (Var fv, t)), ty')) ty' fvs'
    | _ -> failwith "not arrow type"

  let rec clos_conv (CE.Elab (e, t)) =
    let e' =
      match e with
      | CE.Nat n -> Nat n
      | CE.Bool b -> Bool b
      | CE.Var x -> Var x
      | CE.App (e1, e2) -> App (clos_conv e1, clos_conv e2)
      | CE.If (e1, e2, e3) -> If (clos_conv e1, clos_conv e2, clos_conv e3)
      | CE.Cast (t, e) -> Cast (t, clos_conv e)
      | CE.Lam (x, t', e) ->
          let free =
            SMap.remove x (C.freevars_with_tys e) |> SMap.to_seq |> List.of_seq
          in
          let freetys = List.map snd free in
          let lamt = t in
          let closed_lam_ty =
            List.fold_right (fun fvt rest -> TArrow (fvt, rest)) freetys lamt
          in
          let closed_lam =
            Elab (Lam (free @ [ (x, t') ], clos_conv e), closed_lam_ty)
          in
          let (Elab (applied_lam, lamt')) =
            apply_fvs closed_lam closed_lam_ty free
          in
          (* correctness check *)
          if lamt <> lamt' then failwith "lambda closure conversion incorrect";
          applied_lam
      | CE.Builtin _ | CE.Loc _ -> failwith "untranslatable; only for eval"
    in
    Elab (e', t)
end

module Hoist = struct
  open Language

  type ty = Language.ty

  type expr =
    | Nat of int
    | Bool of bool
    | Name of string
    | Call of elaborated * elaborated list
    | If of elaborated * elaborated * elaborated
    | Cast of ty * elaborated

  and elaborated = Elab of expr * ty

  type fn = {
    name : string;
    params : (string * ty) list;
    body : elaborated;
    ret : ty;
  }

  type program = {
    toplevels : fn list;
    body : elaborated;
    fresh : string -> string;
  }

  module CC = ClosConv

  let rec unroll_arrow t n =
    match (t, n) with
    | t, 0 -> t
    | TArrow (_, t'), n -> unroll_arrow t' (n - 1)
    | _ -> failwith "not an arrow"

  let rec allvars (CC.Elab (e, _)) =
    let open CC in
    let open S in
    match e with
    | Nat _ | Bool _ -> empty
    | Var x -> singleton x
    | App (e1, e2) -> union (allvars e1) (allvars e2)
    | Lam (ps, e) -> List.map fst ps |> S.of_list |> union (allvars e)
    | If (e1, e2, e3) -> union (allvars e1) (allvars e2) |> union (allvars e3)
    | Cast (_, e) -> allvars e

  let hoist e =
    let fresh = fresh_generator ~used:(allvars e) () in
    let toplevels = ref [] in
    let rec go (CC.Elab (e, t)) =
      let e' =
        match e with
        | CC.Nat n -> Nat n
        | CC.Bool b -> Bool b
        | CC.Var x -> Name x
        | CC.App (e1, e2) ->
            (* Keep as a binary list for now. We will unroll this in the compaction phase. *)
            Call (go e1, [ go e2 ])
        | CC.If (e1, e2, e3) -> If (go e1, go e2, go e3)
        | CC.Cast (t, e) -> Cast (t, go e)
        | CC.Lam (params, body) ->
            let body' = go body in
            let name = fresh "gen" in
            let ret = unroll_arrow t (List.length params) in
            let fn = { name; params; body = body'; ret } in
            toplevels := fn :: !toplevels;
            Name name
      in
      Elab (e', t)
    in
    let body = go e in
    { toplevels = List.rev !toplevels; body; fresh }

  (** [compact expr] collapses applications into a proper sequence. *)
  let rec compact (Elab (e, t)) =
    let e' =
      match e with
      | Call (target, [ p ]) -> (
          (* Our parsing convention is such that
               f p1 p2 (f2 p3)
               becomes
               ((f p1) p2) (f2 p3)
               ^^^^^^^^^^^ ^^^^^^^
               target      p
             Unroll `target` to yield the sequence [f; p1; p2]; unroll p
             independently but keep it as a separate parameter, as we cannot unroll
             its components. *)
          let rec unroll = function
            | Elab (Call (f, [ p ]), _) ->
                let fs = unroll f in
                let p' = compact p in
                fs @ [ p' ]
            | f -> [ f ]
          in
          let target = unroll target in
          let p' = compact p in
          match target @ [ p' ] with
          | hd :: args -> Call (hd, args)
          | _ -> failwith "impossible")
      | Call _ -> failwith "impossible"
      | Nat n -> Nat n
      | Bool b -> Bool b
      | Name x -> Name x
      | If (e1, e2, e3) -> If (compact e1, compact e2, compact e3)
      | Cast (t, e) -> Cast (t, compact e)
    in
    Elab (e', t)

  let compact_program { toplevels; body; fresh } =
    let toplevels =
      List.map (fun (fn : fn) -> { fn with body = compact fn.body }) toplevels
    in
    { toplevels; body = compact body; fresh }
end

module Linearize = struct
  open Language
  module H = Hoist

  type ty = Language.ty

  type expr =
    | Nat of int
    | Bool of bool
    | Name of string
    | Call of elaborated * elaborated list

  and elaborated = Elab of expr * ty

  type stmt =
    | Decl of string * ty
    | DeclInit of string * ty * elaborated
    | DeclCast of string * ty * elaborated
    | Assign of string * elaborated
    | If of elaborated * stmt list * stmt list

  type seq = Expr of elaborated | Seq of stmt * seq

  type fn = { name : string; params : (string * ty) list; body : seq; ret : ty }

  type program = {
    toplevels : fn list;
    body : seq;
    ty : ty;
    fresh : string -> string;
  }

  let translate fresh expr =
    let rec go (H.Elab (e, t)) =
      match e with
      | H.Nat n -> ([], Elab (Nat n, t))
      | H.Bool b -> ([], Elab (Bool b, t))
      | H.Name x -> ([], Elab (Name x, t))
      | H.Call (target, params) ->
          let stmts, fn = go target in
          let stmts', params =
            List.fold_right
              (fun p (stmts, params) ->
                let s, p = go p in
                (s @ stmts, p :: params))
              params ([], [])
          in
          let x = fresh "x" in
          let call = Elab (Call (fn, params), t) in
          let declx = DeclInit (x, t, call) in
          (stmts @ stmts' @ [ declx ], Elab (Name x, t))
      | If (c, thn, els) ->
          let r = fresh "res" in
          let declr = Decl (r, t) in
          let stmts_c, c = go c in
          let stmts_thn, thn = go thn in
          let stmts_els, els = go els in
          let if' =
            If
              ( c,
                stmts_thn @ [ Assign (r, thn) ],
                stmts_els @ [ Assign (r, els) ] )
          in
          (stmts_c @ [ declr; if' ], Elab (Name r, t))
      | Cast (t, e) ->
          let stmts, e = go e in
          let x = fresh "x" in
          (stmts @ [ DeclCast (x, t, e) ], Elab (Name x, t))
    in
    let stmts, e = go expr in
    List.fold_right (fun s seq -> Seq (s, seq)) stmts (Expr e)

  let translate_fn fresh ({ name; params; body; ret } : H.fn) =
    { name; params; body = translate fresh body; ret }

  let translate_program
      ({ toplevels; body = H.Elab (_, t) as body; fresh } : H.program) =
    let toplevels = List.map (translate_fn fresh) toplevels in
    let body = translate fresh body in
    { toplevels; body; ty = t; fresh }

  let pp_expr f =
    let open Format in
    let rec go (Elab (e, _)) =
      match e with
      | Nat n -> pp_print_int f n
      | Bool b -> pp_print_bool f b
      | Name x -> pp_print_string f x
      | Call (head, params) ->
          fprintf f "@[<hov 2>";
          go head;
          fprintf f "(@[<hv 0>";
          let lasti = List.length params - 1 in
          List.iteri
            (fun i p ->
              go p;
              if i <> lasti then fprintf f ",@ ")
            params;
          fprintf f "@])@]"
    in
    go

  let pp_stmt f =
    let open Format in
    let rec go = function
      | Decl (x, t) ->
          fprintf f "decl @[<hov 2>%s@,: " x;
          pp_ty f t;
          fprintf f ";@]"
      | DeclInit (x, t, e) ->
          fprintf f "decl @[<hv 2>%s@,: " x;
          pp_ty f t;
          fprintf f "@ = ";
          pp_expr f e;
          fprintf f ";@]"
      | DeclCast (x, t, e) ->
          fprintf f "decl @[<hv 2>%s@,: " x;
          pp_ty f t;
          fprintf f "@ = <";
          pp_ty f t;
          fprintf f ">";
          pp_expr f e;
          fprintf f ";@]"
      | Assign (x, e) ->
          fprintf f "@[<hv 2>%s@ = " x;
          pp_expr f e;
          fprintf f ";@]"
      | If (c, t, e) ->
          fprintf f "@[<hv 0>@[<hov 2>if@ ";
          pp_expr f c;
          fprintf f "@]@ @[<v 2>then@ ";
          let lasti = List.length t - 1 in
          List.iteri
            (fun i s ->
              go s;
              if i <> lasti then fprintf f "@,")
            t;
          fprintf f "@]@ @[<hov 2>else@ ";
          let lasti = List.length t - 1 in
          List.iteri
            (fun i s ->
              go s;
              if i <> lasti then fprintf f "@,")
            e;
          fprintf f "@]@]"
    in
    go

  let pp_seq f =
    let open Format in
    let rec go = function
      | Expr e -> pp_expr f e
      | Seq (stmt, rest) ->
          pp_stmt f stmt;
          fprintf f "@,";
          go rest
    in
    go

  let pp_fn f { name; params; body; ret } =
    let open Format in
    fprintf f "@[<hov 2>fn %s(@[<hv 0>" name;
    let lasti = List.length params - 1 in
    List.iteri
      (fun i (p, t) ->
        fprintf f "@[<hov 2>%s:@ " p;
        pp_ty f t;
        fprintf f "@]";
        if i <> lasti then fprintf f ",@ ")
      params;
    fprintf f "@])@,: @[";
    pp_ty f ret;
    fprintf f "@]@ = @[<v 0>";
    pp_seq f body;
    fprintf f "@]@]"

  let pp_program f { toplevels; body; _ } =
    let open Format in
    fprintf f "@[<v 0>";
    List.iter
      (fun fn ->
        pp_fn f fn;
        fprintf f "@,")
      toplevels;
    pp_seq f body;
    fprintf f "@]"

  let string_of_program prog = with_buffer (fun f -> pp_program f prog) 80
end

let translate cast_expr =
  cast_expr |> ClosConv.clos_conv |> Hoist.hoist |> Hoist.compact_program
  |> Linearize.translate_program
