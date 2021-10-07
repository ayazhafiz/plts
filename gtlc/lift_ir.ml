(** Lambda-Lifting and Linearization pass *)

module C = Cast_ir
module CE = Cast_ir.Expr
module L = Language
open Util

type ty =
  | TUnknown
  | TNat
  | TBool
  | TArrow of ty * ty
      (** An arrow type, which can either be a closure or raw function pointer. *)
  | TNamedTup of (string * ty) list
  | TBox of ty  (** A boxed type lives on the heap. *)

type expr =
  | Nat of int
  | Bool of bool
  | Name of string
  | Apply of elaborated * elaborated
  | Proj of elaborated * int
      (** A projection of a variable stored in a closure env *)
  | PackClos of elaborated * elaborated list
      (** Packages a function pointer and its environment into a closure *)
  | PackFnPtr of elaborated  (** A raw function pointer *)
  | Box of elaborated
  | BoxEnplace of elaborated * elaborated
  | Unbox of elaborated

and env = Tup of (string * ty) list

and elaborated = Elab of expr * ty

type stmt =
  | Decl of string * ty  (** Mutable declaration. *)
  | DeclInit of string * ty * elaborated  (** Constant declaration. *)
  | DeclCast of string * ty * elaborated
  | Assign of string * elaborated
  | If of elaborated * stmt list * stmt list
  | Return of elaborated

type body = Body of stmt list

type fn = { name : string; params : (string * ty) list; body : body; ret : ty }

type program = {
  toplevels : fn list;
  body : body;
  ty : ty;
  fresh : string -> string;
}

let pp_ty f =
  let open Format in
  let rec go ?(left_arrow_child = false) = function
    | TUnknown -> pp_print_string f "?"
    | TNat -> pp_print_string f "nat"
    | TBool -> pp_print_string f "bool"
    | TArrow (t1, t2) ->
        fprintf f "@[<hov 2>Clos(";
        if left_arrow_child then pp_print_string f "(";
        go ~left_arrow_child:true t1;
        fprintf f " ->@ ";
        go t2;
        if left_arrow_child then pp_print_string f ")";
        fprintf f ")@]"
    | TNamedTup fs ->
        fprintf f "@[{@[<hv 0>";
        let lasti = List.length fs - 1 in
        List.iteri
          (fun i (x, t) ->
            fprintf f "@[<hov 2>%s:@ " x;
            go t;
            fprintf f "@]";
            if i <> lasti then fprintf f ",@ ")
          fs;
        fprintf f "@]}@]"
    | TBox t ->
        fprintf f "@[Box(";
        go t;
        fprintf f "@]"
  in
  go

let pp_expr f =
  let open Format in
  let rec go (Elab (e, _)) =
    match e with
    | Nat n -> pp_print_int f n
    | Bool b -> pp_print_bool f b
    | Name x -> pp_print_string f x
    | Apply (head, arg) ->
        fprintf f "@[<hov 2>apply(@[<hv 0>";
        go head;
        fprintf f ",@ ";
        go arg;
        fprintf f "@])@]"
    | Proj (s, i) ->
        go s;
        fprintf f ".%d" i
    | PackClos (s, ps) ->
        fprintf f "@[<hov 2>pack(";
        go s;
        fprintf f ", ";
        fprintf f "{@[<hv 0>";
        let lasti = List.length ps - 1 in
        List.iteri
          (fun i p ->
            fprintf f "@[<hov 2>";
            go p;
            fprintf f "@]";
            if i <> lasti then fprintf f ",@ ")
          ps;
        fprintf f "@]})@]"
    | PackFnPtr s ->
        fprintf f "@[<hov 2>fnptr(";
        go s;
        fprintf f ")@]"
    | Box s ->
        fprintf f "@[<hov 2>box(";
        go s;
        fprintf f ")@]"
    | BoxEnplace (e1, e2) ->
        fprintf f "@[<hov 2>box_enplace(@[<hv 0>";
        go e1;
        fprintf f ",@ ";
        go e2;
        fprintf f "@])@]"
    | Unbox s ->
        fprintf f "@[<hov 2>unbox(";
        go s;
        fprintf f ")@]"
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
        fprintf f "decl @[<hov 2>%s: " x;
        pp_ty f t;
        fprintf f "@ = ";
        pp_expr f e;
        fprintf f ";@]"
    | DeclCast (x, t, e) ->
        fprintf f "decl @[<hov 2>%s@,: " x;
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
        fprintf f "@]@ @[<v 2>else@ ";
        let lasti = List.length e - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          e;
        fprintf f "@]@]"
    | Return e ->
        fprintf f "@[<hov 2>return@ ";
        pp_expr f e;
        fprintf f ";@]"
  in
  go

let pp_body f (Body stmts) =
  let open Format in
  fprintf f "@[<v 0>";
  let lasti = List.length stmts - 1 in
  List.iteri
    (fun i s ->
      fprintf f "@[";
      pp_stmt f s;
      fprintf f "@]";
      if i <> lasti then fprintf f "@,")
    stmts;
  fprintf f "@]"

let pp_fn f { name; params; body; ret } =
  let open Format in
  fprintf f "@[<v 2>@[<hov 2>fn %s(@[<hv 0>" name;
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
  fprintf f "@]@]@,= @[<v 0>";
  pp_body f body;
  fprintf f "@]@]"

let pp_program f { toplevels; body; _ } =
  let open Format in
  fprintf f "@[<v 0>";
  List.iter
    (fun fn ->
      pp_fn f fn;
      fprintf f "@,")
    toplevels;
  pp_body f body;
  fprintf f "@]"

let se e = with_buffer (fun f -> pp_expr f e) 80

let sty t = with_buffer (fun f -> pp_ty f t) 80

let string_of_program ?(width = default_width) prog =
  with_buffer (fun f -> pp_program f prog) width

let rec translate_ty = function
  | L.TUnknown -> TUnknown
  | L.TNat -> TNat
  | L.TBool -> TBool
  | L.TArrow (t, t') -> TArrow (translate_ty t, translate_ty t')
  | L.TRef t -> TBox (translate_ty t)
  | L.TInfer (`Resolved t) -> translate_ty t
  | L.TInfer (`Var _) ->
      failwith "unreachable: inference type variable is unresolved"

let split_arrow = function
  | TArrow (l, r) -> (l, r)
  | _ -> failwith "not an arrow"

let tyof (Elab (_, t)) = t

(** Does three things:
      1. Linearize expressions into statements
      2. Convert inline lambdas into closures
      3. Hoist closure definitions *)
let translate expr =
  let fresh = fresh_generator ~used:(C.allvars expr) () in
  let toplevels = ref [] in
  let rec go (CE.Elab (e, t)) =
    let t = translate_ty t in
    match e with
    | CE.Nat n -> ([], Elab (Nat n, t))
    | CE.Bool b -> ([], Elab (Bool b, t))
    | CE.Var (`Global x | `Local x) ->
        (* In this pass we will have converted all functions to closures, so the
           distinction between locally and globally defined variables no longer
           matters as much. *)
        ([], Elab (Name x, t))
    | CE.App (fn, arg) -> (
        (* fn arg
             becomes
           let r = apply(fn, arg) in
           r
        *)
        let stmts1, fn = go fn in
        let stmts2, arg = go arg in
        match tyof fn with
        | TArrow (_, rT) ->
            let r = fresh "r" in
            let rDecl = DeclInit (r, rT, Elab (Apply (fn, arg), rT)) in
            (stmts1 @ stmts2 @ [ rDecl ], Elab (Name r, rT))
        | t ->
            failwith
              ("applying to non-closure and non-function type: found " ^ sty t))
    | CE.Lam (x, t', e) ->
        (* \x: nat. add (add x y) z becomes
           fn gen1(env: (x: .., y: ..), x: nat) -> nat
             = let x = env.0 in
               let y = env.1 in
               add (add x y) z
           ...
           let gen1C = pack(gen1, (y, z)) in
           gen1C

           unless the environment is empty, in which case the package is a
           trivial function pointer.
        *)
        let free =
          SMap.remove x (C.freevars_with_tys e)
          |> SMap.to_seq |> List.of_seq
          |> List.map (fun (v, t) -> (v, translate_ty t))
        in
        let gen = fresh "gen" in
        let _, fnRet = split_arrow t in
        let body_s, body_e = go e in
        let genC = fresh (gen ^ "C") in
        let genCTy = t in
        if List.length free <> 0 then (
          let env = fresh "env" in
          let envT = TNamedTup free in
          let envE = Elab (Name env, envT) in
          let envUnpack =
            List.mapi
              (fun i (v, t) -> DeclInit (v, t, Elab (Proj (envE, i), t)))
              free
          in
          let fn =
            {
              name = gen;
              params = [ (env, envT); (x, translate_ty t') ];
              body = Body (envUnpack @ body_s @ [ Return body_e ]);
              ret = fnRet;
            }
          in
          toplevels := fn :: !toplevels;
          let freeLst = List.map (fun (x, t) -> Elab (Name x, t)) free in
          let clos =
            Elab (PackClos (Elab (Name gen, genCTy), freeLst), genCTy)
          in
          let genCDecl = DeclInit (genC, genCTy, clos) in
          ([ genCDecl ], Elab (Name genC, genCTy)))
        else
          let fn =
            {
              name = gen;
              params = [ (x, translate_ty t') ];
              body = Body (body_s @ [ Return body_e ]);
              ret = fnRet;
            }
          in
          toplevels := fn :: !toplevels;
          let fnPtr = Elab (PackFnPtr (Elab (Name gen, genCTy)), genCTy) in
          let genCDecl = DeclInit (genC, genCTy, fnPtr) in
          ([ genCDecl ], Elab (Name genC, genCTy))
    | CE.If (c, thn, els) ->
        let r = fresh "res" in
        let declr = Decl (r, t) in
        let stmts_c, c = go c in
        let stmts_thn, thn = go thn in
        let stmts_els, els = go els in
        let if' =
          If
            (c, stmts_thn @ [ Assign (r, thn) ], stmts_els @ [ Assign (r, els) ])
        in
        (stmts_c @ [ declr; if' ], Elab (Name r, t))
    | CE.Cast (t, e) ->
        let stmts, e = go e in
        let x = fresh "x" in
        let t = translate_ty t in
        (stmts @ [ DeclCast (x, t, e) ], Elab (Name x, t))
    | CE.Ref e ->
        let stmts, e = go e in
        let r = fresh "r" in
        (stmts @ [ DeclInit (r, t, Elab (Box e, t)) ], Elab (Name r, t))
    | CE.Deref e ->
        let stmts, e = go e in
        let r = fresh "r" in
        (stmts @ [ DeclInit (r, t, Elab (Unbox e, t)) ], Elab (Name r, t))
    | CE.RefAssign (e1, e2) ->
        let stmts1, e1 = go e1 in
        let stmts2, e2 = go e2 in
        let refty = tyof e1 in
        let enplace = Elab (BoxEnplace (e1, e2), refty) in
        let r = fresh "r" in
        ( stmts1 @ stmts2 @ [ DeclInit (r, refty, enplace) ],
          Elab (Name r, refty) )
    | CE.Loc _ | CE.Builtin _ -> failwith "untranslatable (only for eval)"
  in
  let stmts, e = go expr in
  let body = Body (stmts @ [ Return e ]) in
  { toplevels = List.rev !toplevels; body; fresh; ty = tyof e }

(** An optimization pass. *)
module type Opt = sig
  val apply : program -> program
end

(** Collapses variables where their declaration is redundant (e.g. for constants.) *)
module CollapseVars : Opt = struct
  let collect_decls stmts =
    let seen = ref [] in
    (* On average, almost all the statements are declarations. *)
    let decls = Hashtbl.create (List.length stmts) in
    List.iter
      (function
        | DeclInit (x, _, _) when List.mem x !seen ->
            (* Variable is shadowed. For sake of simplicity, do not attempt to
               collapse any instance of this name. *)
            Hashtbl.remove decls x
        | DeclInit (x, _, e) ->
            Hashtbl.add decls x e;
            seen := x :: !seen
        (* We can only eliminate constant variables. Don't attempt to eliminate
           casts as that makes the generated code harder to read. *)
        | _ -> ())
      stmts;
    decls

  let vars_to_collapse cand_vars stmts =
    let usages =
      List.map (fun v -> (v, 0)) cand_vars |> List.to_seq |> Hashtbl.of_seq
    in
    let rec goe (Elab (e, _)) =
      match e with
      | Nat _ | Bool _ -> ()
      | Name x -> (
          match Hashtbl.find_opt usages x with
          | Some n -> Hashtbl.add usages x (n + 1)
          | None -> ())
      | Apply (e1, e2) | BoxEnplace (e1, e2) ->
          goe e1;
          goe e2
      | PackClos (e, es) ->
          goe e;
          List.iter goe es
      | PackFnPtr e | Proj (e, _) | Box e | Unbox e -> goe e
    in
    let rec gos = function
      | Decl _ -> ()
      | DeclInit (_, _, e) | DeclCast (_, _, e) | Assign (_, e) -> goe e
      | Return e -> goe e
      | If (e, s1, s2) ->
          goe e;
          List.iter gos s1;
          List.iter gos s2
    in
    List.iter gos stmts;
    List.filter (fun v -> Hashtbl.find usages v <= 1) cand_vars

  let subst_elim_vars decls stmts =
    let rec goe (Elab (e, t)) =
      let e' =
        match e with
        | Nat _ | Bool _ -> e
        | Apply (e1, e2) -> Apply (goe e1, goe e2)
        | PackClos (e, es) -> PackClos (goe e, List.map goe es)
        | PackFnPtr e -> PackFnPtr (goe e)
        | Proj (e, i) -> Proj (goe e, i)
        | Box e -> Box (goe e)
        | BoxEnplace (e1, e2) -> BoxEnplace (goe e1, goe e2)
        | Unbox e -> Unbox (goe e)
        | Name x -> (
            match Hashtbl.find_opt decls x with
            | Some (Elab (e', _)) -> e'
            | None -> Name x)
      in
      Elab (e', t)
    in
    let rec gos s =
      match s with
      | Decl _ -> Some s
      | DeclInit (x, _, e) when Hashtbl.mem decls x ->
          (* We have to be a bit careful here, since the decl expression could
             itself have variables we are eliminating. So update the expression
             first, then update the binding, and finally mark the declaration
             for elimination. *)
          let e' = goe e in
          Hashtbl.add decls x e';
          None
      | DeclInit (x, t, e) -> Some (DeclInit (x, t, goe e))
      | DeclCast (x, t, e) -> Some (DeclCast (x, t, goe e))
      | Assign (x, e) -> Some (Assign (x, goe e))
      | If (e, s1, s2) ->
          Some (If (goe e, List.filter_map gos s1, List.filter_map gos s2))
      | Return e -> Some (Return (goe e))
    in
    List.filter_map gos stmts

  let rec do_stmtlist stmts =
    let stmts = List.map scope_recurse stmts in
    let decls = collect_decls stmts in
    let cand_vars = Hashtbl.to_seq_keys decls |> List.of_seq in
    let vars = vars_to_collapse cand_vars stmts in
    Hashtbl.filter_map_inplace
      (fun v e -> if List.mem v vars then Some e else None)
      decls;
    let stmts' = subst_elim_vars decls stmts in
    stmts'

  and scope_recurse s =
    match s with
    | Decl _ | DeclInit _ | DeclCast _ | Assign _ | Return _ -> s
    | If (e, s1, s2) -> If (e, do_stmtlist s1, do_stmtlist s2)

  let do_body (Body stmts) = Body (do_stmtlist stmts)

  let apply_fn (fn : fn) = { fn with body = do_body fn.body }

  let apply { toplevels; body; fresh; ty } =
    let toplevels = List.map apply_fn toplevels in
    let body = do_body body in
    { toplevels; body; fresh; ty }
end
