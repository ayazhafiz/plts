(** Lambda-Lifting and Linearization pass *)

module C = Cast_ir
module CE = Cast_ir.Expr
module L = Language
open Util

type ty =
  | TUnknown
  | TNat
  | TBool
  | TClos of ty * ty
  | TNamedTup of (string * ty) list

type expr =
  | Nat of int
  | Bool of bool
  | Name of string
  | Apply of elaborated * elaborated
  (* A projection of a variable stored in a closure env *)
  | Proj of string * int
  (* Packages a function pointer and its environment into a closure *)
  | Pack of elaborated * (string * ty) list

and env = Tup of (string * ty) list

and elaborated = Elab of expr * ty

type stmt =
  | Decl of string * ty
  | DeclInit of string * ty * elaborated
  | DeclCast of string * ty * elaborated
  | Assign of string * elaborated
  | If of elaborated * stmt list * stmt list

type body = Body of stmt list * elaborated

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
    | TClos (t1, t2) ->
        fprintf f "@[<hov 2>Clos(";
        if left_arrow_child then pp_print_string f "(";
        go ~left_arrow_child:true t1;
        fprintf f " ->@ ";
        go t2;
        if left_arrow_child then pp_print_string f ")";
        fprintf f ")@]"
    (*
       | TClos (t1, t2) ->
           fprintf f "@[<hov 2>Clos@,(@[<hov 2>";
           go t1;
           fprintf f ")@]@,(@[<hov 2>";
           go t2;
           fprintf f "@])@]" *)
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
    | Proj (s, i) -> fprintf f "%s.%d" s i
    | Pack (s, ps) ->
        fprintf f "@[<hov 2>pack(";
        go s;
        fprintf f ", ";
        fprintf f "{@[<hv 0>";
        let lasti = List.length ps - 1 in
        List.iteri
          (fun i (p, t) ->
            fprintf f "@[<hov 2>%s:@ " p;
            pp_ty f t;
            fprintf f "@]";
            if i <> lasti then fprintf f ",@ ")
          ps;
        fprintf f "@]})@]"
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
        fprintf f "@]@ @[<v 2>else@ ";
        let lasti = List.length e - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          e;
        fprintf f "@]@]"
  in
  go

let pp_body f (Body (stmts, e)) =
  let open Format in
  fprintf f "@[<v 0>";
  List.iter
    (fun s ->
      pp_stmt f s;
      fprintf f "@,")
    stmts;
  pp_expr f e;
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

let string_of_program prog = with_buffer (fun f -> pp_program f prog) 80

let rec translate_ty = function
  | L.TUnknown -> TUnknown
  | L.TNat -> TNat
  | L.TBool -> TBool
  | L.TArrow (t, t') -> TClos (translate_ty t, translate_ty t')

let split_arrow = function
  | TClos (l, r) -> (l, r)
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
        (* Two cases here.
           1. Closures
                fn arg
              becomes
                let r = apply(fn, arg) in
                r
           2. Closed functions
              TODO
        *)
        let stmts1, fn = go fn in
        let stmts2, arg = go arg in
        match tyof fn with
        | TClos (_, rT) ->
            let r = fresh "r" in
            let rDecl = DeclInit (r, rT, Elab (Apply (fn, arg), rT)) in
            (stmts1 @ stmts2 @ [ rDecl ], Elab (Name r, rT))
        (*
        | TArrow (_, rT) ->
            let r = fresh "r" in
            let target =
              match target with
              | Elab (Name x, _) -> x
              | _ -> failwith (se target ^ " is not a name")
            in
            let rDecl = DeclInit (r, rT, Elab (Call (target, [ arg ]), rT)) in
            (stmts1 @ stmts2 @ [ rDecl ], Elab (Name r, rT))
        *)
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
        *)
        let free =
          SMap.remove x (C.freevars_with_tys e)
          |> SMap.to_seq |> List.of_seq
          |> List.map (fun (v, t) -> (v, translate_ty t))
        in
        let gen = fresh "gen" in
        let env = fresh "env" in
        let envT = TNamedTup free in
        let envUnpack =
          List.mapi
            (fun i (v, t) -> DeclInit (v, t, Elab (Proj (env, i), t)))
            free
        in
        let _, ret = split_arrow t in
        let body_s, body_e = go e in
        let fn =
          {
            name = gen;
            params = [ (env, envT); (x, translate_ty t') ];
            body = Body (envUnpack @ body_s, body_e);
            ret;
          }
        in
        toplevels := fn :: !toplevels;
        let genC = fresh (gen ^ "C") in
        let genCTy = t in
        let clos = Elab (Pack (Elab (Name gen, genCTy), free), genCTy) in
        let genCDecl = DeclInit (genC, genCTy, clos) in
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
    | CE.Loc _ | CE.Builtin _ -> failwith "untranslatable (only for eval)"
  in
  let stmts, e = go expr in
  let body = Body (stmts, e) in
  { toplevels = List.rev !toplevels; body; fresh; ty = tyof e }
