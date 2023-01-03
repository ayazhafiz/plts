open Surface
open Util

let tightest_node_at loc program =
  let open Ast in
  let or_else o f = match o with Some a -> Some a | None -> Lazy.force f in
  let or_with_default deeper (l', t, kind) =
    or_else deeper
      (lazy (if within loc l' then Some (l', `Ty t, kind) else None))
  in
  let or_list l = List.fold_left or_else None l in
  let def (l, t, x) = if within loc l then Some (l, `Ty t, `Def x) else None in
  let def_cap (l, t, x) =
    if within loc l then Some (l, `FxSig t, `Def x) else None
  in
  let rec stmt (l, t, s) =
    let deeper =
      match s with
      | Let (_, d, s, s') ->
          or_list [ lazy (def d); lazy (stmt s); lazy (stmt s') ]
      | App (e1, e2) -> or_list [ lazy (expr e1); lazy (expr e2) ]
      | Return e -> expr e
      | If (e1, e2, e3) ->
          or_list [ lazy (stmt e1); lazy (stmt e2); lazy (stmt e3) ]
      | Handle (c, h, rest) ->
          or_list [ lazy (def_cap c); lazy (cap h); lazy (stmt rest) ]
    in
    or_with_default deeper (l, t, `Generic)
  and expr (l, t, e) =
    let deeper =
      match e with
      | Var _ -> None
      | Lit _ -> None
      | Builtin _ -> None
      | Abs (x, e) -> or_list [ lazy (def x); lazy (stmt e) ]
    in
    let kind = match e with Var x -> `Var x | _ -> `Generic in
    or_with_default deeper (l, t, kind)
  and cap (l, t, c) =
    let deeper =
      match c with
      | CapVar _ -> None
      | HandlerImpl (_op, (x, k), body) ->
          or_list [ lazy (def x); lazy (def_cap k); lazy (stmt body) ]
    in
    or_with_default deeper (l, t, `Generic)
  in
  stmt program

let type_at loc program =
  match tightest_node_at loc program with
  | Some (l, t, _) when l = loc -> Some t
  | _ -> None

let print_type = function
  | `Ty t -> Ty_print.string_of_ty default_width t
  | `FxSig t -> Ty_print.string_of_fx_sig default_width t

let hover_info lineco program =
  let open Printf in
  let wrap_code code = sprintf "```asti\n%s\n```" code in
  let gen_docs (range, t, kind) =
    let ty_str = print_type t in
    let prefix =
      match kind with
      | `Var x -> sprintf "(var) %s: " x
      | `Def x -> sprintf "(decl) %s: " x
      | `Pat -> ""
      | `Generic -> ""
    in
    let ty_doc = prefix ^ ty_str in
    let md_docs = [ wrap_code ty_doc ] in
    { range; md_docs }
  in
  let node = tightest_node_at (lineco, lineco) program in
  Option.map gen_docs node
