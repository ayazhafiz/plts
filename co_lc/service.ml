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
  let rec expr (l, t, e) =
    let deeper =
      match e with
      | Var _ | Lit _ | Yield -> None
      | Tup es -> or_list @@ List.map (fun e -> lazy (expr e)) es
      | Abs (_, x, e) -> or_list [ lazy (def x); lazy (expr e) ]
      | Let (_, d, s, s') ->
          or_list [ lazy (def d); lazy (expr s); lazy (expr s') ]
      | App (e1, e2) | Binop (_, e1, e2) ->
          or_list [ lazy (expr e1); lazy (expr e2) ]
      | If (e1, e2, e3) ->
          or_list [ lazy (expr e1); lazy (expr e2); lazy (expr e3) ]
      | Access (e, _) | Spawn e | Resume e -> expr e
      | Stat { cond; pending; done' = n, done_body } ->
          or_list
            [
              lazy (expr cond);
              lazy (expr pending);
              lazy (def n);
              lazy (expr done_body);
            ]
    in
    let kind = match e with Var x -> `Var x | _ -> `Generic in
    or_with_default deeper (l, t, kind)
  in
  expr program

let type_at loc program =
  match tightest_node_at loc program with
  | Some (l, t, _) when l = loc -> Some t
  | _ -> None

let print_type symbols = function `Ty t -> Ast.string_of_ty symbols t

let hover_info symbols lineco program =
  let open Printf in
  let wrap_code code = sprintf "```co_lc\n%s\n```" code in
  let gen_docs (range, t, kind) =
    let ty_str = print_type symbols t in
    let prefix =
      match kind with
      | `Var x -> sprintf "(var) %s: " (Symbol.string_of symbols x)
      | `Def x -> sprintf "(decl) %s: " (Symbol.string_of symbols x)
      | `Generic -> ""
    in
    let ty_doc = prefix ^ ty_str in
    let md_docs = [ wrap_code ty_doc ] in
    { range; md_docs }
  in
  let node = tightest_node_at (lineco, lineco) program in
  Option.map gen_docs node
