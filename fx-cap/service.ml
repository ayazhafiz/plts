open Surface
open Util

let tightest_node_at loc program =
  let open Ast in
  let or_else o f = match o with Some a -> Some a | None -> f () in
  let str (l, t, x) = if within loc l then Some (l, t, `Def x) else None in
  let rec stmt (l, t, s) =
    let deeper =
      match s with
      | Let (d, s, s') ->
          or_else (str d) (fun () -> or_else (stmt s) (fun () -> stmt s'))
      | App (e1, e2) -> or_else (expr e1) (fun () -> expr e2)
      | Return e -> expr e
      | If (e1, e2, e3) ->
          or_else (expr e1) (fun () -> or_else (expr e2) (fun () -> expr e3))
    in
    or_else deeper (fun () ->
        if within loc l then
          let kind = `Generic in
          Some (l, t, kind)
        else None)
  and expr (l, t, e) =
    let deeper =
      match e with
      | Var _ -> None
      | Lit _ -> None
      | Abs ((l, t_x, x), e) ->
          if within loc l then Some (l, t_x, `Var x) else stmt e
    in
    or_else deeper (fun () ->
        if within loc l then
          let kind = match e with Var x -> `Var x | _ -> `Generic in
          Some (l, t, kind)
        else None)
  in
  stmt program

let type_at loc program =
  match tightest_node_at loc program with
  | Some (l, t, _) when l = loc -> Some t
  | _ -> None

let hover_info lineco program =
  let open Printf in
  let wrap_code code = sprintf "```asti\n%s\n```" code in
  let gen_docs (range, t, kind) =
    let ty_str = Ty_print.string_of_ty default_width t in
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
