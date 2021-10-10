open Language
open Builtin

let within line col
    { start = start_line, start_col, _; fin = fin_line, fin_col, _ } =
  line >= start_line && line <= fin_line && col >= start_col && col < fin_col

let tighest_node_at line col e =
  let collected = ref [] in
  let rec got (Ty (t, pos) as ty) =
    if within line col pos then (
      collected := `Ty ty :: !collected;
      match t with
      | TUnknown | TNat | TBool | TInfer _ -> ()
      | TArrow (t1, t2) ->
          got t1;
          got t2
      | TRef t1 -> got t1)
  in
  let rec goe (Elab (e, pos, _) as node) =
    if within line col pos then collected := `Expr node :: !collected;
    (* We must always descend because our AST is non-linear with the surface
       syntax (e.g. "let x = v in b" gets translated as "(\x. b) v"). *)
    match e with
    | Nat _ | Bool _ | Var _ -> ()
    | App (e1, e2, _) | RefAssign (e1, e2) ->
        goe e1;
        goe e2
    | Lam ((x, xi), t, e) ->
        goe (Elab (Var (`Local x), xi, t));
        got t;
        goe e
    | If (e1, e2, e3) ->
        goe e1;
        goe e2;
        goe e3
    | Ref e | Deref e -> goe e
  in
  goe e;
  let getl = function `Expr (Elab (_, i, _)) -> i | `Ty (Ty (_, i)) -> i in
  let cmp n1 n2 =
    let { start = _, _, s1; fin = _, _, e1 } = getl n1 in
    let { start = _, _, s2; fin = _, _, e2 } = getl n2 in
    compare (e1 - s1) (e2 - s2)
  in
  let sorted =
    !collected |> List.sort cmp
    |> List.filter (function `Expr _ -> true | `Ty _ -> false)
  in
  List.nth_opt sorted 0

let get_doc_for_global global =
  (List.filter (fun { name; _ } -> name = global) builtins |> List.hd).doc

let get_doc_at line col e =
  let resolver =
    let open Printf in
    let wrap_code code = Printf.sprintf "```gtlc\n%s\n```" code in
    function
    | `Expr (Elab (Var (`Local x), i, t)) ->
        let code = wrap_code (sprintf "(local) %s: %s" x (string_of_ty t)) in
        ([ code ], i)
    | `Expr (Elab (Var (`Global x), i, t)) ->
        let code = wrap_code (sprintf "(global) %s: %s" x (string_of_ty t)) in
        let doc = get_doc_for_global x in
        ([ code; doc ], i)
    | `Expr (Elab (_, i, t)) ->
        let code = wrap_code (string_of_ty t) in
        ([ code ], i)
    | `Ty (Ty (_, i) as t) ->
        let code = wrap_code (string_of_ty t) in
        ([ code ], i)
  in
  Option.map resolver (tighest_node_at line col e)
