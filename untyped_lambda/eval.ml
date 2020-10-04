(** Evaluates an expression in a [Context.context] environment. *)

let lookup k env =
  match List.nth env k with
  | Some e -> Some (Grammar.shift (k + 1) e)
  | None -> None

let extend env = None :: env

let eval ?(eager = false) ?(deep = false) =
  let rec eval env ({ Zoo.data = e'; loc } as e) =
    match e' with
    | Grammar.Var k -> (
        match lookup k env with None -> e | Some e -> eval env e )
    | Grammar.Subst (s, e') -> eval env (Grammar.subst s e')
    | Grammar.Lambda (x, e') ->
        if deep then
          let e' = eval (extend env) e' in
          Grammar.mk_lambda x e'
        else e
    | Grammar.App (e1, e2) -> (
        let e2 = if eager then eval env e2 else e2 in
        let ({ Zoo.data = e1'; _ } as e1) = eval env e1 in
        match e1' with
        | Grammar.Lambda (_, e) ->
            eval env (Grammar.mk_subst (Grammar.Dot (e2, Grammar.id_subst)) e)
        | Grammar.Var _ | Grammar.App _ ->
            Zoo.locate ~loc (Grammar.App (e1, e2))
        | Grammar.Subst _ -> Zoo.error ~loc:e1.Zoo.loc "expected a function" )
  in
  eval
