open Ir

let rec check venv (t, e) =
  let t_e =
    match e with
    | Var x -> List.assoc x venv
    | Builtin b -> Ir_conv.conv_ty @@ Ast.ty_of_builtin b
    | Lit (`Bool _) -> TBool
    | Lit (`Int _) -> TInt
    | Let (`Rec recursive, (t_x, x), e, r) ->
        let venv' = if recursive then (x, t_x) :: venv else venv in
        let t_x' = check venv' e in
        assert (t_x = t_x');
        check ((x, t_x) :: venv) r
    | Abs ((t_x, x), b) ->
        let t_b = check ((x, t_x) :: venv) b in
        TFn (t_x, t_b)
    | App (e1, e2) -> (
        let t_e1 = check venv e1 in
        let t_e2 = check venv e2 in
        match t_e1 with
        | TFn (t1, t2) ->
            assert (t1 = t_e2);
            t2
        | _ -> failwith "applying to a non-function")
    | If (c, e1, e2) ->
        let t_c = check venv c in
        let t_e1 = check venv e1 in
        let t_e2 = check venv e2 in
        assert (t_c = TBool);
        assert (t_e1 = t_e2);
        t_e1
  in
  assert (t = t_e);
  t

let check_program program =
  let _ = check [] program in
  ()
