open Ast

exception Solve_err of string

let merge a b content =
  a := Content content;
  b := Link a

let failsolve s = raise (Solve_err s)

let occurs x =
  let rec go t =
    match !t with
    | Unbd n -> n = x
    | Link t -> go t
    | Content TBool -> false
    | Content (TFnFx (in', out, `Stk stkshp)) ->
        go in' || go out || List.exists go stkshp
  in
  go

let unify t1 t2 =
  let error prefix =
    failsolve
      ("Unify error: " ^ prefix ^ " at "
      ^ Ty_print.string_of_ty Util.default_width t1
      ^ " ~ "
      ^ Ty_print.string_of_ty Util.default_width t2)
  in
  let rec unify t1 t2 =
    let t1, t2 = (unlink t1, unlink t2) in
    if t1 != t2 then
      match (!t1, !t2) with
      | Link _, _ | _, Link _ -> failwith "found a link where none was expected"
      | Unbd n, _ -> if occurs n t2 then error "occurs" else t1 := Link t2
      | _, Unbd n -> if occurs n t1 then error "occurs" else t2 := Link t1
      | Content c1, Content c2 -> (
          match (c1, c2) with
          | TBool, TBool -> ()
          | TFnFx (in1, out1, `Stk stkshp1), TFnFx (in2, out2, `Stk stkshp2) ->
              unify in1 in2;
              unify out1 out2;
              (* TODO unify stack shapes *)
              let _ = (stkshp1, stkshp2) in
              ()
          | _ -> error "incompatible")
  in
  unify t1 t2

let infer_expr =
  let rec infer venv (_, t, e) =
    let ity =
      match e with
      | Lit (`Bool _) -> ref @@ Content TBool
      | Var x -> (
          match List.assoc_opt x venv with
          | Some t -> t
          | None -> failsolve ("Variable " ^ x ^ " not in scope"))
      | Abs ((_, t_x, x), e) ->
          let t_res = infer ((x, t_x) :: venv) e in
          let stkshp = [] (* TODO stack shapes!!! *) in
          ref @@ Content (TFnFx (t_x, t_res, `Stk stkshp))
    in
    unify t ity;
    t
  in
  infer

let infer_stmt fv =
  let rec infer venv (_, t, s) =
    let ity =
      match s with
      | App (e1, e2) ->
          let t_fn = infer_expr venv e1 in
          let t_arg = infer_expr venv e2 in
          let t_ret = fv () in
          let stkshp = [] (* TODO stack shapes!!! *) in
          unify t_fn (ref @@ Content (TFnFx (t_arg, t_ret, `Stk stkshp)));
          t_ret
      | Let ((_, t_x, x), e, b) ->
          let t_x' = infer venv e in
          unify t_x t_x';
          infer ((x, t_x) :: venv) b
      | Return e -> infer_expr venv e
    in
    unify t ity;
    t
  in
  infer

let infer_program fresh_var program =
  try
    let _var = infer_stmt fresh_var [] program in
    Result.ok program
  with Solve_err s -> Error s