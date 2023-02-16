open Ast

exception Solve_err of string

let failsolve s = raise (Solve_err s)

let occurs x =
  let rec go t =
    match !t with
    | Unbd n -> n = x
    | Link t -> go t
    | Content c -> (
        match c with
        | TInt | TBool -> false
        | TTup tys -> List.exists go tys
        | TTupSparse tys -> IntMap.exists (fun _ t -> go t) tys
        | TFn (t1, t2) -> go t1 || go t2
        | TFiber t -> go t)
  in
  go

let unify a b =
  let error prefix =
    failsolve
      ("Unify error: " ^ prefix ^ " at " ^ string_of_ty a ^ " ~ "
     ^ string_of_ty b)
  in
  let merge a b content =
    a := Content content;
    b := Link a
  in
  let rec unify a b =
    let a, b = (unlink a, unlink b) in
    if a != b then
      match (!a, !b) with
      | Link _, _ | _, Link _ -> failwith "found a link where none was expected"
      | Unbd n, _ -> if occurs n b then error "occurs" else a := Link b
      | _, Unbd n -> if occurs n a then error "occurs" else b := Link a
      | Content l_content, Content r_content -> (
          match (l_content, r_content) with
          | TInt, TInt -> ()
          | TBool, TBool -> ()
          | TTup ts1, TTup ts2 ->
              if List.length ts1 <> List.length ts2 then
                error "tuples differ in size";
              List.iter2 unify ts1 ts2
          | TTupSparse ts1, TTupSparse ts2 ->
              let combined =
                IntMap.union
                  (fun _ t1 t2 ->
                    unify t1 t2;
                    Some t1)
                  ts1 ts2
              in
              merge a b (TTupSparse combined)
          | TTup tup, TTupSparse ts | TTupSparse ts, TTup tup ->
              IntMap.iter
                (fun i t ->
                  match List.nth_opt tup i with
                  | Some t' -> unify t t'
                  | None -> error "sparse tuple does not fit in tuple")
                ts;
              merge a b (TTup tup)
          | TFn (t11, t12), TFn (t21, t22) ->
              unify t11 t21;
              unify t12 t22
          | TFiber t1, TFiber t2 -> unify t1 t2
          | _ -> error "incompatible")
  in
  unify a b

module T = struct
  let int = ref @@ Content TInt
  let bool = ref @@ Content TBool
  let unit = ref @@ Content (TTup [])
end

let infer symbols fresh_var =
  let rec infer venv (_, t, e) : ty =
    let ty =
      match e with
      | Var x -> (
          match List.assoc_opt x venv with
          | Some t -> t
          | None ->
              failsolve
                ("Variable " ^ Symbol.string_of symbols x ^ " not in scope"))
      | Lit l -> ( match l with `Bool _ -> T.bool | `Int _ -> T.int)
      | Tup es ->
          let ts = List.map (infer venv) es in
          ref @@ Content (TTup ts)
      | Let (kind, (_, t_x, x), e, b) ->
          let t_x' =
            let venv = match kind with `Rec -> (x, t_x) :: venv | _ -> venv in
            infer venv e
          in
          unify t_x t_x';
          if kind == `Unit then unify t_x T.unit;
          infer ((x, t_x) :: venv) b
      | Abs ((_, t_x, x), e) ->
          let t_res = infer ((x, t_x) :: venv) e in
          ref @@ Content (TFn (t_x, t_res))
      | App (e1, e2) ->
          let t_fn = infer venv e1 in
          let t_arg = infer venv e2 in
          let t_ret = fresh_var () in
          unify t_fn (ref @@ Content (TFn (t_arg, t_ret)));
          t_ret
      | Binop (b, e1, e2) ->
          let t1 = infer venv e1 in
          let t2 = infer venv e2 in
          let t_arg1, t_arg2, t_ret =
            match b with
            | `Lt -> (T.int, T.int, T.bool)
            | `Add | `Sub | `Mul -> (T.int, T.int, T.int)
          in
          unify t1 t_arg1;
          unify t2 t_arg2;
          t_ret
      | If (c, e1, e2) ->
          let t_c = infer venv c in
          unify t_c T.bool;
          let t_e1 = infer venv e1 in
          let t_e2 = infer venv e2 in
          unify t_e1 t_e2;
          t_e1
      | Access (tup, idx) ->
          let t_tup = infer venv tup in
          let t_idx = fresh_var () in
          unify t_tup (ref @@ Content (TTupSparse (IntMap.singleton idx t_idx)));
          t_idx
      | Spawn e ->
          let t = infer venv e in
          ref @@ Content (TFiber t)
      | Yield -> T.unit
      | Resume e ->
          let t = infer venv e in
          unify t (ref @@ Content (TFiber (fresh_var ())));
          t
      | Stat { cond; pending; done' = (_, t_x, x), e_done } ->
          let t_cond = infer venv cond in
          let t_fiber_ret = fresh_var () in
          unify t_cond (ref @@ Content (TFiber t_fiber_ret));
          let t_pending = infer venv pending in
          let t_done =
            unify t_x t_fiber_ret;
            infer ((x, t_x) :: venv) e_done
          in
          unify t_pending t_done;
          t_pending
    in
    unify t ty;
    ty
  in
  infer

let infer_program fresh_var symbols program =
  try
    let _var = infer symbols fresh_var [] program in
    Result.ok (symbols, program)
  with Solve_err s -> Error s
