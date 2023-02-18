open Ast

exception Solve_err of string

let failsolve s = raise (Solve_err s)

module SymbolSet = struct
  include Set.Make (struct
    type t = Symbol.symbol

    let compare = compare
  end)

  let concat ls = List.fold_left union empty ls
end

module SymbolMap = struct
  include Map.Make (struct
    type t = Symbol.symbol

    let compare = compare
  end)
end

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
        | TFn (t1, _, t2) -> go t1 || go t2
        | TFiber t -> go t)
  in
  go

let unify symbols a b =
  let error prefix =
    failsolve
      ("Unify error: " ^ prefix ^ " at " ^ string_of_ty symbols a ^ " ~ "
     ^ string_of_ty symbols b)
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
          | TFn (t11, ls1, t12), TFn (t21, ls2, t22) ->
              let ls = unify_lambda_set ls1 ls2 in
              unify t11 t21;
              unify t12 t22;
              merge a b (TFn (t11, ls, t12))
          | TFiber t1, TFiber t2 -> unify t1 t2
          | _ -> error "incompatible")
  and unify_lambda_set ls1 ls2 =
    let set1 = SymbolMap.of_seq @@ List.to_seq ls1 in
    let set2 = SymbolMap.of_seq @@ List.to_seq ls2 in
    let combine _ caps1 (caps2 : Ast.captures) =
      assert (caps1 = caps2);
      Some caps1
    in
    let set = SymbolMap.union combine set1 set2 in
    List.of_seq @@ SymbolMap.to_seq set
  in
  unify a b

module T = struct
  let int = ref @@ Content TInt
  let bool = ref @@ Content TBool
  let unit = ref @@ Content (TTup [])
end

let drop_immaterial_captures lst =
  List.filter
    (fun (_, t) ->
      match !(unlink t) with
      | Content (TFn (_, ls, _)) ->
          let has_any_captures =
            List.exists (fun (_, captures) -> List.length captures > 0) ls
          in
          has_any_captures
      | _ -> true)
    lst

let infer symbols fresh_var =
  let unify = unify symbols in
  let union = SymbolSet.union in
  let rec infer ?(rec_name = None) venv (_, t, e) : ty * SymbolSet.t =
    let infer ?(rec_name = rec_name) = infer ~rec_name in

    let ty, free =
      match e with
      | Var x -> (
          match List.assoc_opt x venv with
          | Some t ->
              let free =
                match rec_name with
                | Some _ -> SymbolSet.empty
                | None -> SymbolSet.singleton x
              in
              (t, free)
          | None ->
              failsolve
                ("Variable " ^ Symbol.string_of symbols x ^ " not in scope"))
      | Lit l ->
          let t = match l with `Bool _ -> T.bool | `Int _ -> T.int in
          (t, SymbolSet.empty)
      | Tup es ->
          let ts, frees = List.split @@ List.map (infer venv) es in
          (ref @@ Content (TTup ts), SymbolSet.concat frees)
      | Let (kind, (_, t_x, x), e, b) ->
          let t_x', free_x =
            let venv, rec_name =
              match kind with
              | `Rec -> ((x, t_x) :: venv, Some x)
              | _ -> (venv, None)
            in
            infer ~rec_name venv e
          in
          unify t_x t_x';
          if kind == `Unit then unify t_x T.unit;
          let t_b, free_b = infer ((x, t_x) :: venv) b in

          let free_x =
            if kind = `Rec then SymbolSet.remove x free_x else free_x
          in
          let free_b = SymbolSet.remove x free_b in
          (t_b, SymbolSet.union free_x free_b)
      | Abs (lam, (_, t_x, x), e) ->
          let t_res, free_e = infer ((x, t_x) :: venv) e in
          let free = SymbolSet.remove x free_e in

          let free_xs = List.of_seq @@ SymbolSet.to_seq free in
          let free_tys = List.map (fun x -> List.assoc x venv) free_xs in
          let captures =
            drop_immaterial_captures @@ List.combine free_xs free_tys
          in
          let lambda_set = [ (lam, captures) ] in

          let t_fn = ref @@ Content (TFn (t_x, lambda_set, t_res)) in
          (t_fn, free)
      | App (e1, e2) ->
          let t_fn, free_fn = infer venv e1 in
          let t_arg, free_arg = infer venv e2 in
          let t_ret = fresh_var () in
          unify t_fn (ref @@ Content (TFn (t_arg, [], t_ret)));
          (t_ret, union free_fn free_arg)
      | Binop (b, e1, e2) ->
          let t1, free1 = infer venv e1 in
          let t2, free2 = infer venv e2 in
          let t_arg1, t_arg2, t_ret =
            match b with
            | `Lt | `Eq -> (T.int, T.int, T.bool)
            | `Add | `Sub | `Mul -> (T.int, T.int, T.int)
          in
          unify t1 t_arg1;
          unify t2 t_arg2;
          (t_ret, union free1 free2)
      | If (c, e1, e2) ->
          let t_c, freec = infer venv c in
          unify t_c T.bool;
          let t_e1, free1 = infer venv e1 in
          let t_e2, free2 = infer venv e2 in
          unify t_e1 t_e2;
          (t_e1, union freec @@ union free1 free2)
      | Access (tup, idx) ->
          let t_tup, free = infer venv tup in
          let t_idx = fresh_var () in
          unify t_tup (ref @@ Content (TTupSparse (IntMap.singleton idx t_idx)));
          (t_idx, free)
      | Spawn e ->
          let t, free = infer venv e in
          (ref @@ Content (TFiber t), free)
      | Yield -> (T.unit, SymbolSet.empty)
      | Resume e ->
          let t, free = infer venv e in
          unify t (ref @@ Content (TFiber (fresh_var ())));
          (t, free)
      | Stat { cond; pending; done' = (_, t_x, x), e_done } ->
          let t_cond, free_cond = infer venv cond in
          let t_fiber_ret = fresh_var () in
          unify t_cond (ref @@ Content (TFiber t_fiber_ret));
          let t_pending, free_pending = infer venv pending in
          let t_done, free_done =
            unify t_x t_fiber_ret;
            infer ((x, t_x) :: venv) e_done
          in
          let free_done = SymbolSet.remove x free_done in
          unify t_pending t_done;
          (t_pending, union free_cond @@ union free_pending free_done)
    in
    unify t ty;
    (ty, free)
  in
  infer

let infer_program fresh_var symbols program =
  try
    let _var, free = infer symbols fresh_var [] program in
    assert (SymbolSet.is_empty free);
    Result.ok (symbols, program)
  with Solve_err s -> Error s
