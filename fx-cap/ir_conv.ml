(** Translation of the source language to capability-passing and CPS STLC *)

type ctx = { fresh_name : string -> string }

let new_ctx () =
  let fresh_name =
    let taken = ref [] in
    fun hint ->
      let rec find i =
        let cand = hint ^ string_of_int i in
        if List.exists (Base.equal_string cand) !taken then find (i + 1)
        else cand
      in
      let name = find 1 in
      taken := name :: !taken;
      name
  in
  { fresh_name }

let unlink_content t =
  match !(Ast.unlink t) with
  | Content c -> c
  | Link _ | Unbd _ -> failwith "unresolved variable after solving"

(** T[t] *)
let rec conv_ty : Ast.ty -> Ir.ty =
 fun t ->
  let content = unlink_content t in
  match content with
  | Ast.TBool -> Ir.TBool
  | Ast.TInt -> Ir.TInt
  | Ast.TFnFx (t1, t2, `Stk stkshp) ->
      let t1 = conv_ty t1 in
      let t2 = conv_fx_ty t2 stkshp in
      TFn (t1, t2)

(** C[t] translates types are produced via effectful computations to a CPS
        shape. *)
and conv_fx_ty t stack_shape =
  let t = conv_ty t in
  match stack_shape with
  | [] -> (* not effectful *) t
  | inner_fx_t :: outer_fx_tys ->
      (* C[t']_[t1, ..trest] = (T[t] -> C[t1]_trest) -> C[t1]_trest *)
      let ret_fx_ty = conv_fx_ty inner_fx_t outer_fx_tys in
      Ir.TFn (Ir.TFn (t, ret_fx_ty), ret_fx_ty)

let rec conv_expr : ctx -> Ast.e_expr -> Ir.e_expr =
 fun ctx (_, t, e) ->
  let ir_t = conv_ty t in
  let ir_e =
    match e with
    | Ast.Lit lit -> Ir.Lit lit
    | Ast.Var x -> Ir.Var x
    | Ast.Abs ((_, t_x, x), body) ->
        let stkshp =
          match unlink_content t with
          | Ast.TFnFx (_, _, stkshp) -> stkshp
          | _ -> failwith "abstraction not a function type after solving"
        in
        let t_x = conv_ty t_x in
        let body = conv_stmt ctx body stkshp in
        Ir.Abs ((t_x, x), body)
  in
  (ir_t, ir_e)

and conv_stmt : ctx -> Ast.e_stmt -> Ast.stack_shape -> Ir.e_expr =
 fun ctx (_, t, s) (`Stk stkshp) ->
  let ir_t = conv_ty t in
  let ir_e =
    match (s, stkshp) with
    | Ast.App (e1, e2), _ -> Ir.App (conv_expr ctx e1, conv_expr ctx e2)
    | Ast.Let ((_, t_x, x), e, s), [] ->
        let t_x = conv_ty t_x in
        let e = conv_stmt ctx e (`Stk []) in
        let s = conv_stmt ctx s (`Stk []) in
        Ir.Let ((t_x, x), e, s)
    | Ast.Let ((_, t_x, x), e, s), stack_shape ->
        (* [let x = e in s] at [t, t_rest] stack
           => \k -> S[e]_[t, t_rest] @ (\x -> (S[s]_[t, t_rest] @ k)) *)
        let stk = `Stk stack_shape in
        let k_var = ctx.fresh_name "k" in
        let cps_x_def = conv_stmt ctx e stk (* S[e]_[t, t_rest] *) in
        let cps_x_after = conv_stmt ctx s stk (* S[s]_[t, t_rest] *) in
        let t_k, cps_after_result =
          match fst cps_x_after with
          | Ir.TFn (wanted_k, tret) -> (wanted_k, tret)
          | _ -> failwith "translated body after let not a continuation!"
        in
        let k : Ir.e_expr = (t_k, Ir.Var k_var) in
        let x : Ir.e_str = (conv_ty t_x, x) in
        (* (S[s]_[t, t_rest] @ k) *)
        let app_after_with_cont =
          let body = Ir.App (cps_x_after, k) in
          (cps_after_result, body)
        in
        (* \x -> (S[s]_[t, t_rest] @ k) *)
        let cont_after =
          let body = Ir.Abs (x, app_after_with_cont) in
          let t = Ir.TFn (fst x, fst app_after_with_cont) in
          (t, body)
        in
        (* S[e]_[t, t_rest] @ (\x -> (S[s]_[t, t_rest] @ k)) *)
        let app_after_cont_to_def =
          let body = Ir.App (cps_x_def, cont_after) in
          let t =
            match fst cps_x_def with
            | Ir.TFn (_, tret) -> tret
            | _ -> failwith "translated let definition not a continuation"
          in
          (t, body)
        in
        (* \k -> S[e]_[t, t_rest] @ (\x -> (S[s]_[t, t_rest] @ k)) *)
        Ir.Abs ((t_k, k_var), app_after_cont_to_def)
    | Ast.Return e, [] -> snd (conv_expr ctx e)
    | Ast.Return e, _stk ->
        (* [return e] at [t, t_rest] stack
           => \k -> k (E[e])
           throw behind one layer of CPS
        *)
        let k_var = ctx.fresh_name "k" in
        let e = conv_expr ctx e in
        let t_k =
          match ir_t with
          | Ir.TFn (t_k, _) -> t_k
          | _ -> failwith "translated effectful return is not a continuation"
        in
        Ir.Abs ((t_k, k_var), e)
  in
  (ir_t, ir_e)

let conv_program : Ast.program -> Ir.e_expr =
 fun p ->
  let ctx = new_ctx () in
  (* top-levels start off with the empty stack shape *)
  let top_stack = `Stk [] in
  conv_stmt ctx p top_stack
