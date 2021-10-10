module T = Typecheck
open Cast_ir
open Cast_ir.Expr
open Cast_ir.Builtin
open Util

type error = CastError of elaborated_expr | TypeError of elaborated_expr

let type_error e = Error (TypeError e)

let cast_error e = Error (CastError e)

let rec unbox = function
  | Elab (Cast (Ty (TUnknown, _), e), _) -> unbox e
  | e -> e

let ( >>= ) = Result.bind

let subst x (Elab (v, _) as velab) =
  let rec go subs (Elab (e, t)) =
    let e' =
      match e with
      | Nat _ | Bool _ | Loc _ | Builtin _ -> e
      | Var (`Local y) -> (
          match List.assoc_opt y subs with Some e' -> e' | None -> e)
      | Var (`Global _) -> e
      | App (e1, e2) -> App (go subs e1, go subs e2)
      | If (c, t, e) -> If (go subs c, go subs t, go subs e)
      | Ref e -> Ref (go subs e)
      | Deref e -> Deref (go subs e)
      | RefAssign (e1, e2) -> RefAssign (go subs e1, go subs e2)
      | Cast (t', e') -> Cast (t', go subs e')
      | Lam (y, t', e') ->
          if x = y then e
          else
            let y' = freshen y (S.union (freevars velab) (freevars e')) in
            Lam (y', t', go ((y, Var (`Local y')) :: subs) e')
    in
    Elab (e', t)
  in
  go [ (x, v) ]

let bad_arg () = failwith "bad_arg"

class add_captured n : builtin =
  object
    method name = Printf.sprintf "__builtin_add_captured(%d)" n

    method eval =
      function
      | Elab (Nat m, _) -> Elab (Nat (n + m), ft TNat) | _ -> bad_arg ()
  end

class mult_captured n : builtin =
  object
    method name = Printf.sprintf "__builtin_mult_captured(%d)" n

    method eval =
      function
      | Elab (Nat m, _) -> Elab (Nat (n * m), ft TNat) | _ -> bad_arg ()
  end

class eqn_captured n : builtin =
  object
    method name = Printf.sprintf "__builtin_eqn_captured(%d)" n

    method eval =
      function
      | Elab (Nat m, _) -> Elab (Bool (n = m), ft TBool) | _ -> bad_arg ()
  end

class eqb_captured b : builtin =
  object
    method name = Printf.sprintf "__builtin_eqb_captured(%b)" b

    method eval =
      function
      | Elab (Bool c, _) -> Elab (Bool (b = c), ft TBool) | _ -> bad_arg ()
  end

let builtin_eval = function
  | "succ" -> (
      function Nat n -> Elab (Nat (n + 1), ft TNat) | _ -> bad_arg ())
  | "pred" -> (
      function Nat n -> Elab (Nat (n - 1), ft TNat) | _ -> bad_arg ())
  | "add" -> (
      function
      | Nat n ->
          Elab (Builtin (new add_captured n), ft (TArrow (ft TNat, ft TNat)))
      | _ -> bad_arg ())
  | "mult" -> (
      function
      | Nat n ->
          Elab (Builtin (new mult_captured n), ft (TArrow (ft TNat, ft TNat)))
      | _ -> bad_arg ())
  | "eqn" -> (
      function
      | Nat n ->
          Elab (Builtin (new eqn_captured n), ft (TArrow (ft TNat, ft TBool)))
      | _ -> bad_arg ())
  | "eqb" -> (
      function
      | Bool b ->
          Elab (Builtin (new eqb_captured b), ft (TArrow (ft TBool, ft TBool)))
      | _ -> bad_arg ())
  | _ -> bad_arg ()

let eval e =
  let freshname = fresh_generator () in
  let freshloc =
    let i = ref 0 in
    fun () ->
      incr i;
      !i
  in
  let heap = Hashtbl.create 32 in
  let rec eval (Elab (e, te) as input) : (elaborated_expr, error) result =
    match e with
    (* ELam *)
    | Lam (s, t, e) -> Ok (Elab (Lam (s, t, e), te))
    (* EApp *)
    | App (e1, e2) -> (
        eval e1 >>= fun (Elab (e1, _)) ->
        match e1 with
        | Lam (x, _, e3) ->
            eval e2 >>= fun e2 ->
            let e3' = subst x e2 e3 in
            eval e3'
        (* EDelta *)
        | Var (`Global fn) ->
            eval e2 >>= fun (Elab (e2, _)) -> Ok (builtin_eval fn e2)
        | Builtin builtin -> eval e2 >>= fun e2 -> Ok (builtin#eval e2)
        | _ -> type_error input)
    (* EIf (new) *)
    | If (c, thn, els) -> (
        eval c >>= fun (Elab (c, _)) ->
        match c with
        | Bool true -> eval thn
        | Bool false -> eval els
        | _ -> type_error input)
    (* EConst *)
    | Nat n -> Ok (Elab (Nat n, ft TNat))
    | Bool b -> Ok (Elab (Bool b, ft TBool))
    (* ECstG *)
    | Cast (Ty (TNat, _), e) -> (
        eval e >>= fun v ->
        let uv = unbox v in
        match uv with Elab (_, Ty (TNat, _)) -> Ok uv | _ -> cast_error input)
    | Cast (Ty (TBool, _), e) -> (
        eval e >>= fun v ->
        let uv = unbox v in
        match uv with Elab (_, Ty (TBool, _)) -> Ok uv | _ -> cast_error input)
    (* ECstF *)
    | Cast (Ty (TArrow (s, s'), _), e) -> (
        eval e >>= fun v ->
        let uv = unbox v in
        match uv with
        | Elab (_, Ty (TArrow (t, t'), _))
          when T.consistent (ft (TArrow (s, s'))) (ft (TArrow (t, t'))) ->
            (* λ z:σ. (⟨σ′⟩ (unbox v (⟨τ⟩ z))) *)
            let z = freshname "z" in
            let inner_app =
              Elab (App (uv, Elab (Cast (t, Elab (Var (`Local z), s)), t)), t')
            in
            let body = Elab (Cast (s', inner_app), s') in
            let res = Lam (z, s, body) in
            Ok (Elab (res, ft (TArrow (s, s'))))
        | _ -> cast_error input)
    (* ECstU *)
    | Cast (Ty (TUnknown, _), e) ->
        eval e >>= fun v -> Ok (Elab (Cast (ft TUnknown, unbox v), ft TUnknown))
    (* ECstR *)
    | Cast (Ty (TRef t, _), e) -> (
        eval e >>= fun v ->
        match unbox v with
        | Elab (_, Ty (TRef t', _)) when t = t' -> Ok v
        | _ -> cast_error input)
    (* ERef *)
    | Ref e ->
        eval e >>= fun v ->
        let l = freshloc () in
        Hashtbl.add heap l v;
        Ok (Elab (Loc l, ft (TRef te)))
    (* EDeref *)
    | Deref e -> (
        eval e >>= function
        | Elab (Loc l, _) -> Ok (Hashtbl.find heap l)
        | _ -> type_error input)
    (* EAssign *)
    | RefAssign (e1, e2) -> (
        eval e1 >>= function
        | Elab (Loc l, t) ->
            eval e2 >>= fun e2 ->
            Hashtbl.add heap l e2;
            Ok (Elab (Loc l, t))
        | _ -> type_error input)
    | Var _ | Loc _ | Builtin _ -> Ok input
    | Cast (Ty (TInfer _, _), _) ->
        failwith "unreachable: inference type variable is unresolved"
  in
  eval e |> Result.map unbox
