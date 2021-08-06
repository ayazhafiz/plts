type name = Global of string | Local of int | Quote of int

type term_infer =
  | Annot of term_check * term_check
  | Star
  | Pi of term_check * term_check
  | Bound of int
  | Free of name
  | App of term_infer * term_check
  | Nat
  | NatElim of term_check * term_check * term_check * term_check

and term_check =
  | Infer of term_infer
  | Lam of term_check
  | Zero
  | Succ of term_check

let string_of_term t =
  let rec goi = function
    | Annot (t, ty) -> Printf.sprintf "(%s :: %s)" (goc t) (goc ty)
    | Star -> "*"
    | Pi (d, b) -> Printf.sprintf "(Π: %s. %s)" (goc d) (goc b)
    | Bound i -> string_of_int i
    | Free (Global n) -> n
    | Free (Local i) -> string_of_int i
    | Free (Quote i) -> string_of_int i
    | App (ti, tc) -> Printf.sprintf "(%s %s)" (goi ti) (goc tc)
    | Nat -> "Nat"
    | NatElim (m, mz, ms, n) ->
        Printf.sprintf "(natElim %s %s %s %s)" (goc m) (goc mz) (goc ms) (goc n)
  and goc = function
    | Infer ti -> goi ti
    | Lam tc -> Printf.sprintf "(λ. %s)" (goc tc)
    | Zero -> "0"
    | Succ n -> Printf.sprintf "(S %s)" (goc n)
  in
  goc t

type value =
  | VLam of (value -> value)
  | VStar
  | VPi of value * (value -> value)
  | VNeutral of neutral
  | VNat
  | VZero
  | VSucc of value

and neutral =
  | NFree of name
  | NApp of neutral * value
  | NNatElim of value * value * value * neutral

let vfree n = VNeutral (NFree n)

let vapp f v =
  match f with
  | VLam f -> f v
  | VNeutral n -> VNeutral (NApp (n, v))
  | _ -> failwith "application not applicable"

(* eval *)

let rec eval_infer env = function
  | Annot (e, _) -> eval_check env e
  | Star -> VStar
  | Pi (dom, b) -> VPi (eval_check env dom, fun x -> eval_check (x :: env) b)
  | Bound i -> List.nth env i
  | Free x -> vfree x
  | App (e1, e2) -> vapp (eval_infer env e1) (eval_check env e2)
  | Nat -> VNat
  | NatElim (m, mz, ms, n) ->
      let mz = eval_check env mz in
      let ms = eval_check env ms in
      let rec solve = function
        | VZero -> mz
        | VSucc n1 -> vapp (vapp ms n1) (solve n1)
        | VNeutral n -> VNeutral (NNatElim (eval_check env m, mz, ms, n))
        | _ -> failwith "invalid value in nat eliminator"
      in
      solve (eval_check env n)

and eval_check env = function
  | Infer it -> eval_infer env it
  | Lam b -> VLam (fun x -> eval_check (x :: env) b)
  | Zero -> VZero
  | Succ n -> VSucc (eval_check env n)

(* quote *)

let bindfree i = function Quote k -> Bound (i - k - 1) | x -> Free x

let rec quote i = function
  | VLam f -> Lam (quote (i + 1) (f (vfree (Quote i))))
  | VStar -> Infer Star
  | VPi (d, b) -> Infer (Pi (quote i d, quote (i + 1) (b (vfree (Quote i)))))
  | VNeutral n -> Infer (quote_neutral i n)
  | VNat -> Infer Nat
  | VZero -> Zero
  | VSucc k -> Succ (quote i k)

and quote_neutral i = function
  | NFree x -> bindfree i x
  | NApp (n, v) -> App (quote_neutral i n, quote i v)
  | NNatElim (m, ms, mz, n) ->
      NatElim (quote i m, quote i ms, quote i mz, Infer (quote_neutral i n))

let vquote = quote 0

(* typecheck *)

type ty = ETy of value

type context = (name * ty) list

let ( >>= ) = Result.bind

let rec subst_infer o n tm =
  let rec go = function
    | Annot (e, t) -> Annot (subst_check o n e, t)
    | Star -> Star
    | Pi (d, b) -> Pi (subst_check o n d, subst_check (o + 1) n b)
    | Bound x -> if o = x then n else Bound x
    | Free n -> Free n
    | App (e1, e2) -> App (go e1, subst_check o n e2)
    | Nat -> Nat
    | NatElim (m, mz, ms, k) ->
        NatElim
          ( subst_check o n m,
            subst_check o n mz,
            subst_check o n ms,
            subst_check o n k )
  in
  go tm

and subst_check o n = function
  | Infer it -> Infer (subst_infer o n it)
  | Lam b -> Lam (subst_check (o + 1) n b)
  | Zero -> Zero
  | Succ k -> Succ (subst_check o n k)

let rec type_infer i ctx = function
  | Annot (e, ty) ->
      type_check i ctx ty VStar >>= fun _ ->
      let ety = eval_check [] ty in
      type_check i ctx e ety >>= fun _ -> Ok ety
  | Star -> Ok VStar
  | Pi (dom, b) ->
      type_check i ctx dom VStar >>= fun _ ->
      let edom = eval_check [] dom in
      type_check (i + 1)
        ((Local i, ETy edom) :: ctx)
        (subst_check 0 (Free (Local i)) b)
        VStar
      >>= fun _ -> Ok VStar
  | Bound _ -> Error "impossible state: bound variable never expected"
  | Free x -> (
      match List.assoc_opt x ctx with
      | Some (ETy ty) -> Ok ty
      | _ -> Error "bad type for free var")
  | App (e1, e2) -> (
      type_infer i ctx e1 >>= function
      | VPi (d, b) ->
          type_check i ctx e2 d >>= fun _ -> Ok (b (eval_check [] e2))
      | _ -> Error "LHS of application must be function type")
  | Nat -> Ok VStar
  | NatElim (m, mz, ms, n) ->
      type_check i ctx m (VPi (VNat, fun _ -> VStar)) >>= fun _ ->
      let mval = eval_check [] m in
      type_check i ctx mz (vapp mval VZero) >>= fun _ ->
      type_check i ctx ms
        (VPi (VNat, fun k -> VPi (vapp mval k, fun _ -> vapp mval (VSucc k))))
      >>= fun _ ->
      type_check i ctx n VNat >>= fun _ ->
      let nval = eval_check [] n in
      Ok (vapp mval nval)

and type_check i ctx e ty =
  match (e, ty) with
  | Infer it, ety ->
      type_infer i ctx it >>= fun ity ->
      if vquote ity = vquote ety then Ok ()
      else
        Error
          (Printf.sprintf "inferred type %s does not match checked type %s"
             (string_of_term (vquote ity))
             (string_of_term (vquote ety)))
  | Lam b, VPi (t1, t2) ->
      type_check (i + 1) ((Local i, ETy t1) :: ctx)
        (subst_check 0 (Free (Local i)) b)
        (t2 (vfree (Local i)))
  | Zero, VNat -> Ok ()
  | Succ k, VNat -> type_check i ctx k VNat
  | a, b ->
      Error
        (Printf.sprintf "type mismatch: cannot check %s as %s"
           (string_of_term a)
           (string_of_term (vquote b)))

let tyck ctx tm = type_infer 0 ctx tm
