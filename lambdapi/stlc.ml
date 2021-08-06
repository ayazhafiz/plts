type name = Global of string | Local of int | Quote of int

type kind = KStar

type ty = TFree of name | TFun of ty * ty

type term_infer =
  | Annot of term_check * ty
  | Bound of int
  | Free of name
  | App of term_infer * term_check

and term_check = Infer of term_infer | Lam of term_check

type value = VLam of (value -> value) | VNeutral of neutral

and neutral = NFree of name | NApp of neutral * value

let vfree n = VNeutral (NFree n)

let vapp f v =
  match f with VLam f -> f v | VNeutral n -> VNeutral (NApp (n, v))

(* eval *)

let rec eval_infer env = function
  | Annot (e, _) -> eval_check env e
  | Bound i -> List.nth env i
  | Free x -> vfree x
  | App (e1, e2) -> vapp (eval_infer env e1) (eval_check env e2)

and eval_check env = function
  | Infer it -> eval_infer env it
  | Lam b -> VLam (fun x -> eval_check (x :: env) b)

(* typecheck *)

type info = HasKind of kind | HasType of ty

type context = (name * info) list

let ( >>= ) = Result.bind

let rec kind_check ctx ty k =
  match (ty, k) with
  | TFree x, KStar -> (
      match List.assoc_opt x ctx with
      | Some (HasKind KStar) -> Ok ()
      | _ -> Error "bad kind for free type")
  | TFun (t1, t2), KStar ->
      kind_check ctx t1 KStar >>= fun _ -> kind_check ctx t2 KStar

let rec subst_infer o n tm =
  let rec go = function
    | Annot (e, t) -> Annot (subst_check o n e, t)
    | Bound x -> if o = x then n else Bound x
    | Free n -> Free n
    | App (e1, e2) -> App (go e1, subst_check o n e2)
  in
  go tm

and subst_check o n = function
  | Infer it -> Infer (subst_infer o n it)
  | Lam b -> Lam (subst_check (o + 1) n b)

let rec type_infer i ctx = function
  | Annot (e, ty) ->
      kind_check ctx ty KStar >>= fun _ ->
      type_check i ctx e ty >>= fun _ -> Ok ty
  | Bound _ -> Error "impossible state: bound variable never expected"
  | Free x -> (
      match List.assoc_opt x ctx with
      | Some (HasType ty) -> Ok ty
      | _ -> Error "bad type for free var")
  | App (e1, e2) -> (
      type_infer i ctx e1 >>= function
      | TFun (t1, t2) -> type_check i ctx e2 t1 >>= fun _ -> Ok t2
      | _ -> Error "LHS of application must be function type")

and type_check i ctx e ty =
  match (e, ty) with
  | Infer it, ety ->
      type_infer i ctx it >>= fun ity ->
      if ity = ety then Ok ()
      else Error "inferred type does not match checked type"
  | Lam b, TFun (t1, t2) ->
      type_check (i + 1)
        ((Local i, HasType t1) :: ctx)
        (subst_check 0 (Free (Local i)) b)
        t2
  | _ -> Error "type mismatch"

let tyck ctx tm = type_infer 0 ctx tm

(* quote *)

let bindfree i = function Quote k -> Bound (i - k - 1) | x -> Free x

let rec quote i = function
  | VLam f -> Lam (quote (i + 1) (f (vfree (Quote i))))
  | VNeutral n -> Infer (quote_neutral i n)

and quote_neutral i = function
  | NFree x -> bindfree i x
  | NApp (n, v) -> App (quote_neutral i n, quote i v)

let vquote = quote 0

(* test *)

let tfree a = TFree (Global a)

let free x = Infer (Free (Global x))

let id = Lam (Infer (Bound 0))

let const = Lam (Lam (Infer (Bound 1)))

let term1 = App (Annot (id, TFun (tfree "a", tfree "a")), free "y")

let term2 =
  App
    ( App
        ( Annot
            ( const,
              TFun
                ( TFun (tfree "b", tfree "b"),
                  TFun (tfree "a", TFun (tfree "b", tfree "b")) ) ),
          id ),
      free "y" )

let ctx1 = [ (Global "y", HasType (tfree "a")); (Global "a", HasKind KStar) ]

let ctx2 = ctx1 @ [ (Global "b", HasKind KStar) ]

let%test _ = eval_infer [] term1 |> vquote = Infer (Free (Global "y"))

let%test _ = eval_infer [] term2 |> vquote = id

let%test _ = tyck ctx1 term1 = Ok (TFree (Global "a"))

let%test _ =
  tyck ctx2 term2 = Ok (TFun (TFree (Global "b"), TFree (Global "b")))
