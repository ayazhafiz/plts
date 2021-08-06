type kind = Star | Box

type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * ty * expr
  | Pi of string * ty * expr (* dependent arrow type *)
  | Kind of kind

and ty = expr

let app f a = App (f, a)

let rec string_of_kind = function Star -> "*" | Box -> "[]"

let rec string_of_t = function
  | Var x -> x
  | App (f, a) -> Printf.sprintf "(%s %s)" (string_of_t f) (string_of_t a)
  | Lam (s, t, e) ->
      Printf.sprintf "(λ%s:%s. %s)" s (string_of_t t) (string_of_t e)
  | Pi (s, t, e) ->
      Printf.sprintf "(Π%s:%s. %s)" s (string_of_t t) (string_of_t e)
  | Kind k -> string_of_kind k

module S = Set.Make (struct
  type t = string

  let compare = compare
end)

let rec fv = function
  | Var x -> S.singleton x
  | App (e1, e2) -> S.union (fv e1) (fv e2)
  | Lam (x, t, e) | Pi (x, t, e) -> S.(union (fv t) (fv e |> remove x))
  | Kind _ -> S.empty

let rec freshen x used = if S.mem x used then freshen (x ^ "'") used else x

let subst v x e =
  let rec go subs expr =
    match expr with
    | Var x -> ( match List.assoc_opt x subs with Some r -> r | None -> expr)
    | App (e1, e2) -> App (go subs e1, go subs e2)
    | Lam (s, t, b) | Pi (s, t, b) ->
        if s = v then expr
        else if S.mem s (fv x) then
          let s' = freshen s (S.union (fv x) (fv b)) in
          Lam (s', go subs t, go ((s, Var s') :: subs) b)
        else Lam (s, go subs t, go subs b)
    | Kind _ -> expr
  in
  go [ (v, x) ] e

let alpha_eq e1 e2 =
  let fresh =
    let next = ref 0 in
    fun () ->
      incr next;
      !next
  in
  let rec cmp n1 n2 e1 e2 =
    match (e1, e2) with
    | Var x, Var y -> (
        match (List.assoc_opt x n1, List.assoc_opt y n2) with
        | Some n, Some m -> n = m
        | None, None -> x = y
        | _ -> false)
    | App (e11, e12), App (e21, e22) -> cmp n1 n2 e11 e21 && cmp n1 n2 e12 e22
    | Lam (s1, t1, b1), Lam (s2, t2, b2) | Pi (s1, t1, b1), Pi (s2, t2, b2) ->
        let s = fresh () in
        cmp n1 n2 t1 t2 && cmp ((s1, s) :: n1) ((s2, s) :: n2) b1 b2
    | Kind k1, Kind k2 -> k1 = k2
    | _ -> false
  in
  cmp [] [] e1 e2

let rec nf e =
  let app f rest = List.fold_left app f (List.map nf rest) in
  let rec spine = function
    | App (f, a), sp -> spine (f, a :: sp)
    | Lam (s, _, e), a :: sp -> spine (subst s a e, sp)
    | Lam (s, t, e), [] -> Lam (s, nf t, nf e)
    | Pi (s, t, e), sp -> app (Pi (s, nf t, nf e)) sp
    | f, sp -> app f sp
  in
  spine (e, [])

let abeq e1 e2 = alpha_eq (nf e1) (nf e2)

let ( >>= ) = Option.bind

let rec tyck ctx = function
  | Var x -> List.assoc_opt x ctx
  | App (f, a) -> (
      tyck ctx f >>= function
      | Pi (x, t1, t2) ->
          tyck ctx a >>= fun ta ->
          if abeq ta t1 then Some (subst x a t2) else None
      | _ -> None)
  | Lam (s, t, b) ->
      tyck ctx t >>= fun _t_well_formed ->
      tyck ((s, t) :: ctx) b >>= fun tb ->
      let rt = Pi (s, t, tb) in
      tyck ctx rt >>= fun _rt_well_formed -> Some rt
  | Pi (s, t, b) -> (
      tyck_nf ctx t >>= fun k_t ->
      tyck_nf ((s, t) :: ctx) b >>= fun k_tb ->
      match (k_t, k_tb) with Kind _, Kind _ -> Some k_tb | _ -> None)
  | Kind Star -> Some (Kind Box)
  | Kind Box -> None

and tyck_nf ctx e = tyck ctx e >>= fun e -> Some (nf e)
