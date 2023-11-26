type expr = Var of string | App of expr * expr | Lam of string * expr

let app e1 e2 = App (e1, e2)

let rec string_of_expr = function
  | Var x -> x
  | App (f, a) -> Printf.sprintf "(%s %s)" (string_of_expr f) (string_of_expr a)
  | Lam (s, e) -> Printf.sprintf "(λ%s. %s)" s (string_of_expr e)

module S = Set.Make (struct
  type t = string

  let compare = compare
end)

let rec freevars = function
  | Var x -> S.singleton x
  | App (e1, e2) -> S.union (freevars e1) (freevars e2)
  | Lam (x, e) -> S.remove x (freevars e)

let rec freshen x used = if S.mem x used then freshen (x ^ "'") used else x

let subst v x e =
  let rec go subs expr =
    match expr with
    | Var u -> ( match List.assoc_opt u subs with Some e -> e | None -> expr)
    | App (e1, e2) -> App (go subs e1, go subs e2)
    | Lam (s, b) ->
        if s = v then expr
        else if S.mem s (freevars x) then
          (* We have a case like [y->s] (\s. y). If we perform a direct substitution
             we go from referencing "s" in a narrower scope to a tighter scope. So
             instead, generate a fresh variable name for s. *)
          let s' = freshen s (S.union (freevars x) (freevars b)) in
          Lam (s', go ((s, Var s') :: subs) b)
        else Lam (s, go subs b)
  in
  go [ (v, x) ] e

let%expect_test _ =
  subst "y" (Var "s") (Lam ("s", Var "y")) |> string_of_expr |> prerr_string;
  [%expect "(λs'. s)"]

let%expect_test _ =
  subst "y" (Var "s") (Lam ("s", Lam ("s", App (Var "s", Var "y"))))
  |> string_of_expr |> prerr_string;
  [%expect "(λs'. (λs'. (s' s)))"]

let%expect_test _ =
  subst "x" (Var "y") (Lam ("x", Var "x")) |> string_of_expr |> prerr_string;
  [%expect "(λx. x)"]

let%expect_test _ =
  subst "x" (Lam ("z", App (Var "z", Var "w"))) (Lam ("y", Var "x"))
  |> string_of_expr |> prerr_string;
  [%expect "(λy. (λz. (z w)))"]

(** Yields the weak-head normal form of an expression.
    Only beta redexes along the left-hand spine of the expression are reduced,
    leaving arguments "into" the spine untouched.
       ((\x. x) (\x. x)) ((\y. y) z)
    -> (\x. x) ((\y. y) z)
    -> (\y. y) z
    -> z
 *)
let whnf e =
  let rec spine e sp =
    match (e, sp) with
    | App (f, a), rest -> spine f (a :: rest)
    | Lam (s, e), a :: rest -> spine (subst s a e) rest
    | f, rest -> List.fold_left app f rest
  in
  spine e []

let%expect_test _ =
  whnf
    (App
       ( App (Lam ("x", Var "x"), Lam ("x", Var "x")),
         App (Lam ("y", Var "y"), Var "z") ))
  |> string_of_expr |> print_string;
  [%expect "z"]

let%expect_test _ =
  whnf (App (Var "x", App (Lam ("y", Var "y"), Var "z")))
  |> string_of_expr |> print_string;
  [%expect "(x ((λy. y) z))"]

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
    | Lam (s1, b1), Lam (s2, b2) ->
        let s = fresh () in
        cmp ((s1, s) :: n1) ((s2, s) :: n2) b1 b2
    | _ -> false
  in
  cmp [] [] e1 e2

let%test _ =
  let fn x y = Lam (x, App (Lam (y, App (Var y, Var x)), Var x)) in
  alpha_eq (fn "x" "y") (fn "z" "x")

(** Yields the normal form of an expression, containing no beta redexes at
    all, by way of normal-order reduction (which is complete, see
    https://en.wikipedia.org/wiki/Beta_normal_form).
       nf (x ((\y. y) z))
    -> App x (nf ((\y. y) z))
    -> App x z
 *)
let rec nf1 e =
  let rec spine e sp =
    match (e, sp) with
    | App (f, a), rest -> spine f (a :: rest)
    | Lam (s, e), [] -> Lam (s, nf1 e)
    | Lam (s, e), a :: rest -> spine (subst s a e) rest
    | f, rest -> List.fold_left app f (List.map nf1 rest)
  in
  spine e []

exception NoRule

let rec nf = function
  | App (f, a) -> (
      let f = nf f in
      match f with Lam (s, e) -> nf (subst s a e) | _ -> App (f, nf a))
  | Lam (s, e) -> Lam (s, nf e)
  | e -> e

let%expect_test _ =
  nf (App (Var "x", Var "y")) |> string_of_expr |> print_string;
  [%expect "(x y)"]

let%expect_test _ =
  nf (App (Var "x", App (Lam ("y", Var "y"), Var "z")))
  |> string_of_expr |> print_string;
  [%expect "(x z)"]

let%expect_test _ =
  nf (Lam ("x", App (Lam ("y", Var "y"), Var "z")))
  |> string_of_expr |> print_string;
  [%expect "(λx. z)"]

let%expect_test _ =
  let trip = Lam ("w", App (App (Var "w", Var "w"), Var "w")) in
  nf (App (Lam ("x", Var "z"), App (trip, trip)))
  |> string_of_expr |> print_string;
  [%expect "z"]

(** Alpha-beta equality *)
let ab_eq e1 e2 = alpha_eq (nf e1) (nf e2)

let one = Lam ("s", Lam ("z", App (Var "s", Var "z")))

let two = Lam ("s", Lam ("z", App (Var "s", App (Var "s", Var "z"))))

let plus =
  Lam
    ( "m",
      Lam
        ( "n",
          Lam
            ( "s",
              Lam
                ( "z",
                  App
                    ( App (Var "m", Var "s"),
                      App (App (Var "n", Var "s"), Var "z") ) ) ) ) )

let%expect_test _ =
  nf (App (App (plus, one), one)) |> string_of_expr |> print_string;
  [%expect "(λs. (λz. (s (s z))))"]

let%test _ = ab_eq (App (App (plus, one), one)) two
