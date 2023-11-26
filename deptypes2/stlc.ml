type ty = Base | Arrow of ty * ty

type expr = Var of string | App of expr * expr | Lam of string * ty * expr

let rec string_of_ty = function
  | Base -> "B"
  | Arrow (l, r) -> Printf.sprintf "(%s->%s)" (string_of_ty l) (string_of_ty r)

let rec string_of_expr = function
  | Var x -> x
  | App (f, a) -> Printf.sprintf "(%s %s)" (string_of_expr f) (string_of_expr a)
  | Lam (s, t, e) ->
      Printf.sprintf "(λ%s:%s. %s)" s (string_of_ty t) (string_of_expr e)

let ( >>= ) = Option.bind

let rec tyck ctx = function
  | Var x -> List.assoc_opt x ctx
  | App (f, a) -> (
      tyck ctx f >>= function
      | Arrow (t1, t2) ->
          tyck ctx a >>= fun ta -> if t1 = ta then Some t2 else None
      | _ -> None)
  | Lam (s, t1, b) ->
      tyck ((s, t1) :: ctx) b >>= fun t2 -> Some (Arrow (t1, t2))

let%expect_test _ =
  tyck []
    (Lam ("x", Arrow (Base, Base), Lam ("y", Base, App (Var "x", Var "y"))))
  |> Option.get |> string_of_ty |> prerr_string;
  [%expect "((B->B)->(B->B))"]
