type ty = Arrow of ty * ty | TVar of string

type kind = KArrow of kind * kind | Star

type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * ty * expr
  | TLam of string * kind * expr
  | TApp of expr * ty

let rec string_of_ty = function
  | Arrow (l, r) -> Printf.sprintf "(%s->%s)" (string_of_ty l) (string_of_ty r)
  | TVar v -> v

let rec string_of_kind = function
  | Star -> "*"
  | KArrow (l, r) ->
      Printf.sprintf "(%s->%s)" (string_of_kind l) (string_of_kind r)

let rec string_of_expr = function
  | Var x -> x
  | App (f, a) -> Printf.sprintf "(%s %s)" (string_of_expr f) (string_of_expr a)
  | Lam (s, t, e) ->
      Printf.sprintf "(λ%s:%s. %s)" s (string_of_ty t) (string_of_expr e)
  | TLam (s, k, e) ->
      Printf.sprintf "(Λ%s:%s. %s)" s (string_of_kind k) (string_of_expr e)
  | TApp (e, t) -> Printf.sprintf "(%s[%s])" (string_of_expr e) (string_of_ty t)

let ( >>= ) = Option.bind
