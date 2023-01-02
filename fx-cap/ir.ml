(** Intermediate representation: STLC *)

type ty = TBool | TFn of ty * ty
type e_str = ty * string

type expr =
  | Var of string
  | Lit of Ast.literal
  | Let of (ty * string) * e_expr * e_expr
  | Abs of (ty * string) * e_expr
  | App of e_expr * e_expr

and e_expr = ty * expr
