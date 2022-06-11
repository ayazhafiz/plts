type lineco = int * int
(** line * col *)

let string_of_lineco (l, c) = string_of_int l ^ ":" ^ string_of_int c

type loc = lineco * lineco
(** start * end *)

type loc_ty = loc * ty

and ty =
  | TAny
  | TNever
  | TString
  | TInt
  | TArrow of loc_ty * loc_ty
  | TProd of loc_ty * loc_ty
  | TOr of loc_ty * loc_ty
  | TAnd of loc_ty * loc_ty
  | TNot of loc_ty
