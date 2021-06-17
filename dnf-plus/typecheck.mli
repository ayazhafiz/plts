open Language.Ast

val flatten_ty : ty -> ty

type dnf

val dnf : ty -> dnf

val ty_of_dnf : dnf -> ty

val dnf_plus : ty -> ty

val ( <: ) : ty -> ty -> bool

val typecheck : term -> (ty, string) Result.t
