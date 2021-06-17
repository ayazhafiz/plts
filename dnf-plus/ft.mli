open Language.Ast

val parse_term : string -> term

val parse_ty : string -> ty

val string_of_term : term -> string

val string_of_ty : ty -> string

type dnf
(** Disjunctive Normal Form representation of a type.
    See 3.2 in Pearce 2012. *)

val dnf : ty -> dnf
(** [dnf t] is the Disjunctive Normal Form of a type [t].
    See 3.2 in Pearce 2012. *)

val ty_of_dnf : dnf -> ty

val dnf_plus : ty -> ty
(** [dnf_plus t] is the Canonicalized Disjunctive Normal Form of a type [t].
    See 4.4 *)

val ( <: ) : ty -> ty -> bool
(** [s <: t] determines if s <: t, i.e. if the subtype relation s <= t holds. *)

val typecheck : term -> (ty, string) Result.t
(** [typecheck tm] typechecks a term, returning the first error found, if any. *)
