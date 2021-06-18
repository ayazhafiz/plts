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

val infer_types : term -> (term, string) Result.t
(** [infer_types tm] infers the types in a term, returning the first semantic or
    inference error found, if any.
    NB: This is a tool to generate types for terms, and not a substitute for
    [typecheck]. Indeed, a term should first have its types inferred, then be
    typechecked. *)
