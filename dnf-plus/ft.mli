open Language.Ast

val parse_term : string -> term

val parse_ty : string -> ty

val string_of_term : term -> string

val string_of_ty : ty -> string

val dnf : ty -> ty
(** [dnf t] is the Disjunctive Normal Form of a type [t].
    See 3.2 in Pearce 2012. *)
