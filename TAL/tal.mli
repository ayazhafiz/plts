module type Lang = sig
  type term

  type ty

  val string_of_term : term -> string

  val string_of_ty : ty -> string
end

(** The System F-like source language. *)
module F : sig
  include Lang

  val typeof : term -> ty
end

(** Continuation-passing-style conversion pass. *)
module K : sig
  include Lang

  val check_well_typed : term -> unit

  val of_F : F.term -> term
end

val parse_term : string -> F.term
