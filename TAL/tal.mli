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

  val eval : term -> term
end

(** Continuation-passing-style conversion pass. *)
module K : sig
  include Lang

  type value

  val string_of_value : value -> string

  val of_F : F.term -> term

  val check_well_typed : term -> unit

  val eval : term -> value
end

val parse_term : string -> F.term
