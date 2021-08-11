module type Lang = sig
  type term

  type ty

  val string_of_term : term -> string

  val string_of_ty : ty -> string
end

module type Lower = sig
  include Lang

  type source

  type value

  val string_of_value : value -> string

  val convert : source -> term

  val check_well_typed : term -> unit

  val eval : term -> value
end

(** The System F-like source language. *)
module F : sig
  include Lang

  type elaborated_term

  val elaborate : term -> elaborated_term
  (** Elaborate subterms with their types. *)

  val typeof : elaborated_term -> ty

  val eval : term -> term
end

module K : Lower with type source = F.elaborated_term
(** Continuation-passing-style conversion pass. *)

module C : Lower with type source = K.term
(** Closure converison pass. *)

module H : Lower with type source = C.term
(** Hoisting pass. *)

module A : Lower with type source = H.term
(** Explicit allocation pass. *)

module TAL : Lower with type source = A.term
(** Typed assembly language. *)

val parse_term : string -> F.term

exception TyError of string

exception EvalError of string
