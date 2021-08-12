module type Int = Tal.Int

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

(** Typed assembly language. *)
module TAL (I : Int) : Lower with type source = A.term

module OCamlInt : Int with type t = int

module OCamlTAL : module type of TAL (OCamlInt)
(** [TAL] with OCaml integers. *)

(** x86 assembly language. *)
module X86 (I : Int) : Lower with type source = TAL(I).term

val parse_term : string -> F.term

exception TyError of string

exception EvalError of string
