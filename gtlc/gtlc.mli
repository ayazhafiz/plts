(*** Front ***)
type expr

type elaborated_expr
(** Like expr, but with types attached on nodes. *)

type ty

val parse : string -> (expr, string) Result.t

val elaborate : expr -> (elaborated_expr, string) Result.t

val ty_of_elaborated_expr : elaborated_expr -> ty

val string_of_ty : ty -> string

type builtin = { name : string; ty : string; doc : string }

val builtin_docs : builtin list

(*** IR ***)

type cast_expr

val insert_casts : elaborated_expr -> cast_expr

val string_of_cast_expr : cast_expr -> string

(*** Lifts ***)

type lifted_program

val lift : optimize:bool -> cast_expr -> lifted_program

val string_of_lifted_program : lifted_program -> string

(*** Evaluation ***)

type value

type eval_error

val eval : elaborated_expr -> (value, string) Result.t

val string_of_value : value -> string

(*** Codegen ***)

module Cgen : sig
  val typescript : ?with_prelude:bool -> lifted_program -> string

  val c : ?with_prelude:bool -> lifted_program -> string
end
