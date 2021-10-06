(*** Front ***)
type expr

type elaborated_expr
(** Like expr, but with types attached on nodes. *)

type ty

type freshty

val parse : string -> (expr * freshty, string) Result.t

val infer : expr -> freshty -> (expr, string) Result.t

val elaborate : expr -> (elaborated_expr, string) Result.t

val ty_of_elaborated_expr : elaborated_expr -> ty

val string_of_expr : ?width:int -> expr -> string

val string_of_ty : ?width:int -> ty -> string

type builtin = { name : string; ty : string; doc : string }

val builtin_docs : builtin list

(*** IR ***)

type cast_expr

val insert_casts : elaborated_expr -> cast_expr

val string_of_cast_expr : ?width:int -> cast_expr -> string

(*** Lifts ***)

type lifted_program

val lift : optimize:bool -> cast_expr -> lifted_program

val string_of_lifted_program : ?width:int -> lifted_program -> string

(*** Evaluation ***)

type value

type eval_error

val eval : elaborated_expr -> (value, string) Result.t

val string_of_value : ?width:int -> value -> string

(*** Codegen ***)

module Cgen : sig
  val typescript : ?width:int -> ?with_prelude:bool -> lifted_program -> string

  val c : ?width:int -> ?with_prelude:bool -> lifted_program -> string
end
