(* module Syntax: syntax trees and associated support functions *)

open Support.Error

(* Data type definitions *)
type ty = TyArr of ty * ty | TyBool

type term =
  | TmVar of info * int * int
  (* A let binding: "let x = t1 in t2" -> "(lambda x: t2) t1" *)
  | TmLet of info * string * (* t1 *) term * (* t2 *) term
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term

type binding = NameBind | VarBind of ty

type command = Eval of info * term | Bind of info * string * binding

(* Contexts *)
type context

val emptycontext : context

val ctxlength : context -> int

val addbinding : context -> string -> binding -> context

val addname : context -> string -> context

val index2name : info -> context -> int -> string

val getbinding : info -> context -> int -> binding

val name2index : info -> context -> string -> int

val isnamebound : context -> string -> bool

val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift : int -> term -> term

val termSubstTop : term -> term -> term

(* Printing *)
val printtm : context -> term -> unit

val printtm_ATerm : bool -> context -> term -> unit

val printty : ty -> unit

val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo : term -> info
