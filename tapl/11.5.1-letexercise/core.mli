(* module Core

   Core typechecking and evaluation functions
*)

open Syntax

val eval : context -> term -> term 
val typeof : context -> term -> ty
