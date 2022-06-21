open Syntax
open Syntax_help
open Type
open Subtype

type ty = Type.ty
type cmd = Syntax.cmd

let handle_cmd = function
  | CTy (`Sub (s, t)) -> string_of_bool @@ (ty_of_syn s <: ty_of_syn t)
  | CTy (`Iso (s, t)) -> string_of_bool @@ (ty_of_syn s =~ ty_of_syn t)
  | CTy (`Norm t) -> string_of_ty @@ syn_of_ty @@ ty_of_syn t

let compile_cmd s = Result.map handle_cmd (parse_cmd s)
