open Language
open Typecheck
open Infer
open Builtin
open Util

(*** Front ***)

type expr = unelaborated_expr

type elaborated_expr = Language.elaborated_expr

type ty = Language.ty

type freshty = unit -> ty

type builtin = { name : string; ty : string; doc : string }

let builtin_docs =
  List.map
    (fun ({ name; ty; doc } : Builtin.builtin) ->
      { name; ty = string_of_ty ty; doc })
    builtins

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse s =
  let full = s in
  let lexbuf = Lexing.from_string ~with_positions:true full in
  try
    let freshty = freshty_generator () in
    let parsed = Parser.toplevel_expr Lexer.read lexbuf freshty in
    Ok (parsed, freshty)
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position lexbuf.lex_curr_p))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position lexbuf.lex_curr_p))

let infer e freshty =
  let ctx =
    List.map (fun ({ name; ty; _ } : Builtin.builtin) -> (name, ty)) builtins
  in
  infer freshty ctx e

let elaborate =
  let global_ctx =
    List.map
      (fun ({ name; ty; _ } : Builtin.builtin) ->
        (name, (ty, true (* global *))))
      builtins
  in
  elaborate global_ctx

let ty_of_elaborated_expr (Elab (_, t)) = t

let string_of_expr = string_of_expr

let string_of_ty = string_of_ty

(*** Cast IR ***)

type cast_expr = Cast_ir.Expr.elaborated_expr

let insert_casts = Cast_ir.translate

let string_of_cast_expr = Cast_ir.string_of_expr

(*** Lift IR ***)

type lifted_program = Lift_ir.program

let lift ~optimize e =
  let open Lift_ir in
  let optimizations = [ CollapseVars.apply ] in
  let e' = translate e in
  if optimize then List.fold_left (fun e pass -> pass e) e' optimizations
  else e'

let string_of_lifted_program = Lift_ir.string_of_program

(*** Eval ***)

type value = Cast_ir.Expr.elaborated_expr

type eval_error = Eval.error

let eval e =
  let open Printf in
  let open Cast_ir in
  Cast_ir.translate e |> Eval.eval_top
  |> Result.map_error (function
       | Eval.CastError e -> sprintf "Cast Error at %s" (string_of_expr e)
       | Eval.TypeError e -> sprintf "Type Error at %s" (string_of_expr e))

let string_of_value = Cast_ir.string_of_expr

(*** Codegen ***)

module Cgen = struct
  let typescript ?(width = default_width) ?(with_prelude = true) =
    Typescript.string_of_program width with_prelude

  let c ?(width = default_width) ?(with_prelude = true) =
    C.string_of_program width with_prelude
end
