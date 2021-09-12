open Language
open Typecheck

(*** Front ***)

type expr = unelaborated_expr

type elaborated_expr = Language.elaborated_expr

type ty = Language.ty

let builtins =
  [
    ("succ", TArrow (TNat, TNat));
    ("pred", TArrow (TNat, TNat));
    ("add", TArrow (TNat, TArrow (TNat, TNat)));
    ("mult", TArrow (TNat, TArrow (TNat, TNat)));
    ("eqn", TArrow (TNat, TArrow (TNat, TBool)));
    ("eqb", TArrow (TBool, TArrow (TBool, TBool)));
    ("eqb", TArrow (TBool, TArrow (TBool, TBool)));
  ]

let string_of_position ({ pos_lnum; pos_cnum; pos_bol; _ } : Lexing.position) =
  Printf.sprintf "%d:%d" pos_lnum (pos_cnum - pos_bol + 1)

let parse s =
  let full = s in
  let lexbuf = Lexing.from_string ~with_positions:true full in
  try
    let parsed = Parser.toplevel_expr Lexer.read lexbuf in
    Ok parsed
  with
  | Lexer.SyntaxError what ->
      Error
        (Printf.sprintf "Syntax error: %s at %s" what
           (string_of_position lexbuf.lex_curr_p))
  | Parser.Error ->
      Error
        (Printf.sprintf "Parse error at %s"
           (string_of_position lexbuf.lex_curr_p))

let elaborate = elaborate builtins

let ty_of_elaborated_expr (Elab (_, t)) = t

let string_of_ty = string_of_ty

(*** IR ***)

type cast_expr = Cast_ir.Expr.elaborated_expr

let insert_casts = Cast_ir.translate

let string_of_cast_expr = Cast_ir.string_of_expr

(*** Lifts ***)

type lifted_program = Lift_ir.Linearize.program

let lift = Lift_ir.translate

let string_of_lifted_program = Lift_ir.Linearize.string_of_program

(*** Eval ***)

type value = Cast_ir.Expr.elaborated_expr

type eval_error = Eval.error

let eval e =
  let open Printf in
  let open Cast_ir in
  Cast_ir.translate e |> Eval.eval
  |> Result.map_error (function
       | Eval.CastError e -> sprintf "Cast Error at %s" (string_of_expr e)
       | Eval.TypeError e -> sprintf "Type Error at %s" (string_of_expr e))

let string_of_value = Cast_ir.string_of_expr

(*** Codegen ***)

module Cgen = struct
  let typescript lifted = Typescript.(trans_program lifted |> string_of_program)
end
