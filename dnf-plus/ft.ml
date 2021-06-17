open Language.Ast
open Typecheck

let string_of_term = Language.string_of_term

let string_of_ty = Language.string_of_ty

let rec flatten_ty_in_term t =
  match t with
  | Num _ | Var _ -> t
  | Tup ts -> Tup (List.map flatten_ty_in_term ts)
  | App (fn, t) -> App (fn, flatten_ty_in_term t)
  | Dec (fn, params, body, cont) ->
      Dec
        ( fn,
          List.map (fun (p, t) -> (p, flatten_ty t)) params,
          flatten_ty_in_term body,
          flatten_ty_in_term cont )
  | If (var, isty, then', else') ->
      If
        ( var,
          flatten_ty isty,
          flatten_ty_in_term then',
          flatten_ty_in_term else' )

let lex = Lexing.from_string ~with_positions:true

let parse_term s =
  lex s |> Parser.toplevel_term Lexer.read |> flatten_ty_in_term

let parse_ty s = lex s |> Parser.toplevel_ty Lexer.read |> flatten_ty

type dnf = Typecheck.dnf

let dnf = Typecheck.dnf

let ty_of_dnf = Typecheck.ty_of_dnf

let dnf_plus = Typecheck.dnf_plus

let ( <: ) = Typecheck.( <: )
