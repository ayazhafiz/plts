open Surface
open Util
open Symbol

let noloc = ((0, 0), (0, 0))

(** Simple type for lambda calculus *)
type ty =
  | TVar of int  (** Type variable *)
  | TFn of ty * ty  (** Function type: t1 -> t2 *)

type e_expr = loc * ty option * expr
(** An elaborated expression with location and type information *)

(** Lambda calculus expressions *)
and expr =
  | Var of symbol  (** Variable *)
  | Abs of symbol * e_expr  (** Lambda abstraction: λx.e *)
  | App of e_expr * e_expr  (** Function application: e1 e2 *)

type program = e_expr
(** A whole program *)

type parse_ctx = { symbols : Symbol.t; opt_name : symbol option ref }

(* extractions *)
let xloc (l, _, _) = l
let xty (_, t, _) = t
let xv (_, _, v) = v
let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

(** Pretty print an expression *)
let pp_expr f symbols parens =
  let open Format in
  let rec go parens (_, _, e) =
    match e with
    | Var x -> pp_print_string f (Symbol.string_of symbols x)
    | Abs (x, e) ->
        let app () =
          fprintf f "@[<hov 2>\\%s ->@ " (Symbol.string_of symbols x);
          go `Free e;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
    | App (head, arg) ->
        let app () =
          fprintf f "@[<hov 2>";
          go `Apply head;
          fprintf f "@ ";
          go `Apply arg;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
  in
  go parens

(** Convert a program to a string *)
let string_of_program ?(width = default_width) (symbols : Symbol.t)
    (program : program) =
  let open Format in
  with_buffer
    (fun f ->
      fprintf f "@[<v 0>";
      pp_expr f symbols `Free program;
      fprintf f "@]")
    width

(** Pretty print a type *)
let rec pp_ty f = function
  | TVar n -> Format.fprintf f "t%d" n
  | TFn (t1, t2) ->
      Format.fprintf f "@[<hov 2>";
      let pty () =
        pp_ty_atom f t1;
        Format.fprintf f " -> ";
        pp_ty f t2
      in
      with_parens f (is_fn t1) pty;
      Format.fprintf f "@]"

and pp_ty_atom f = function
  | TVar _ as t -> pp_ty f t
  | TFn _ as t -> Format.fprintf f "(%a)" pp_ty t

and is_fn = function TFn _ -> true | _ -> false

(** Convert a type to a string *)
let string_of_ty ?(width = default_width) (ty : ty) =
  with_buffer (fun f -> pp_ty f ty) width
