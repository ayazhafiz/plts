open Surface
open Util

let noloc = ((0, 0), (0, 0))

type stack_shape = [ `Stk of ty list  (** ordered stack shape *) ]

and ty_content =
  | TBool
  | TFnFx of (ty * ty * stack_shape)
      (** effectful function type t -> [t]_{\bar{t}} where \bar{t} is the stack shape *)

and ty_var =
  | Unbd of int  (** unbound type *)
  | Link of ty
  | Content of ty_content  (** concrete type *)

and ty = ty_var ref [@@deriving show]
(** A type *)

let rec unlink ty = match !ty with Link t -> unlink t | _ -> ty

type literal = [ `Bool of bool ]
type e_str = loc * ty * string

type e_expr = loc * ty * expr
(** An elaborated expression *)

and expr =
  | Var of string
  | Lit of literal
  | Abs of e_str * e_expr  (** \x -> e *)

type e_stmt = loc * ty * stmt

and stmt =
  | App of e_expr * e_expr  (** f x *)
  | Let of e_str * e_stmt * e_stmt  (** x <- s; s' *)
  | Return of e_expr  (** return e *)

type program = e_stmt
(** A whole program *)

type fresh_var = unit -> ty
type parse_ctx = { fresh_var : fresh_var }

(* extractions *)
let xloc (l, _, _) = l
let xty (_, t, _) = t
let xv (_, _, v) = v

let with_parens f needs_parens inside =
  let open Format in
  if needs_parens then pp_print_string f "(";
  inside ();
  if needs_parens then pp_print_string f ")"

let pp_lit f =
  let open Format in
  function `Bool b -> pp_print_bool f b

let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let pp_expr f parens =
  let open Format in
  let rec go parens (_, _, e) =
    match e with
    | Var x -> pp_print_string f x
    | Lit l -> pp_lit f l
    | Abs ((_, _, x), e) ->
        let app () =
          fprintf f "@[<hov 2>\\%s ->@ " x;
          go `Apply e;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
  in
  go parens

let string_of_expr e = with_buffer (fun f -> pp_expr f `Free e) default_width

let pp_stmt f =
  let open Format in
  let rec go parens (_, _, e) =
    match e with
    | App (head, arg) ->
        let app () =
          fprintf f "@[<hov 2>";
          pp_expr f `Apply head;
          fprintf f "@ ";
          pp_expr f `Apply arg;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
    | Let ((_, _, x), rhs, body) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let %s =@ " x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
          go `Free body
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Return e -> pp_expr f `Free e
  in
  go `Free

let string_of_program ?(width = default_width) (program : program) =
  let open Format in
  with_buffer
    (fun f ->
      fprintf f "@[<v 0>";
      pp_stmt f program;
      fprintf f "@]")
    width