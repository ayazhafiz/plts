open Surface
open Util

let noloc = ((0, 0), (0, 0))

type effect_op = [ `Fx of string ] [@@deriving show]

type stack_shape = [ `Stk of ty list  (** ordered stack shape *) ]

and ty_content =
  | TBool
  | TInt
  | TFnFx of (ty * ty * stack_shape)
      (** effectful function type t -> [t]_{\bar{t}} where \bar{t} is the stack shape *)
  | TFnCap of (effect_op * stack_shape * ty)
      (** capability function type, of a handler
            [Fx]_{\bar{t}} -> t
          where Fx is an effect operation [effect_op] *)

and ty_var =
  | Unbd of int  (** unbound type *)
  | Link of ty
  | Content of ty_content  (** concrete type *)

and ty = ty_var ref [@@deriving show]
(** A type *)

let rec unlink ty = match !ty with Link t -> unlink t | _ -> ty

type literal = [ `Bool of bool | `Int of int ]
type builtin = [ `Lt | `Gt | `Add | `Sub | `Mul ]
type e_str = loc * ty * string
type recursive = [ `Rec of bool ]

type e_expr = loc * ty * expr
(** An elaborated expression *)

and expr =
  | Var of string
  | Lit of literal
  | Builtin of builtin
  | Abs of e_str * e_stmt  (** \x -> s *)

and e_stmt = loc * ty * stmt

and stmt =
  | App of e_expr * e_expr  (** f x *)
  | If of e_stmt * e_stmt * e_stmt
  | Let of recursive * e_str * e_stmt * e_stmt  (** x <- s; s' *)
  | Return of e_expr  (** return e *)
  | Handle of e_str * e_cap * e_stmt  (** handle c = h in rest *)

and e_cap = loc * ty * cap

and cap =
  | CapVar of string
  | HandlerImpl of effect_op * (e_str * e_str) * e_stmt  (** F(x, k) -> impl *)

type program = e_stmt
(** A whole program *)

type fresh_var = unit -> ty
type parse_ctx = { fresh_var : fresh_var }

(* extractions *)
let xloc (l, _, _) = l
let xty (_, t, _) = t
let xv (_, _, v) = v

let ty_of_builtin : builtin -> ty =
  let topstk = `Stk [] in
  let int = ref @@ Content TInt in
  let bool = ref @@ Content TBool in
  let tfn_int_int = ref @@ Content (TFnFx (int, int, topstk)) in
  let tfn_int_int_int = ref @@ Content (TFnFx (int, tfn_int_int, topstk)) in
  let tfn_int_bool = ref @@ Content (TFnFx (int, bool, topstk)) in
  let tfn_int_int_bool = ref @@ Content (TFnFx (int, tfn_int_bool, topstk)) in
  let map =
    [
      (`Lt, tfn_int_int_bool);
      (`Gt, tfn_int_int_bool);
      (`Add, tfn_int_int_int);
      (`Sub, tfn_int_int_int);
      (`Mul, tfn_int_int_int);
    ]
  in
  fun b -> List.assoc b map

let pp_lit f =
  let open Format in
  function `Bool b -> pp_print_bool f b | `Int n -> pp_print_int f n

let pp_builtin f b =
  let open Format in
  let s =
    match b with
    | `Lt -> "lt"
    | `Gt -> "gt"
    | `Add -> "add"
    | `Sub -> "sub"
    | `Mul -> "mul"
  in
  fprintf f "@%s" s

let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let rec pp_expr f parens =
  let open Format in
  let go parens (_, _, e) =
    match e with
    | Var x -> pp_print_string f x
    | Lit l -> pp_lit f l
    | Builtin b -> pp_builtin f b
    | Abs ((_, _, x), e) ->
        let app () =
          fprintf f "@[<hov 2>\\%s ->@ " x;
          pp_stmt f `Apply e;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
  in
  go parens

and pp_stmt f parens =
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
    | Let (`Rec recursive, (_, _, x), rhs, body) ->
        let recursive = if recursive then " rec" else "" in
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let%s %s =@ " recursive x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
          go `Free body
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
    | Return e -> pp_expr f `Free e
    | If (c, t, e) ->
        fprintf f "@[<hv 0>";
        let if' () =
          fprintf f "@[<hv 2>if@ ";
          go `Free c;
          fprintf f "@]@ @[<hv 2>then@ ";
          go `Free t;
          fprintf f "@]@ @[<hv 2>else@ ";
          go `Free e;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) if';
        fprintf f "@]"
    | Handle ((_, _, cap), handler, rest) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>handle %s =@ " cap;
          pp_cap f `Free handler;
          fprintf f "@]@ in@]@,";
          go `Free rest
        in
        with_parens f (parens >> `Free) expr;
        fprintf f "@]"
  in
  go parens

and pp_cap f parens =
  let open Format in
  let go parens (_, _, c) =
    match c with
    | CapVar x -> pp_print_string f x
    | HandlerImpl (`Fx op, ((_, _, x), (_, _, k)), impl) ->
        let app () =
          fprintf f "@[<hov 2>%s %s %s ->@ " op x k;
          pp_stmt f `Apply impl;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) app
  in
  go parens

let string_of_program ?(width = default_width) (program : program) =
  let open Format in
  with_buffer
    (fun f ->
      fprintf f "@[<v 0>";
      pp_stmt f `Free program;
      fprintf f "@]")
    width
