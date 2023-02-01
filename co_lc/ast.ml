open Surface
open Util

let noloc = ((0, 0), (0, 0))

(** A generic type variable.
    Either unbound, a reference to another type, or a resolved type. *)
type ty_var =
  | Unbd of int  (** unbound type *)
  | Link of ty_var ref
  | Content of ty_content  (** concrete type *)

and ty_content = TInt | TBool | TTup of ty list | TFn of (ty * ty)

and ty = ty_var ref [@@deriving show]
(** A type *)

let rec unlink ty = match !ty with Link t -> unlink t | _ -> ty

type literal = [ `Bool of bool | `Int of int ]
type binop = [ `Lt | `Add | `Sub ]
type co_op = [ `Spawn | `Yield | `Resume ]
type e_str = loc * ty * string

type letkind =
  [ `Bare | `Rec  (** let rec **) | `Unit  (** foo; e => let _ = foo in e *) ]

type e_expr = loc * ty * expr
(** An elaborated expression *)

and expr =
  | Var of string
  | Lit of literal
  | Tup of e_expr list
  | Let of letkind * e_str * e_expr * e_expr
  | Abs of e_str * e_expr
  | App of e_expr * e_expr
  | Binop of binop * e_expr * e_expr
  | If of e_expr * e_expr * e_expr
  | Access of e_expr * int
  | Spawn of e_expr
  | Yield
  | Resume of e_expr
  | Stat of { cond : e_expr; pending : e_expr; done' : e_str * e_expr }

type program = e_expr
(** A whole program *)

type fresh_var = unit -> ty
type parse_ctx = { fresh_var : fresh_var }

(* extractions *)
let xloc (l, _, _) = l
let xty (_, t, _) = t
let xv (_, _, v) = v

let pp_lit f =
  let open Format in
  function `Bool b -> pp_print_bool f b | `Int n -> pp_print_int f n

let pp_binop f (b : binop) =
  let open Format in
  let s = match b with `Lt -> "<" | `Add -> "+" | `Sub -> "-" in
  fprintf f "%s" s

let pp_co_op f (b : co_op) =
  let open Format in
  let s =
    match b with `Spawn -> "spawn" | `Yield -> "yield" | `Resume -> "resume"
  in
  fprintf f "%s" s

let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let rec pp_expr f parens =
  let open Format in
  let rec go parens (_, _, e) =
    match e with
    | Var x -> pp_print_string f x
    | Lit l -> pp_lit f l
    | Tup es ->
      fprintf f "@[<hov 2>{@,";
      intersperse f "," (fun _ not_fst e -> if not_fst then fprintf f "@ "; go `Free e) es;
      fprintf f "@,}@]"
    | Abs ((_, _, x), e) ->
      let app () =
        fprintf f "@[<hov 2>\\%s ->@ " x;
        go `Free e;
        fprintf f "@]"
      in
      with_parens f (parens >> `Free) app
    | App (head, arg) ->
      let app () =
        fprintf f "@[<hov 2>";
        pp_expr f `Apply head;
        fprintf f "@ ";
        pp_expr f `Apply arg;
        fprintf f "@]"
      in
      with_parens f (parens >> `Free) app
    | Binop (b, l, r) ->
      let app () =
        fprintf f "@[<hov 2>";
        pp_expr f `Apply l;
        fprintf f "@ ";
        pp_binop f b;
        fprintf f " ";
        pp_expr f `Apply r;
        fprintf f "@]"
      in
      with_parens f (parens >> `Free) app
    | Let (((`Bare | `Rec) as r), (_, _, x), rhs, body) ->
      let recursive = if r = `Rec then " rec" else "" in
      fprintf f "@[<v 0>@[<hov 0>";
      let expr () =
        fprintf f "@[<hov 2>let%s %s =@ " recursive x;
        go `Free rhs;
        fprintf f "@]@ in@]@,";
        go `Free body
      in
      with_parens f (parens >> `Free) expr;
      fprintf f "@]"
    | Let (`Unit, _, rhs, body) ->
      fprintf f "@[<v 0>@[<hov 0>";
      let expr () =
        fprintf f "@[<hov 2>";
        go `Free rhs;
        fprintf f "@];@]@,";
        go `Free body
      in
      with_parens f (parens >> `Free) expr;
      fprintf f "@]"
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
    | Access (e, i) ->
      fprintf f "@[<hov 2>";
      go `Free e;
      fprintf f "@,.%d" i;
      fprintf f "@]"
    | Spawn arg ->
      let app () =
        fprintf f "@[<hov 2>spawn@ ";
        pp_expr f `Apply arg;
        fprintf f "@]"
      in
      with_parens f (parens >> `Free) app
    | Yield -> pp_print_string f "yield"
    | Resume arg ->
      let app () =
        fprintf f "@[<hov 2>spawn@ ";
        pp_expr f `Apply arg;
        fprintf f "@]"
      in
      with_parens f (parens >> `Free) app
    | Stat { cond; pending; done' = n, done_body } ->
      fprintf f "@[<v 2>@[<hov 2>stat@ ";
      go `Free cond;
      fprintf f "@]";
      (* pending branch *)
      fprintf f "@,@[<hov 2>| `Pending ->@ ";
      go `Free pending;
      fprintf f "@]";
      (* done branch *)
      fprintf f "@,@[<hov 2>| `Done %s ->@ " (xv n);
      go `Free done_body;
      fprintf f "@]";
      (**)
      fprintf f "@]"
  in
  go parens

let string_of_program ?(width = default_width) (program : program) =
  let open Format in
  with_buffer
    (fun f ->
       fprintf f "@[<v 0>";
       pp_expr f `Free program;
       fprintf f "@]")
    width
