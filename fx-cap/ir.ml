(** Intermediate representation: STLC *)

type ty = TBool | TInt | TFn of ty * ty
type e_str = ty * string

type expr =
  | Var of string
  | Lit of Ast.literal
  | Let of (ty * string) * e_expr * e_expr
  | Abs of (ty * string) * e_expr
  | App of e_expr * e_expr
  | If of e_expr * e_expr * e_expr

and e_expr = ty * expr

let int_of_parens_ctx = function `Free -> 1 | `Apply -> 2
let ( >> ) ctx1 ctx2 = int_of_parens_ctx ctx1 > int_of_parens_ctx ctx2

let rec pp_expr f parens =
  let open Format in
  let open Util in
  let rec go parens (_, e) =
    match e with
    | Var x -> pp_print_string f x
    | Lit l -> Ast.pp_lit f l
    | Abs ((_, x), e) ->
        let app () =
          fprintf f "@[<hov 2>\\%s ->@ " x;
          go `Apply e;
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
    | Let ((_, x), rhs, body) ->
        fprintf f "@[<v 0>@[<hv 0>";
        let expr () =
          fprintf f "@[<hv 2>let %s =@ " x;
          go `Free rhs;
          fprintf f "@]@ in@]@,";
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
  in
  go parens

let string_of_program ?(width = Util.default_width) (program : e_expr) =
  let open Format in
  Util.with_buffer
    (fun f ->
      fprintf f "@[<v 0>";
      pp_expr f `Free program;
      fprintf f "@]")
    width

let pp_ty f t =
  let open Format in
  let open Util in
  let rec go parens t =
    match t with
    | TBool -> fprintf f "bool"
    | TInt -> fprintf f "int"
    | TFn (in', out) ->
        let fn () =
          fprintf f "@[<hov 2>";
          go `Apply in';
          fprintf f "@ -> ";
          go `Apply out;
          fprintf f "@]"
        in
        with_parens f (parens >> `Free) fn
  in
  go `Free t
