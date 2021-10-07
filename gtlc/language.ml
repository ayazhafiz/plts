open Util

type ty =
  | TUnknown
  | TNat
  | TBool
  | TArrow of ty * ty
  | TRef of ty
  | TInfer of [ `Var of int | `Resolved of ty ]

module TyMap = Map.Make (struct
  type t = ty

  let compare = compare
end)

type 'sub expr =
  | Nat of int
  | Bool of bool
  | Var of [ `Global of string | `Local of string ]
  | App of 'sub * 'sub * [ `App | `DesugaredLet | `DesugaredSeq ]
  | Lam of string * ty * 'sub
  | If of 'sub * 'sub * 'sub
  | Ref of 'sub
  | RefAssign of 'sub * 'sub
  | Deref of 'sub

(** Unelaborated expressions (due to parsing) *)
and unelaborated_expr = Just of unelaborated_expr expr

(** Unelaborated expressions (after typechecking) *)
and elaborated_expr = Elab of elaborated_expr expr * ty

(** A generator to generate fresh inferrable types. *)
let freshty_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    TInfer (`Var !n)

type tctx = [ `Free | `Arrow | `Head | `TyArg | `Infer ]

let int_of_tctx = function
  | `Free -> 1
  | `Arrow -> 2
  | `Head -> 3
  | `TyArg -> 4
  | `Infer -> 5

(** [p >> s] means [p] is a stricter context than [s]. *)
let ( >| ) p s = int_of_tctx p > int_of_tctx s

let pp_ty f =
  let open Format in
  let paren doparen inside =
    if doparen then fprintf f "(";
    inside ();
    if doparen then fprintf f ")"
  in
  let rec go t = function
    | TUnknown -> pp_print_string f "?"
    | TNat -> pp_print_string f "nat"
    | TBool -> pp_print_string f "bool"
    | TArrow (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren
          (t >| `Arrow)
          (fun () ->
            go `Head t1;
            fprintf f " ->@ ";
            go `Free t2);
        fprintf f "@]"
    | TRef ty ->
        fprintf f "@[";
        paren
          (t >| `TyArg)
          (fun () ->
            fprintf f "ref ";
            go `TyArg ty);
        fprintf f "@]"
    | TInfer (`Var n) -> fprintf f "`t%d" n
    | TInfer (`Resolved t) ->
        fprintf f "@[`";
        go `Infer t;
        fprintf f "@]"
  in
  go `Free

type pctx = [ `Free | `AppHead | `Arg ]

let int_of_pctx = function `Free -> 1 | `AppHead -> 2 | `Arg -> 3

(** [p >> s] means [p] is a stricter context than [s]. *)
let ( >> ) p s = int_of_pctx p > int_of_pctx s

let pp_expr f =
  let open Format in
  let paren doparen inside =
    if doparen then fprintf f "(";
    inside ();
    if doparen then fprintf f ")"
  in
  let rec go p (Just e) =
    match e with
    | Nat n -> pp_print_int f n
    | Bool true -> pp_print_string f "#t"
    | Bool false -> pp_print_string f "#f"
    | Var (`Global x | `Local x) -> pp_print_string f x
    | App (Just (Lam (x, t, body)), rhs, `DesugaredLet) ->
        fprintf f "@[<v 0>";
        paren
          (p >> `Free)
          (fun () ->
            fprintf f "@[<hov 2>let %s: " x;
            pp_ty f t;
            fprintf f " =@ ";
            go `Free rhs;
            fprintf f " in@]@,";
            go `Free body);
        fprintf f "@]"
    | App (Just (Lam (_, _, body)), rhs, `DesugaredSeq) ->
        fprintf f "@[<v 0>";
        paren
          (p >> `Free)
          (fun () ->
            fprintf f "@[";
            go `Free rhs;
            fprintf f ";@]@,";
            go `Free body);
        fprintf f "@]"
    | App (_, _, (`DesugaredLet | `DesugaredSeq)) -> failwith "unreachable"
    | App (head, arg, `App) ->
        fprintf f "@[<hov 2>";
        paren
          (p >> `AppHead)
          (fun () ->
            fprintf f "@[<hv 0>";
            go `AppHead head;
            fprintf f "@ ";
            go `Arg arg;
            fprintf f "@]");
        fprintf f "@]"
    | Lam (x, t, e) ->
        fprintf f "@[<hov 2>";
        paren
          (p >> `Free)
          (fun () ->
            fprintf f "λ%s: " x;
            pp_ty f t;
            fprintf f ".@ ";
            go `Free e);
        fprintf f "@]"
    | If (c, t, e) ->
        fprintf f "@[<hv 0>";
        paren
          (p >> `Free)
          (fun () ->
            fprintf f "@[<hv 2>if@ ";
            go `Free c;
            fprintf f "@]@ @[<hv 2>then@ ";
            go `Free t;
            fprintf f "@]@ @[<hv 2>else@ ";
            go `Free e;
            fprintf f "@]");
        fprintf f "@]"
    | Ref e ->
        fprintf f "@[ref ";
        go `Arg e;
        fprintf f "@]"
    | RefAssign (e1, e2) ->
        fprintf f "@[<hov 2>";
        go `Free e1;
        fprintf f " :=@ ";
        go `Free e2;
        fprintf f "@]"
    | Deref e ->
        fprintf f "@[!";
        go `Arg e;
        fprintf f "@]"
  in
  go `Free

let string_of_ty ?(width = default_width) ty =
  with_buffer (fun f -> pp_ty f ty) width

let string_of_expr ?(width = default_width) expr =
  with_buffer (fun f -> pp_expr f expr) width
