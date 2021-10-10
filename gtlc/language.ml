open Util

type loc = int * int * int
(** line * col * byte *)

type pos_info = { start : loc; fin : loc }

let fauxinfo = { start = (-1, -1, -1); fin = (-1, -1, -1) }

type 't rawty =
  | TUnknown
  | TNat
  | TBool
  | TArrow of 't * 't
  | TRef of 't
  | TInfer of [ `Var of int | `Resolved of 't rawty ]

and ty = Ty of ty rawty * pos_info

and triv_ty = Tty of triv_ty rawty

module TyMap = Map.Make (struct
  type t = ty

  let compare = compare
end)

module TtyMap = Map.Make (struct
  type t = triv_ty

  let compare = compare
end)

let ft ty = Ty (ty, fauxinfo)

let tt ty = Tty ty

let rec trivialize (Ty (t, _)) =
  let t' =
    match t with
    | TUnknown -> TUnknown
    | TNat -> TNat
    | TBool -> TBool
    | TArrow (t1, t2) -> TArrow (trivialize t1, trivialize t2)
    | TRef t -> TRef (trivialize t)
    | TInfer (`Var i) -> TInfer (`Var i)
    | TInfer (`Resolved t) ->
        let (Tty t) = trivialize (ft t) in
        TInfer (`Resolved t)
  in
  Tty t'

let rec fauxify (Tty t) =
  let t' =
    match t with
    | TUnknown -> TUnknown
    | TNat -> TNat
    | TBool -> TBool
    | TArrow (t1, t2) -> TArrow (fauxify t1, fauxify t2)
    | TRef t -> TRef (fauxify t)
    | TInfer (`Var i) -> TInfer (`Var i)
    | TInfer (`Resolved t) ->
        let (Ty (t, _)) = fauxify (tt t) in
        TInfer (`Resolved t)
  in
  ft t'

type 'sub expr =
  | Nat of int
  | Bool of bool
  | Var of [ `Global of string | `Local of string ]
  | App of 'sub * 'sub * [ `App | `DesugaredLet | `DesugaredSeq ]
  | Lam of (string * pos_info) * ty * 'sub
  | If of 'sub * 'sub * 'sub
  | Ref of 'sub
  | RefAssign of 'sub * 'sub
  | Deref of 'sub

(** Unelaborated expressions (due to parsing) *)
and unelaborated_expr = Just of unelaborated_expr expr * pos_info

(** Unelaborated expressions (after typechecking) *)
and elaborated_expr = Elab of elaborated_expr expr * pos_info * ty

(** A generator to generate fresh inferrable types. *)
let freshty_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    Tty (TInfer (`Var !n))

type tctx = [ `Free | `Arrow | `Head | `TyArg | `Infer ]

let int_of_tctx = function
  | `Free -> 1
  | `Arrow -> 2
  | `Head -> 3
  | `TyArg -> 4
  | `Infer -> 5

(** [p >> s] means [p] is a stricter context than [s]. *)
let ( >| ) p s = int_of_tctx p > int_of_tctx s

let pp_ty1 f get =
  let open Format in
  let paren doparen inside =
    if doparen then fprintf f "(";
    inside ();
    if doparen then fprintf f ")"
  in
  let rec go flag ty =
    match ty with
    | TUnknown -> pp_print_string f "?"
    | TNat -> pp_print_string f "nat"
    | TBool -> pp_print_string f "bool"
    | TArrow (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren
          (flag >| `Arrow)
          (fun () ->
            go `Head (get t1);
            fprintf f " ->@ ";
            go `Free (get t2));
        fprintf f "@]"
    | TRef ty ->
        fprintf f "@[";
        paren
          (flag >| `TyArg)
          (fun () ->
            fprintf f "ref ";
            go `TyArg (get ty));
        fprintf f "@]"
    | TInfer (`Var n) -> fprintf f "`t%d" n
    | TInfer (`Resolved t) ->
        fprintf f "@[`";
        go `Infer t;
        fprintf f "@]"
  in
  go `Free

let pp_ty f (Ty (t, _)) = pp_ty1 f (fun (Ty (ty, _)) -> ty) t

let pp_tty f (Tty t) = pp_ty1 f (fun (Tty ty) -> ty) t

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
  let rec go p (Just (e, _)) =
    match e with
    | Nat n -> pp_print_int f n
    | Bool true -> pp_print_string f "#t"
    | Bool false -> pp_print_string f "#f"
    | Var (`Global x | `Local x) -> pp_print_string f x
    | App (Just (Lam ((x, _), t, body), _), rhs, `DesugaredLet) ->
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
    | App (Just (Lam (_, _, body), _), rhs, `DesugaredSeq) ->
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
    | Lam ((x, _), t, e) ->
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

let string_of_tty ?(width = default_width) ty =
  with_buffer (fun f -> pp_tty f ty) width

let string_of_expr ?(width = default_width) expr =
  with_buffer (fun f -> pp_expr f expr) width
