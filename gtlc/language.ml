open Util

type ty = TUnknown | TNat | TBool | TArrow of ty * ty | TInfer of int

module TyMap = Map.Make (struct
  type t = ty

  let compare = compare
end)

type 'sub expr =
  | Nat of int
  | Bool of bool
  | Var of [ `Global of string | `Local of string ]
  | App of 'sub * 'sub
  | Lam of string * ty * 'sub
  | If of 'sub * 'sub * 'sub

(** Unelaborated expressions (due to parsing) *)
and unelaborated_expr = Just of unelaborated_expr expr

(** Unelaborated expressions (after typechecking) *)
and elaborated_expr = Elab of elaborated_expr expr * ty

(** A generator to generate fresh inferrable types. *)
let freshty_generator () =
  let n = ref 0 in
  fun () ->
    incr n;
    TInfer !n

let pp_ty f =
  let open Format in
  let rec go ?(left_arrow_child = false) = function
    | TUnknown -> pp_print_string f "?"
    | TNat -> pp_print_string f "nat"
    | TBool -> pp_print_string f "bool"
    | TArrow (t1, t2) ->
        fprintf f "@[<hov 2>";
        if left_arrow_child then pp_print_string f "(";
        go ~left_arrow_child:true t1;
        fprintf f " ->@ ";
        go t2;
        if left_arrow_child then pp_print_string f ")";
        fprintf f "@]"
    | TInfer n -> fprintf f "`t%d" n
  in
  go ~left_arrow_child:false

let pp_expr f =
  let open Format in
  let rec go (Just e) =
    match e with
    | Nat n -> pp_print_int f n
    | Bool b -> pp_print_bool f b
    | Var (`Global x | `Local x) -> pp_print_string f x
    | App (head, arg) ->
        fprintf f "@[<hov 2>(@[<hv 0>";
        go head;
        fprintf f "@ ";
        go arg;
        fprintf f "@])@]"
    | Lam (x, t, e) ->
        fprintf f "@[<hov 2>(λ%s: " x;
        pp_ty f t;
        fprintf f ".@ ";
        go e;
        fprintf f ")@]"
    | If (c, t, e) ->
        fprintf f "@[<hv 0>@[<hov 2>if@ ";
        go c;
        fprintf f "@]@ @[<v 2>then@ ";
        go t;
        fprintf f "@]@ @[<v 2>else@ ";
        go e;
        fprintf f "@]@]"
  in
  go

let string_of_ty ty = with_buffer (fun f -> pp_ty f ty) 80

let string_of_expr expr = with_buffer (fun f -> pp_expr f expr) 80
