open Util

type ty = TUnknown | TNat | TBool | TArrow of ty * ty

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
  in
  go ~left_arrow_child:false

let string_of_ty ty = with_buffer (fun f -> pp_ty f ty) 80
