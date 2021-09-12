module Ir = Lift_ir.Linearize
module L = Language
open Util

type ty = TUnknown | TNumber | TBool | TArrow of ty * ty

type expr =
  | Number of int
  | Bool of bool
  | Name of string
  | Call of expr * expr list

type stmt =
  | Decl of string * ty * expr option
  | DeclCast of string * ty * expr
  | Assign of string * expr
  | If of expr * stmt list * stmt list
  | Ret of expr

type fn = {
  name : string;
  params : (string * ty) list;
  body : stmt list;
  ret : ty;
}

type program = { main : string; fns : fn list }

let rec trans_ty = function
  | L.TUnknown -> TUnknown
  | L.TNat -> TNumber
  | L.TBool -> TBool
  | L.TArrow (t1, t2) -> TArrow (trans_ty t1, trans_ty t2)

let rec trans_expr (Ir.Elab (e, _)) =
  match e with
  | Ir.Nat n -> Number n
  | Ir.Bool b -> Bool b
  | Ir.Name x -> Name x
  | Ir.Call (f, ps) -> Call (trans_expr f, List.map trans_expr ps)

let rec trans_stmt = function
  | Ir.Decl (x, t) -> Decl (x, trans_ty t, None)
  | Ir.DeclInit (x, t, e) -> Decl (x, trans_ty t, Some (trans_expr e))
  | Ir.DeclCast (x, t, e) -> DeclCast (x, trans_ty t, trans_expr e)
  | Ir.Assign (x, e) -> Assign (x, trans_expr e)
  | Ir.If (c, t, e) ->
      If (trans_expr c, List.map trans_stmt t, List.map trans_stmt e)

let rec trans_seq = function
  | Ir.Expr e -> [ Ret (trans_expr e) ]
  | Ir.Seq (s, rest) -> trans_stmt s :: trans_seq rest

let trans_fn ({ name; params; body; ret } : Ir.fn) =
  let params = List.map (fun (p, t) -> (p, trans_ty t)) params in
  let body = trans_seq body in
  let ret = trans_ty ret in
  { name; params; body; ret }

let trans_program ({ toplevels; body; ty; fresh } : Ir.program) =
  let fns = List.map trans_fn toplevels in
  let main = fresh "main" in
  let main_fn = trans_fn { name = main; params = []; body; ret = ty } in
  { main; fns = fns @ [ main_fn ] }

let pp_ty f =
  let open Format in
  let rec go = function
    | TUnknown -> pp_print_string f "unknown"
    | TNumber -> pp_print_string f "number"
    | TBool -> pp_print_string f "boolean"
    | TArrow (t, t') ->
        fprintf f "@[<hov 2>((_: ";
        go t;
        fprintf f ") =>@ ";
        go t';
        fprintf f ")@]"
  in
  go

let pp_expr f =
  let open Format in
  let rec go = function
    | Number n -> pp_print_int f n
    | Bool b -> pp_print_bool f b
    | Name x -> pp_print_string f x
    | Call (head, params) ->
        fprintf f "@[<hov 2>";
        go head;
        fprintf f "(@[<hv 0>";
        let lasti = List.length params - 1 in
        List.iteri
          (fun i p ->
            go p;
            if i <> lasti then fprintf f ",@ ")
          params;
        fprintf f "@])@]"
  in
  go

let pp_stmt f =
  let open Format in
  let rec go = function
    | Decl (x, t, None) ->
        fprintf f "let @[<hov 2>%s@,: " x;
        pp_ty f t;
        fprintf f ";@]"
    | Decl (x, t, Some e) ->
        fprintf f "const @[<hv 2>%s@,: " x;
        pp_ty f t;
        fprintf f "@ = ";
        pp_expr f e;
        fprintf f ";@]"
    | DeclCast (x, t, e) ->
        fprintf f "const @[<hv 2>%s@,: " x;
        pp_ty f t;
        fprintf f "@ = @[<hov 2>";
        pp_expr f e;
        fprintf f "@ as ";
        pp_ty f t;
        fprintf f "@];@]"
    | Assign (x, e) ->
        fprintf f "@[<hv 2>%s@ = " x;
        pp_expr f e;
        fprintf f ";@]"
    | If (c, t, e) ->
        fprintf f "@[<v 0>@[<v 2>@[<v 2>if@ (";
        pp_expr f c;
        fprintf f ")@] {@ ";
        let lasti = List.length t - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          t;
        fprintf f "@]@ }@]@[<v 1> else {@ ";
        let lasti = List.length e - 1 in
        List.iteri
          (fun i s ->
            go s;
            if i <> lasti then fprintf f "@,")
          e;
        fprintf f "@]@ }@]"
    | Ret e ->
        fprintf f "@[<hv 2>return@ ";
        pp_expr f e;
        fprintf f ";@]"
  in
  go

let pp_fn f { name; params; body; ret } =
  let open Format in
  fprintf f "@[<v 0>@[<v 2>@[<hov 2>function %s(@[<hv 0>" name;
  let lasti = List.length params - 1 in
  List.iteri
    (fun i (p, t) ->
      fprintf f "@[<hov 2>%s:@ " p;
      pp_ty f t;
      fprintf f "@]";
      if i <> lasti then fprintf f ",@ ")
    params;
  fprintf f "@])@,: @[";
  pp_ty f ret;
  fprintf f "@]@] {@,@[<v 0>";
  let lasti = List.length body - 1 in
  List.iteri
    (fun i s ->
      pp_stmt f s;
      if i <> lasti then fprintf f "@,")
    body;
  fprintf f "@]@]@ }@]"

let pp_program f { main; fns } =
  let open Format in
  fprintf f "@[<v 0>";
  List.iter
    (fun s ->
      pp_fn f s;
      fprintf f "@,")
    fns;
  fprintf f "%s();@]" main

let string_of_program p = with_buffer (fun f -> pp_program f p) 80
