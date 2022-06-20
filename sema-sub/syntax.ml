type lineco = int * int
(** line * col *)

let string_of_lineco (l, c) = string_of_int l ^ ":" ^ string_of_int c

type loc = lineco * lineco
(** start * end *)

let string_of_loc (l1, l2) = string_of_lineco l1 ^ "-" ^ string_of_lineco l2
let deeper (l1, c1) (l2, c2) = l1 > l2 || (l1 = l2 && c1 >= c2)
let shallower lc1 lc2 = deeper lc2 lc1
let within (lc11, lc12) (lc21, lc22) = deeper lc11 lc21 && shallower lc12 lc22
let zero_loc : loc = ((0, 0), (0, 0))
let zero v = (zero_loc, v)
let gen_zero_loc _st = zero_loc
let gen_loc = gen_zero_loc

type loc_ty = loc * ty [@@deriving qcheck]

and ty =
  | TAny
  | TNever
  | TString
  | TInt
  | TTrue
  | TFalse
  | TArrow of loc_ty * loc_ty
  | TProd of loc_ty * loc_ty
  | TOr of loc_ty * loc_ty
  | TAnd of loc_ty * loc_ty
  | TNot of loc_ty
[@@deriving qcheck]

let rec simpl_ty (l, t) =
  let t' =
    match t with
    | TAny | TNever | TString | TInt | TTrue | TFalse -> t
    | TArrow (t1, t2) -> TArrow (simpl_ty t1, simpl_ty t2)
    | TProd (t1, t2) -> TProd (simpl_ty t1, simpl_ty t2)
    | TOr (t1, t2) -> (
        let t1, t2 = (simpl_ty t1, simpl_ty t2) in
        match (snd t1, snd t2) with
        | TNever, t | t, TNever -> t
        | TAny, _ | _, TAny -> TAny
        | _ -> TOr (t1, t2))
    | TAnd (t1, t2) -> (
        let t1, t2 = (simpl_ty t1, simpl_ty t2) in
        match (snd t1, snd t2) with
        | TNever, _ | _, TNever -> TNever
        | TAny, t | t, TAny -> t
        | _ -> TAnd (t1, t2))
    | TNot t -> (
        let t = simpl_ty t in
        match snd t with
        | TNever -> TAny
        | TAny -> TNever
        | TNot t -> snd t
        | _ -> TNot t)
  in
  (l, t')

type typrctx = [ `Free | `Arrow | `Or | `And | `Not ]

let prec_of_tprctx : typrctx -> int = function
  | `Free -> 1
  | `Arrow -> 2
  | `Or -> 3
  | `And -> 4
  | `Not -> 5

let stricter parent child = prec_of_tprctx parent > prec_of_tprctx child

let pretty s =
  match s with
  (*
  | "any" -> "\u{22A4}" (* ⊤ *)
  | "never" -> "\u{22A5}" (* ⊥ *)
  | "&" -> "\u{2227}" (* ∧ *)
  | "|" -> "\u{2228}" (* ∨ *)
  | "->" -> "\u{2192}" (* → *)
  *)
  | s -> s

let pp_ty f =
  let open Format in
  let paren doparen inside =
    if doparen then fprintf f "(";
    inside ();
    if doparen then fprintf f ")"
  in
  let rec go p (_, t) =
    match t with
    | TInt -> pp_print_string f "int"
    | TString -> pp_print_string f "string"
    | TAny -> pp_print_string f @@ pretty "any"
    | TNever -> pp_print_string f @@ pretty "never"
    | TTrue -> pp_print_string f @@ pretty "true"
    | TFalse -> pp_print_string f @@ pretty "false"
    | TProd (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren true (fun () ->
            go `Free t1;
            fprintf f ",@ ";
            go `Free t2);
        fprintf f "@]"
    | TArrow (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren (stricter p `Arrow) (fun () ->
            go `Arrow t1;
            fprintf f " ->@ ";
            go `Free t2);
        fprintf f "@]"
    | TOr (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren (stricter p `Or) (fun () ->
            go `Or t1;
            fprintf f " %s@ " (pretty "|");
            go `Or t2);
        fprintf f "@]"
    | TAnd (t1, t2) ->
        fprintf f "@[<hov 2>";
        paren (stricter p `And) (fun () ->
            go `And t1;
            fprintf f " %s@ " (pretty "&");
            go `And t2);
        fprintf f "@]"
    | TNot t ->
        fprintf f "@[!";
        paren (stricter p `Not) (fun () -> go `Not t);
        fprintf f "@]"
  in
  go `Free

let string_of_ty t = Util.with_buffer (fun f -> pp_ty f t) 80
