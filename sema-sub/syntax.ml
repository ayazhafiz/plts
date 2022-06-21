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

type loc_ty = loc * ty

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

let any = zero @@ TAny
let never = zero @@ TNever
let string = zero @@ TString
let int = zero @@ TInt
let ttrue = zero @@ TTrue
let tfalse = zero @@ TFalse
let arrow t u = zero @@ TArrow (t, u)
let prod t u = zero @@ TProd (t, u)
let tor t u = zero @@ TOr (t, u)
let tand t u = zero @@ TAnd (t, u)
let not t = zero @@ TNot t

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

let gen_ty =
  let gen_atom =
    QCheck.Gen.(
      map zero @@ oneofl [ TAny; TNever; TString; TInt; TTrue; TFalse ])
  in
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
           (* Limit depth to 10 *)
           match min n 10 with
           | 0 -> gen_atom
           | n ->
               frequency
                 [
                   (1, gen_atom);
                   (2, map2 arrow (self (n / 2)) (self (n / 2)));
                   (2, map2 prod (self (n / 2)) (self (n / 2)));
                   (2, map2 tor (self (n / 2)) (self (n / 2)));
                   (2, map2 tand (self (n / 2)) (self (n / 2)));
                   (2, map not (self (n - 1)));
                 ]))

let shrink_ty =
  let open QCheck.Iter in
  let rec shrink2 mapper t u =
    of_list [ t; u ]
    <+> (shrink t >|= fun t' -> mapper t' u)
    <+> (shrink u >|= fun u' -> mapper t u')
  and shrink (_, t) =
    print_endline (string_of_ty @@ zero t);
    match t with
    | TAny -> return @@ zero TAny
    | TNever -> return @@ zero TNever
    | TString -> return @@ zero TString
    | TInt -> return @@ zero TInt
    | TTrue -> return @@ zero TTrue
    | TFalse -> return @@ zero TFalse
    | TArrow (t, u) -> shrink2 arrow t u
    | TProd (t, u) -> shrink2 prod t u
    | TOr (t, u) -> shrink2 tor t u
    | TAnd (t, u) -> shrink2 tand t u
    | TNot t -> shrink t >|= not
  in
  shrink

let arbitrary_ty = QCheck.make gen_ty ~print:string_of_ty
(* TODO: loops forever? ~shrink:shrink_ty *)

type cmd =
  | CTy of
      [ `Sub of loc_ty * loc_ty | `Iso of loc_ty * loc_ty | `Norm of loc_ty ]
