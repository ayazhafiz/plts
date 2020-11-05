open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty = TyArr of ty * ty | TyBool

type term =
  | TmVar of info * int * int
  | TmLet of info * string * term * term
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term

type binding = NameBind | VarBind of ty

type context = (string * binding) list

type command = Eval of info * term | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x, bind) :: ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
  | [] -> false
  | (y, _) :: rest -> if y = x then true else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x ^ "'")
  else ((x, NameBind) :: ctx, x)

let index2name fi ctx x =
  try
    let xn, _ = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
  | [] -> error fi ("Identifier " ^ x ^ " is unbound")
  | (y, _) :: rest -> if y = x then 0 else 1 + name2index fi rest x

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tmmap onvar c t =
  let rec walk c t =
    match t with
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmLet (fi, x, t1, t2) -> TmLet (fi, x, walk c t1, walk (c + 1) t2)
    | TmAbs (fi, x, tyT1, t2) -> TmAbs (fi, x, tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmTrue _ as t -> t
    | TmFalse _ as t -> t
    | TmIf (fi, t1, t2, t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
  in
  walk c t

let termShiftAbove d c t =
  tmmap
    (fun fi c x n ->
      if x >= c then TmVar (fi, x + d, n + d) else TmVar (fi, x, n + d))
    c t

let termShift d t = termShiftAbove d 0 t

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap (fun fi j x n -> if x = j then termShift j s else TmVar (fi, x, n)) j t

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let getbinding fi ctx i =
  try
    let _, bind = List.nth ctx i in
    bind
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in
    error fi (msg i (List.length ctx))

let getTypeFromContext fi ctx i =
  match getbinding fi ctx i with
  | VarBind tyT -> tyT
  | _ ->
      error fi
        ( "getTypeFromContext: Wrong kind of binding for variable "
        ^ index2name fi ctx i )

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t =
  match t with
  | TmVar (fi, _, _) -> fi
  | TmLet (fi, _, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi, _, _, _) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0 () = open_hvbox 0

let obox () = open_hvbox 2

let cbox () = close_box ()

let break () = print_break 0 0

let small t = match t with TmVar (_, _, _) -> true | _ -> false

let rec printty_Type outer tyT =
  match tyT with tyT -> printty_ArrowType outer tyT

and printty_ArrowType outer tyT =
  match tyT with
  | TyArr (tyT1, tyT2) ->
      obox0 ();
      printty_AType false tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space () else break ();
      printty_ArrowType outer tyT2;
      cbox ()
  | tyT -> printty_AType outer tyT

and printty_AType outer tyT =
  match tyT with
  | TyBool -> pr "Bool"
  | tyT ->
      pr "(";
      printty_Type outer tyT;
      pr ")"

let printty tyT = printty_Type true tyT

let rec printtm_Term outer ctx t =
  match t with
  | TmLet (_, x, t1, t2) ->
      obox0 ();
      pr "let ";
      pr x;
      pr " = ";
      printtm_Term false ctx t1;
      print_space ();
      pr "in";
      print_space ();
      printtm_Term false (addname ctx x) t2;
      cbox ()
  | TmAbs (_, x, tyT1, t2) ->
      let ctx', x' = pickfreshname ctx x in
      obox ();
      pr "lambda ";
      pr x';
      pr ":";
      printty_Type false tyT1;
      pr ".";
      if small t2 && not outer then break () else print_space ();
      printtm_Term outer ctx' t2;
      cbox ()
  | TmIf (_, t1, t2, t3) ->
      obox0 ();
      pr "if ";
      printtm_Term false ctx t1;
      print_space ();
      pr "then ";
      printtm_Term false ctx t2;
      print_space ();
      pr "else ";
      printtm_Term false ctx t3;
      cbox ()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t =
  match t with
  | TmApp (_, t1, t2) ->
      obox0 ();
      printtm_AppTerm false ctx t1;
      print_space ();
      printtm_ATerm false ctx t2;
      cbox ()
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t =
  match t with
  | TmVar (fi, x, n) ->
      if ctxlength ctx = n then pr (index2name fi ctx x)
      else
        pr
          ( "[bad index: " ^ string_of_int x ^ "/" ^ string_of_int n ^ " in {"
          ^ List.fold_left (fun s (x, _) -> s ^ " " ^ x) "" ctx
          ^ " }]" )
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | t ->
      pr "(";
      printtm_Term outer ctx t;
      pr ")"

let printtm ctx t = printtm_Term true ctx t

let prbinding _ b =
  match b with
  | NameBind -> ()
  | VarBind tyT ->
      pr ": ";
      printty tyT
