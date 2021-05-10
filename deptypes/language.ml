(** File: path, line, co *)
type info = File of string * int * int | Unknown

type 'a withinfo = { i : info; v : 'a }

let dummyinfo = Unknown

let makeinfo f l c = File (f, l, c)

type ty =
  | TyVar of int * int  (** index in context, length of context *)
  | TyPi of string * ty * ty  (** Pi x: T. U *)
  | TyPiApp of ty * term  (** T t *)

and term =
  | TmVar of info * int * int  (** index in context, length of context *)
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term

type kind = KnStar | KnPi of string * ty * kind  (** Pi x: T. Kn *)

type binding = NameBind | TyVarBind of kind | VarBind of ty

type command = Eval of info * term | Bind of info * string * binding

(*         *)
(* Context *)
(*         *)

type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx name bind = (name, bind) :: ctx

let addname ctx name = addbinding ctx name NameBind

let isnamebound ctx name = List.mem_assoc name ctx

let rec freshname ctx name =
  if isnamebound ctx name then freshname ctx (name ^ "'")
  else (name, addname ctx name)

let index2name ctx i =
  try
    let name, _ = List.nth ctx i in
    name
  with Failure s ->
    failwith
      (Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d.\n%s" i
         (ctxlength ctx) s)

let rec name2index ctx name =
  match ctx with
  | [] -> failwith (Printf.sprintf "Identifier %s is unbound" name)
  | (cand, _) :: rest -> if cand = name then 0 else 1 + name2index rest name

(*          *)
(* Shifting *)
(*          *)

(* Common method for mapping over types, for use in both shifting and
   substitution algorithms. *)
let tymap on_var on_tyvar cutoff =
  let rec walk cutoff = function
    | TyVar (name, ctxlen) -> on_tyvar cutoff name ctxlen
    | TyPi (name, varTy, rTy) ->
        TyPi (name, walk cutoff varTy, walk (cutoff + 1) rTy)
    | TyPiApp (ty1, tm) -> TyPiApp (walk cutoff ty1, on_var cutoff tm)
  in
  walk cutoff

(* Common method for mapping over terms, for use in both shifting and
   substitution algorithms. *)
let tmmap on_var on_type cutoff =
  let rec walk cutoff = function
    | TmVar (fi, name, ctxlen) -> on_var fi cutoff name ctxlen
    | TmAbs (fi, param, tyIn, body) ->
        TmAbs (fi, param, on_type cutoff tyIn, walk (cutoff + 1) body)
    | TmApp (fi, left, right) -> TmApp (fi, walk cutoff left, walk cutoff right)
  in
  walk cutoff

let knmap on_type cutoff =
  let rec walk cutoff = function
    | KnStar -> KnStar
    | KnPi (v, vTy, rKn) -> KnPi (v, on_type cutoff vTy, walk (cutoff + 1) rKn)
  in
  walk cutoff

let rec type_shift_above delta cutoff ty =
  tymap (term_shift_above delta)
    (fun cutoff name ctxlen ->
      (* No shift *)
      if name < cutoff then TyVar (name, ctxlen + delta)
        (* The shift is "okay", meaning that we won't be trying to refer to things
           not in scope anymore. *)
      else if name + delta >= 0 then TyVar (name + delta, ctxlen + delta)
      else
        failwith
          (Printf.sprintf "Scoping error; type not visible in outer scope."))
    cutoff ty

and term_shift_above delta cutoff tm =
  tmmap
    (fun info cutoff name ctxlen ->
      if name < cutoff then TmVar (info, name, ctxlen + delta)
      else TmVar (info, name + delta, ctxlen + delta))
    (type_shift_above delta) cutoff tm

let type_shift delta ty = type_shift_above delta 0 ty

let term_shift delta tm = term_shift_above delta 0 tm

let kind_shift_above delta cutoff kn = knmap (type_shift_above delta) cutoff kn

let kind_shift delta kn = kind_shift_above delta 0 kn

let binding_shift delta = function
  | NameBind -> NameBind
  | VarBind ty -> VarBind (type_shift delta ty)
  | TyVarBind kn -> TyVarBind (kind_shift delta kn)

(*              *)
(* Substitution *)
(*              *)

let term_subst j s tm =
  tmmap
    (fun fi cutoff name ctxlen ->
      if j + cutoff = name then term_shift cutoff s else TmVar (fi, name, ctxlen))
    (fun _ ty -> ty)
    0 tm

let term_subst_top s tm = term_shift (-1) (term_subst 0 (term_shift 1 s) tm)

let type_subst j s ty =
  tymap
    (fun _ tm -> tm)
    (fun cutoff name ctxlen ->
      if j + cutoff = name then type_shift (j + cutoff) s
      else TyVar (name, ctxlen))
    0 ty

let type_subst_top s ty = type_shift (-1) (type_subst 0 (type_shift 1 s) ty)

let tyterm_subst j tyS tm =
  tmmap
    (fun fi _ name ctxlen -> TmVar (fi, name, ctxlen))
    (fun cutoff ty -> type_subst (j + cutoff) tyS ty)
    0 tm

let tyterm_subst_top tyS tm =
  term_shift (-1) (tyterm_subst 0 (type_shift 1 tyS) tm)

(** Substitutes a top-level term into a dependent type. *)
let termty_subst j tmS ty =
  tymap
    (fun _ tm -> term_subst j tmS tm)
    (fun _ name ctxlen -> TyVar (name, ctxlen))
    0 ty

let termty_subst_top tmS ty =
  type_shift (-1) (termty_subst 0 (term_shift 1 tmS) ty)

(** Substitutes kinds *)
let kind_subst j tmS kn =
  knmap (fun cutoff ty -> termty_subst (j + cutoff) tmS ty) 0 kn

let kind_subst_top tmS kn = kind_shift (-1) (kind_subst 0 (term_shift 1 tmS) kn)

let getbinding ctx i =
  (* References in the binding to other items in the context are those that were
     introduced before the binding was added to the context. Since the context is
     now at index i, we should account for the "staled" references by shifting
     the references by "i", and +1 to account for the actual insertion of the
     presenting binding. *)
  try
    let _, bind = List.nth ctx i in
    binding_shift (i + 1) bind
  with Failure s ->
    failwith
      (Printf.sprintf "Variable lookup failed: %d, ctx size: %d\n%s" i
         (ctxlength ctx) s)

let gettype ctx i =
  match getbinding ctx i with
  | VarBind ty -> ty
  | _ ->
      failwith
        (Printf.sprintf "Wrong kind of binding for variable %s"
           (index2name ctx i))

let getkind ctx i =
  match getbinding ctx i with
  | TyVarBind kn -> kn
  | _ ->
      failwith
        (Printf.sprintf "Wrong kind of binding for variable %s"
           (index2name ctx i))

(*                    *)
(* Printing utilities *)
(*                    *)

let string_of_ctx ctx =
  List.map
    (function
      | name, bind ->
          Printf.sprintf "(%s, %s)" name
            ( match bind with
            | NameBind -> "name"
            | VarBind _ -> "var"
            | TyVarBind _ -> "tyVar" ))
    ctx
  |> String.concat "; "
  |> function
  | ctx -> "\n\t{" ^ ctx ^ "}"

let string_of_idx ctx name ctxlen =
  if ctxlength ctx = ctxlen then Printf.sprintf "%s" (index2name ctx name)
  else
    Printf.sprintf "[[bad context (%d): expected len %d, is %d: %s]]" name
      ctxlen (ctxlength ctx) (string_of_ctx ctx)

let rec string_of_kind ctx = function
  | KnStar -> "*"
  | KnPi (termVar, varTy, rKn) ->
      let termVar', ctx' = freshname ctx termVar in
      Printf.sprintf "(Π%s: %s. %s)" termVar' (string_of_ty ctx varTy)
        (string_of_kind ctx' rKn)

and string_of_ty ctx = function
  | TyVar (name, ctxlen) -> string_of_idx ctx name ctxlen
  | TyPi (termVar, varTy, rTy) ->
      let termVar', ctx' = freshname ctx termVar in
      Printf.sprintf "(Π%s: %s. %s)" termVar' (string_of_ty ctx varTy)
        (string_of_ty ctx' rTy)
  | TyPiApp (ty1, ty2) ->
      Printf.sprintf "%s %s" (string_of_ty ctx ty1) (string_of_term ctx ty2)

and string_of_term ctx = function
  | TmVar (_, name, ctxlen) -> string_of_idx ctx name ctxlen
  | TmAbs (_, param, ty, body) ->
      let param', ctx' = freshname ctx param in
      Printf.sprintf "(λ%s: %s. %s)" param' (string_of_ty ctx ty)
        (string_of_term ctx' body)
  | TmApp (_, left, right) ->
      Printf.sprintf "(%s %s)" (string_of_term ctx left)
        (string_of_term ctx right)

let string_of_binding ctx = function
  | NameBind -> ""
  | TyVarBind kd -> Printf.sprintf " :: %s" (string_of_kind ctx kd)
  | VarBind ty -> Printf.sprintf ": %s" (string_of_ty ctx ty)
