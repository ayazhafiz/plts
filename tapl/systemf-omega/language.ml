(** File: path, line, co *)
type info = File of string * int * int | Unknown

type 'a withinfo = { i : info; v : 'a }

let dummyinfo = Unknown

let makeinfo f l c = File (f, l, c)

type kind = KnStar | KnArrow of kind * kind

type ty =
  | TyVar of int * int  (** index in context, length of context *)
  | TyFn of ty * ty  (** T -> U *)
  | TyAll of string * kind * ty  (** forall X. (T->X)*)
  | TyAbs of string * kind * ty  (** lam X. (T->X)*)
  | TyApp of ty * ty  (** X T *)

type term =
  | TmVar of info * int * int  (** index in context, length of context *)
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTyAbs of info * string * kind * term  (** lam X. lam n: X. n *)
  | TmTyApp of info * term * ty  (** (lam X. lam n: X. n) [Nat] *)

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
let tymap on_tyvar cutoff =
  let rec walk cutoff = function
    | TyVar (name, ctxlen) -> on_tyvar cutoff name ctxlen
    | TyFn (tyIn, tyOut) -> TyFn (walk cutoff tyIn, walk cutoff tyOut)
    | TyAll (name, kn, ty) -> TyAll (name, kn, walk (cutoff + 1) ty)
    | TyAbs (name, kn, ty) -> TyAbs (name, kn, walk (cutoff + 1) ty)
    | TyApp (ty1, ty2) -> TyApp (walk cutoff ty1, walk cutoff ty2)
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
    | TmTyAbs (fi, tyName, kn, body) ->
        TmTyAbs (fi, tyName, kn, walk (cutoff + 1) body)
    | TmTyApp (fi, left, right) ->
        TmTyApp (fi, walk cutoff left, on_type cutoff right)
  in
  walk cutoff

let type_shift_above delta cutoff ty =
  tymap
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

let term_shift_above delta cutoff tm =
  tmmap
    (fun info cutoff name ctxlen ->
      if name < cutoff then TmVar (info, name, ctxlen + delta)
      else TmVar (info, name + delta, ctxlen + delta))
    (type_shift_above delta) cutoff tm

let type_shift delta ty = type_shift_above delta 0 ty

let term_shift delta tm = term_shift_above delta 0 tm

let binding_shift delta = function
  | VarBind ty -> VarBind (type_shift delta ty)
  | els -> els

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
    Printf.sprintf "[[bad context: expected len %d, is %d: %s]]" ctxlen
      (ctxlength ctx) (string_of_ctx ctx)

let rec string_of_kind = function
  | KnStar -> "*"
  | KnArrow (l, r) ->
      Printf.sprintf "(%s => %s)" (string_of_kind l) (string_of_kind r)

let rec string_of_ty ctx = function
  | TyVar (name, ctxlen) -> string_of_idx ctx name ctxlen
  | TyFn (ty1, ty2) ->
      Printf.sprintf "(%s -> %s)" (string_of_ty ctx ty1) (string_of_ty ctx ty2)
  | TyAll (name, kd, ty) ->
      let name', ctx' = freshname ctx name in
      Printf.sprintf "(∀%s::%s. %s)" name' (string_of_kind kd)
        (string_of_ty ctx' ty)
  | TyAbs (name, kd, ty) ->
      let name', ctx' = freshname ctx name in
      Printf.sprintf "(λ%s::%s. %s)" name' (string_of_kind kd)
        (string_of_ty ctx' ty)
  | TyApp (ty1, ty2) ->
      Printf.sprintf "%s %s" (string_of_ty ctx ty1) (string_of_ty ctx ty2)

let rec string_of_term ctx = function
  | TmVar (_, name, ctxlen) -> string_of_idx ctx name ctxlen
  | TmAbs (_, param, ty, body) ->
      let param', ctx' = freshname ctx param in
      Printf.sprintf "(λ%s: %s. %s)" param' (string_of_ty ctx ty)
        (string_of_term ctx' body)
  | TmApp (_, left, right) ->
      Printf.sprintf "(%s %s)" (string_of_term ctx left)
        (string_of_term ctx right)
  | TmTyAbs (_, tyParam, kd, body) ->
      let tyParam', ctx' = freshname ctx tyParam in
      Printf.sprintf "(λ%s::%s. %s)" tyParam' (string_of_kind kd)
        (string_of_term ctx' body)
  | TmTyApp (_, term, ty) ->
      Printf.sprintf "%s [%s]" (string_of_term ctx term) (string_of_ty ctx ty)

let string_of_binding ctx = function
  | NameBind -> ""
  | TyVarBind kd -> Printf.sprintf " :: %s" (string_of_kind kd)
  | VarBind ty -> Printf.sprintf ": %s" (string_of_ty ctx ty)
