open Util
open Util.Error

(** Types *)
type ty =
  (* A type variable present in the context. First parameter is the index of the
     type name in the context, second parameter is the length of the context at
     the time of variable addition, checked during printing for validation. *)
  | TyVar of int * int
  (* An unbound type identifier. *)
  | TyId of string
  (* A function type, T -> T *)
  | TyArrow of ty * ty
  (* A record type like {a: Nat, b: String}. Contains both a and b. *)
  | TyRecord of (string * ty) list
  (* A variant type like <a: Nat, b: String>. Contains either a or b. *)
  | TyVariant of (string * ty) list
  | TyList of ty
  | TyRef of ty
  | TyString
  | TyUnit
  | TyBool
  | TyFloat
  | TyNat

(** A lambda term, using de Bruijn indeces. *)
type term =
  (* A variable "x" *)
  | TmVar of info * int (* deBruijn index *) * int (* length of context checked for validation *)
  (* A function definition "λx: T. <definition>" *)
  | TmAbs of info * string (* name hint *) * ty * term
  (* A function application "(λx: T. <definition>)(y)" *)
  | TmApp of info * term * term
  (* A condition/then/else expression *)
  | TmIf of info * term * term * term
  (* case <term> (| <str = str> ==> <term>)* *)
  | TmCase of info * term * (string * (string * term)) list
  (* A tag of variant type. For example "<cat = catTerm> as Animal" is a tag,
     where "Animal" is defined as "<cat: Cat, dog: Dog>".*)
  | TmTag of info * string * term * ty
  (* let <name> = t1 in <t2> *)
  | TmLet of info * string * term * term
  (* Applying "fix" on a term returns a fixed point on that term.
     More specifically, fix is a combinator that provides its input back to
     itself.
     For example if we have a function
       [ fact := λfact: Nat->Nat. λn:Nat. fact(pred n) ]
     "fix(fact)" would give to us a function that "feeds fact back to itself",
     providing "fact" as fixed point. Therefore "fact" can now be evaluated on a
     fixed point, and so while "fact" is "(Nat -> Nat) -> Nat -> Nat",
     "fix(fact)" is just "Nat -> Nat".
     Another intuitive way to think about this is that "fix(fact)" has
     "everything it needs" for the recursive definition of "fact" to work; that
     is, a user of "fix(fact)" no longer needs to supply the definition of
     "fact", and can just use it as a definition of factorial directly! That is
     the meaning behind a fixed point -- it is provided, and thus fixed. *)
  | TmFix of info * term
  (* term as ty *)
  | TmAscribe of info * term * ty
  (* {a=true, b=false} or a tuple {true, false} *)
  | TmRecord of info * (string * term) list
  (* {a=true, b=false}.b
     ^^^^^^^^^^^^^^^^^--- term
                       ^- string *)
  | TmProj of info * term * string
  (* inert [ Ty ] *)
  | TmInert of info * ty
  | TmTrue of info
  | TmFalse of info
  | TmString of info * string
  | TmUnit of info
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmPlusfloat of info * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmNil of info * ty
  | TmCons of info * term * term
  | TmIsNil of info * term
  | TmHead of info * term
  | TmTail of info * term
  | TmRef of info * term
  | TmDeref of info * term
  (* a := b *)
  | TmRefAssign of info * term * term
  (* A location of a reference in a store. *)
  | TmLoc of info * int

type binding =
  (* A named, but untyped variable. *)
  | NameBinding
  (* A named, but untyped type variable. *)
  | TyVarBinding
  (* A typed variable, for example "let x: bool" *)
  | VarBinding of ty
  (* A variable bound by another term, whose type may or may not be known.
     For example, "let x = true" is bound by the term "true", whose type may be
     inferred but is not explicitly known. *)
  | TmAbbBinding of term * ty option
  (* A type variable bound to a type.
     For example, "T = Nat -> Nat" is bound by the type "Nat -> Nat". *)
  | TyAbbBinding of ty

(** Get term info *)
let tmInfo t =
  match t with
  | TmVar (info, _, _) -> info
  | TmAbs (info, _, _, _) -> info
  | TmApp (info, _, _) -> info
  | TmIf (info, _, _, _) -> info
  | TmCase (info, _, _) -> info
  | TmTag (info, _, _, _) -> info
  | TmLet (info, _, _, _) -> info
  | TmFix (info, _) -> info
  | TmAscribe (info, _, _) -> info
  | TmRecord (info, _) -> info
  | TmProj (info, _, _) -> info
  | TmInert (info, _) -> info
  | TmTrue info -> info
  | TmFalse info -> info
  | TmString (info, _) -> info
  | TmUnit info -> info
  | TmFloat (info, _) -> info
  | TmTimesfloat (info, _, _) -> info
  | TmPlusfloat (info, _, _) -> info
  | TmZero info -> info
  | TmSucc (info, _) -> info
  | TmPred (info, _) -> info
  | TmIsZero (info, _) -> info
  | TmNil (info, _) -> info
  | TmCons (info, _, _) -> info
  | TmIsNil (info, _) -> info
  | TmHead (info, _) -> info
  | TmTail (info, _) -> info
  | TmRef (info, _) -> info
  | TmDeref (info, _) -> info
  | TmRefAssign (info, _, _) -> info
  | TmLoc (info, _) -> info

(*** Commands ***)
type command = Eval of info * term | Bind of info * string * binding

(*** Shifting and Substitution ***)

(* Common method for mapping over types, for use in both shifting and
   substitution algorithms. *)
let tyMap ontype cutoff ty =
  let rec walk cutoff ty =
    match ty with
    | TyVar (name, ctxlen) -> ontype cutoff name ctxlen
    | TyId _ as t -> t
    | TyList ty -> TyList (walk cutoff ty)
    | TyRecord fields ->
        TyRecord (List.map (fun (name, ty) -> (name, walk cutoff ty)) fields)
    | TyVariant opts ->
        TyVariant (List.map (fun (name, ty) -> (name, walk cutoff ty)) opts)
    | TyArrow (ty1, ty2) -> TyArrow (walk cutoff ty1, walk cutoff ty2)
    | TyRef ty -> TyRef (walk cutoff ty)
    | TyString -> TyString
    | TyUnit -> TyUnit
    | TyFloat -> TyFloat
    | TyNat -> TyNat
    | TyBool -> TyBool
  in
  walk cutoff ty

(* Common method for mapping over terms, for use in both shifting and
   substitution algorithms. *)
let tmMap onvar ontype cutoff term =
  let rec walk cutoff term =
    match term with
    | TmVar (info, name, ctxlen) -> onvar info cutoff name ctxlen
    | TmAbs (info, name, ty, term) ->
        let ty' = ontype cutoff ty in
        let term' = walk (cutoff + 1) term in
        TmAbs (info, name, ty', term')
    | TmApp (info, t1, t2) ->
        let t1' = walk cutoff t1 in
        let t2' = walk cutoff t2 in
        TmApp (info, t1', walk cutoff t2')
    | TmIf (info, cond, thn, els) ->
        let cond' = walk cutoff cond in
        let thn' = walk cutoff thn in
        let els' = walk cutoff els in
        TmIf (info, cond', thn', els')
    | TmCase (info, cond, cases) ->
        let cond' = walk cutoff cond in
        let cases' =
          List.map
            (fun (left, (right, evalTerm)) ->
              (* Entering abstraction (a new name induced by right) so
                 increase cutoff *)
              (left, (right, walk (cutoff + 1) evalTerm)))
            cases
        in
        TmCase (info, cond', cases')
    | TmTag (info, name, value, ty) ->
        let value' = walk cutoff value in
        let ty' = ontype cutoff ty in
        TmTag (info, name, value', ty')
    | TmLet (info, name, lhs, rhs) ->
        let lhs' = walk cutoff lhs in
        (* New name induced by lhs, so increase cutoff *)
        let rhs' = walk (cutoff + 1) rhs in
        TmLet (info, name, lhs', rhs')
    | TmFix (info, term) -> TmFix (info, walk cutoff term)
    | TmAscribe (info, term, ty) ->
        TmAscribe (info, walk cutoff term, ontype cutoff ty)
    | TmRecord (info, fields) ->
        let fields' =
          List.map (fun (name, value) -> (name, walk cutoff value)) fields
        in
        TmRecord (info, fields')
    | TmProj (info, term, name) -> TmProj (info, walk cutoff term, name)
    | TmInert (info, ty) -> TmInert (info, ontype cutoff ty)
    | TmUnit _ as t -> t
    | TmTrue _ as t -> t
    | TmFalse _ as t -> t
    | TmString _ as t -> t
    | TmFloat _ as t -> t
    | TmTimesfloat (info, t1, t2) ->
        TmTimesfloat (info, walk cutoff t1, walk cutoff t2)
    | TmPlusfloat (info, t1, t2) ->
        TmPlusfloat (info, walk cutoff t1, walk cutoff t2)
    | TmZero _ as t -> t
    | TmSucc (info, term) -> TmSucc (info, walk cutoff term)
    | TmPred (info, term) -> TmPred (info, walk cutoff term)
    | TmIsZero (info, term) -> TmIsZero (info, walk cutoff term)
    | TmNil (info, ty) -> TmNil (info, ontype cutoff ty)
    | TmCons (info, t1, t2) -> TmCons (info, walk cutoff t1, walk cutoff t2)
    | TmIsNil (info, term) -> TmIsNil (info, walk cutoff term)
    | TmHead (info, term) -> TmHead (info, walk cutoff term)
    | TmTail (info, term) -> TmTail (info, walk cutoff term)
    | TmRef (info, term) -> TmRef (info, walk cutoff term)
    | TmDeref (info, term) -> TmDeref (info, walk cutoff term)
    | TmRefAssign (info, t1, t2) ->
        TmRefAssign (info, walk cutoff t1, walk cutoff t2)
    | TmLoc (info, where) -> TmLoc (info, where)
  in
  walk cutoff term

let typeShiftAbove d cutoff ty =
  tyMap
    (fun cutoff name ctxlen ->
      (* Over cutoff <=> unbound variable, make a shift.
         In either case we update the context length under which this variable
         is present, because we are asked to shift by "d" spaces, so "d"
         variables have been introduced to the context and thus we are "d"
         spaces ahead in the context. *)
      if name >= cutoff then TyVar (name + d, ctxlen + d)
        (* Under cutoff <=> bound variable, no shift. *)
      else TyVar (name, ctxlen + d))
    cutoff ty

let termShiftAbove d cutoff term =
  tmMap
    (fun info cutoff name ctxlen ->
      (* Over cutoff <=> unbound variable, no shift. *)
      if name >= cutoff then TmVar (info, name + d, ctxlen + d)
        (* Under cutoff <=> bound variable, no shift. *)
      else TmVar (info, name, ctxlen + d))
    (typeShiftAbove d) cutoff term

let typeShift d ty = typeShiftAbove d 0 ty

let termShift d term = termShiftAbove d 0 term

let bindingShift d binding =
  match binding with
  | NameBinding -> NameBinding
  | TyVarBinding -> TyVarBinding
  | VarBinding ty -> VarBinding (typeShift d ty)
  | TmAbbBinding (term, ty) ->
      let term' = termShift d term in
      let ty' =
        match ty with None -> None | Some ty -> Some (typeShift d ty)
      in
      TmAbbBinding (term', ty')
  | TyAbbBinding ty -> TyAbbBinding (typeShift d ty)

(** Applies the substitution [j->s]term *)
let termSubst j s term =
  tmMap
    (fun info cutoff name ctxlen ->
      if name = j + cutoff then
        (* We've "walked" to the variable we want to substitute. This is because
           by doing "j + cutoff", we account for the variable name in the
           outermost scope where we started (j) and the shifts in the name
           introduced by entering abstractions (cutoff).

           Now the idea is to give back s, accounting for what we would have had
           to shift in s up to this point (which is namely the cutoff). We have
           to do this because tmMap will _not_ shift s when entering
           abstractions while walking "term"! But the number of abstractions
           we have entered is represented by cutoff, so as mentioned, we can
           apply them all at once here.

           We need to shift s in the first place so that free variables in s
           continue referring to the same variables they did before, since
           everytime we enter an abstraction we shift all free variables up by
           one.

           For example consider substituting [0->3] (λ. 0 1 (λ. 0 1 2)). Clearly
           we can't this directly, because 0 is bound twice twice (per each
           lambda abstraction). But the free variable `0` in the global scope is
           the free variable `1` in the leftmost abstraction and the free
           variable `2` in the rightmost abstraction (because we introduce the
           abstraction binding variables as new variables when entering the
           abstraction). So we need to shift `3` up when replacing as well to
           ensure we still refer to the `3` in global scope after entering an
           abstraction. Thus, we would end up with (λ. 0 4 (λ. 0 1 5)).
        *)
        termShift cutoff s
      else
        (* This is not what we're looking for, apply no substitution. *)
        TmVar (info, name, ctxlen))
    (fun _cutoff ty -> ty) (* We never perform type substitution. *)
    0 term

(** "Top level" beta reduction of an abstraction, for example (λ0. 0 1)(0).
    We evaluate the body of abstraction with the substitution directly; that is,
    this effectively becomes [0->0'](0 1), where 0' is the (0) being applied.

    But clearly we can not do [0->0](0 1) directly, as then we would have
    (0 1), which is incorrect:
      - "1" is unbound on the global scope; we really want this to be "0" now
        since we have destroyed the variable "0" previously bound by the
        abstraction, and now want to regress to the "0" in global scope.
    So, we need to shift the evaluated term down by one to accout for the
    bound variable we have destroyed. But doing so immediately would give us
    (-1 0), which not has the problem that -1 is an invalid de Bruijn index. To
    avoid this, we do the following:
      1. Shift up the term to be substituted by one, since the size of its
         context (bound variables) is one less than that of the body we are
         substituting it in.
      2. Perform the substitution
      3. Shift the evaluated substitution down by one to account for the
         destroyed bound variable.

    Running with our previous example of (λ0. 0 1)(0), we get
      => [0->(shift 1 Term(0))](0 1)  { definition of application }
      => [0->1](0 1)                  { shift of substitution term }
      => (1 1)                        { substitution }
      => (0 0)                        { lowering of evaluated term }
    as expected. *)
let termSubstTop s term = termShift (-1) (termSubst 0 (termShift 1 s) term)

(** Applies the substitution [j->s]ty.
    See [ termSubst ] for the logic used here. *)
let typeSubst j s tyTerm =
  tyMap
    (fun cutoff tyName ctxlen ->
      if tyName = j + cutoff then typeShift cutoff s else TyVar (tyName, ctxlen))
    0 tyTerm

(** "Top level" beta reduction of a type, [0->s](ty).
    See [ termSubstTop ] for the logic used here. *)
let typeSubstTop s tyTerm = typeShift (-1) (typeSubst 0 (typeShift 1 s) tyTerm)

(*** Context ***)
type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x, bind) :: ctx

let addname ctx name = addbinding ctx name NameBinding

let rec isNameBound ctx name =
  match ctx with
  | [] -> false
  | (cand, _) :: rest -> if cand = name then true else isNameBound rest name

(** Picks the first variable name derived from [name] that is not already bound
    in [context ctx]. *)
let rec freshname ctx name =
  if isNameBound ctx name then freshname ctx (name ^ "'")
  else ((name, NameBinding) :: ctx, name)

let index2name info ctx x =
  try
    let xn, _ = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset %d, ctx size: %d"
    in
    error info (msg x (List.length ctx))

let rec name2index info ctx x =
  match ctx with
  | [] -> error info ("Identifier " ^ x ^ " is unbound")
  | (cand, _) :: rest -> if cand = x then 0 else 1 + name2index info rest x

let getbinding info ctx i =
  try
    let _, binding = List.nth ctx i in
    (* Before we added a binding to the context, its reference to other items in
       the context were those of when the binding was not yet in the context!
       This means that our references to items in the context are now stale, and
       we should update them by shifting up the number of spots the binding we
       are interested in is in the context currently, and an extra spot to
       account for the fact that the binding was created referencing the context
       without its presence. *)
    bindingShift (i + 1) binding
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in
    error info (msg i (List.length ctx))

(** Returns the bound type of a variable. *)
let getTermType info ctx i =
  match getbinding info ctx i with
  | VarBinding ty | TmAbbBinding (_, Some ty) -> ty
  | TmAbbBinding (_, None) ->
      error info
        (index2name info ctx i ^ " has an abstraction binding but no known type")
  | _ ->
      error info
        ( "getTermType: untyped or wrong kind of binding for term "
        ^ index2name info ctx i )

(** Returns the bound name of a type variable, or [ None ] otherwise. *)
let getBoundNameOfTypeVar info ctx i =
  match getbinding info ctx i with
  | TyVarBinding -> None
  | TyAbbBinding ty -> Some ty
  | _ ->
      error info
        ( "getBoundNameOfTypeVar: Wrong kind of binding for type "
        ^ index2name info ctx i )

(*** Printing ***)
let prIndexName info ctx name ctxlen =
  if ctxlength ctx = ctxlen then pr (index2name info ctx name)
  else
    pr
      ( "[bad index: " ^ string_of_int name ^ "/" ^ string_of_int ctxlen
      ^ " in {"
      ^ List.fold_left (fun s (x, _) -> s ^ " " ^ x) "" ctx
      ^ " }]" )

(** Prints a type *)
let rec printty ctx ty =
  let prfield ctx name ty =
    pr name;
    pr ":";
    printty ctx ty
  in

  let rec prfields fields =
    match fields with
    | [] -> ()
    | [ (name, ty) ] -> prfield ctx name ty
    | (name, ty) :: rest ->
        prfield ctx name ty;
        pr ", ";
        prfields rest
  in

  match ty with
  | TyVar (name, ctxlen) -> prIndexName dummyinfo ctx name ctxlen
  | TyId ty -> pr ty
  | TyArrow (f, t) ->
      printty ctx f;
      pr " -> ";
      printty ctx t
  | TyRecord fields ->
      pr "{";
      prfields fields;
      pr "}"
  | TyVariant fields ->
      pr "<";
      prfields fields;
      pr ">"
  | TyList ty ->
      pr "[";
      printty ctx ty;
      pr "]"
  | TyRef ty ->
      pr "Ref ";
      printty ctx ty
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyBool -> pr "bool"
  | TyFloat -> pr "Float"
  | TyNat -> pr "Nat"

(** Prints a term *)
let rec printtm ctx t =
  let prfield ctx name term =
    pr name;
    pr "=";
    printtm ctx term
  in

  let rec prfields fields =
    match fields with
    | [] -> ()
    | [ (name, term) ] -> prfield ctx name term
    | (name, term) :: rest ->
        prfield ctx name term;
        pr ", ";
        prfields rest
  in

  match t with
  | TmVar (info, name, ctxlen) -> prIndexName info ctx name ctxlen
  | TmAbs (_, name, ty, term) ->
      let ctx', name' = freshname ctx name in
      pr "(λ ";
      pr name';
      pr ": ";
      printty ctx ty;
      pr ". ";
      printtm ctx' term;
      pr ")"
  | TmApp (_, t1, t2) ->
      pr "(";
      printtm ctx t1;
      pr " <- ";
      printtm ctx t2;
      pr ")"
  | TmIf (_, cond, thn, els) ->
      pr "if ";
      printtm ctx cond;
      pr " then ";
      printtm ctx thn;
      pr " else ";
      printtm ctx els
  | TmCase (_, term, cases) ->
      pr "case ";
      printtm ctx term;
      pr " of ";
      let printcase lhs rhs thn =
        let ctx', rhs' = freshname ctx rhs in
        pr "<";
        pr lhs;
        pr "=";
        pr rhs';
        pr "> ==> ";
        printtm ctx' thn
      in
      let rec printcases cases =
        match cases with
        | [] -> ()
        | (lhs, (rhs, thn)) :: rest ->
            pr "| ";
            printcase lhs rhs thn;
            printcases rest
      in
      printcases cases
  | TmTag (_, name, term, ty) ->
      pr "<";
      pr name;
      pr "=";
      printtm ctx term;
      pr ">";
      pr " as ";
      printty ctx ty
  | TmLet (_, name, expr, inTerm) ->
      pr "let ";
      pr name;
      pr " = ";
      printtm ctx expr;
      pr " in ";
      printtm (addname ctx name) inTerm
  | TmFix (_, term) ->
      pr "fix ";
      printtm ctx term
  | TmAscribe (_, term, ty) ->
      pr "(";
      printtm ctx term;
      pr " as ";
      printty ctx ty;
      pr ")"
  | TmRecord (_, fields) ->
      pr "{";
      prfields fields;
      pr "}"
  | TmProj (_, term, key) ->
      printtm ctx term;
      pr ".";
      pr key
  | TmInert (_, ty) ->
      pr "inert[";
      printty ctx ty;
      pr "]"
  | TmTrue _ -> pr "true"
  | TmFalse _ -> pr "false"
  | TmString (_, s) -> pr s
  | TmUnit _ -> pr "unit"
  | TmFloat (_, f) -> pr (string_of_float f)
  | TmTimesfloat (_, f1, f2) ->
      pr "(timesfloat ";
      printtm ctx f1;
      pr " ";
      printtm ctx f2;
      pr ")"
  | TmPlusfloat (_, f1, f2) ->
      pr "(plusfloat ";
      printtm ctx f1;
      pr " ";
      printtm ctx f2;
      pr ")"
  | TmZero _ -> pr "0"
  | TmSucc (_, term) ->
      let rec acc n term =
        match term with
        | TmZero _ -> pr (string_of_int n)
        | TmSucc (_, inner) -> acc (n + 1) inner
        | _ ->
            pr "(succ ";
            printtm ctx term;
            pr ")"
      in
      acc 1 term
  | TmPred (_, term) ->
      pr "(pred ";
      printtm ctx term;
      pr ")"
  | TmIsZero (_, term) ->
      pr "(iszero ";
      printtm ctx term;
      pr ")"
  | TmNil (_, ty) ->
      pr "nil";
      printty ctx ty
  | TmCons (_, t1, t2) ->
      pr "(cons ";
      printtm ctx t1;
      pr " ";
      printtm ctx t2;
      pr ")"
  | TmIsNil (_, term) ->
      pr "(isnil ";
      printtm ctx term;
      pr ")"
  | TmHead (_, term) ->
      pr "(head ";
      printtm ctx term;
      pr ")"
  | TmTail (_, term) ->
      pr "(tail ";
      printtm ctx term;
      pr ")"
  | TmRef (_, term) ->
      pr "ref ";
      printtm ctx term
  | TmDeref (_, term) ->
      pr "!";
      printtm ctx term
  | TmRefAssign (_, t1, t2) ->
      printtm ctx t1;
      pr " := ";
      printtm ctx t2
  | TmLoc (_, where) ->
      pr "<loc # ";
      pr (string_of_int where);
      pr ">"

let printbinding ctx b =
  match b with
  | NameBinding -> ()
  | TyVarBinding -> ()
  | VarBinding ty ->
      pr ": ";
      printty ctx ty
  | TmAbbBinding (term, None) ->
      pr "= ";
      printtm ctx term
  | TmAbbBinding (term, Some ty) ->
      pr ": ";
      printty ctx ty;
      pr " = ";
      printtm ctx term
  | TyAbbBinding ty ->
      pr "= ";
      printty ctx ty
