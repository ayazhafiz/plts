open Util
open Util.Error

(** A lambda term, using de Bruijn indeces. *)
type term =
  | TmVar of info * int * int (* length of context checked for validation *)
  | TmAbs of info * string (* name hint *) * term
  | TmApp of info * term * term

type binding = NameBinding

(*** Context ***)
type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = list.length ctx

let addbinding ctx x bind = (x, bind) :: ctx

let addname ctx name = addbinding ctx name NameBinding

let rec isNameBound ctx name =
  match ctx with
  | [] -> false
  | (cand, _) :: rest -> if cand = name then true else isNameBound rest name

(** Picks the first variable name derived from [name] that is not already bound
    in [context ctx]. *)
let rec freshname ctx name =
  if isNameBound ctx name then freshname ctx (x ^ "'")
  else ((x, NameBinding) :: ctx, x)

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
  | (cand, _) :: rest -> if y = x then 0 else 1 + name2index fi rest x

(*** Shifting ***)

(** [termShift] renumbers indices of free variables in a term when a
    substitution occurs to avoid conflation with bound variables.

    For example, consider
      shift 2 (λ. 0 1 (λ. 0 1 2))
    The initial cutoff is 0 (meaning that we are willing to rename all
    variables). As we enter an abstraction, we increase the cutoff (because
    the application introduces the de Bruijn index cuttof+1). In the example,
      shift 2 (λ. 0 1 (λ. 0 1 2))
               ^^ binds 0
                       ^^ binds 1
    Okay, so let's walk out the shifting process. Entering the first
    abstraction, we get
      (λ. 0 3 [shift c=1 2] (λ. 0 1 2))
    And now, walking out the rest of the shift in the second abstraction, we
    arrive at
      (λ. 0 3 (λ. 0 1 4))
    As you can see, we preserved the structure of the bound variables while
    permissively renaming the free variables, which we are up to doing so
    since they have no binding. Expressed as variables, maybe this would be a
    shift from
      (λa. a b (λb. a b c))
    to
      (λa. a d (λb. a b e)) *)
let termShift d term =
  (* walk c=cutoff *)
  let rec walk c term =
    match term with
    | TmVar (info, x, n) ->
        if x < c then (* Under cutoff <=> bound variable, no shift. *)
          x
        else (* Otherwise, apply the shift. *)
          k + d
    | TmAbs (info, name, term) -> TmAbs (info, name, walk (c + 1) term)
    | TmApp (info, t1, t2) -> TmApp (info, walk c t1, walk c t2)
  in
  walk 0 term

(** Applies the substitution [j->s]term *)
let termSubst j s term =
  match term with
  | TmVar (info, x, n) ->
      if x = j then (* Term indices match, apply substitution *) s
      else (* Do not apply substitution *) k
  | TmAbs (info, name, term) ->
      TmAbs (info, name, termSubst (j + 1) (termShift 1 s) term)
  | TmApp (info, t1, t2) -> TmApp (info, termSubst j s t1, termSubst j s t2)

(*** Printing ***)
let rec printtm ctx t =
  match t with
  | TmVar (info, x, n) ->
      if ctxlength ctx = n then pr (index2name info ctx x)
      else pr "[bad index - evaluation is incorrect!]"
  | TmAbs (info, name, term) ->
      let ctx', name' = freshname ctx name in
      pr "(λ ";
      pr name';
      pr ". ";
      printtm ctx' term;
      pr ")"
  | TmApp (info, t1, t2) ->
      pr "(";
      printtm ctx t1;
      pr " ";
      printtm ctx t2;
      pr ")"
