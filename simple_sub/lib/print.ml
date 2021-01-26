(** Module [print] pretty-prints types *)

open Language

let empty l = List.length l = 0

(** Pretty-prints a [simple_ty], the inferred type IR. *)
let string_of_sty ?(showBounds = true) ty =
  let uniq_bounds ty =
    let all = ref [] in
    let rec go = function
      | [] -> ()
      | STyVar vs :: rest ->
          if List.mem vs !all then go rest
          else (
            all := vs :: !all;
            go (vs.lower_bounds @ vs.upper_bounds @ rest) )
      | STyPrim _ :: rest -> go rest
      | STyFn (p, r) :: rest -> go (p :: r :: rest)
      | STyRecord fields :: rest -> go (List.map snd fields @ rest)
    in
    go [ ty ];
    List.sort (fun a b -> compare a.uid b.uid) !all
  in
  let rec show = function
    | STyPrim n -> n
    | STyVar { uid; level; _ } ->
        Printf.sprintf "α%s%s" (String.make level '\'') (string_of_int uid)
    | STyFn (p, r) -> Printf.sprintf "(%s -> %s)" (show p) (show r)
    | STyRecord fields ->
        List.map (fun (f, t) -> Printf.sprintf "%s: %s" f (show t)) fields
        |> String.concat ", " |> Printf.sprintf "{%s}"
  in
  let displayTy = show ty in
  if not showBounds then displayTy
  else
    let bounds =
      uniq_bounds ty
      |> List.filter (fun { lower_bounds; upper_bounds; _ } ->
             not (empty lower_bounds && empty upper_bounds))
      |> List.map (fun vs ->
             let ty = show (STyVar vs) in
             let prBs bs st mrg =
               if empty bs then ""
               else
                 Printf.sprintf " %s (%s)" st
                   (String.concat mrg (List.map show bs))
             in
             let boundsL = prBs vs.lower_bounds ":>" "|" in
             let boundsR = prBs vs.upper_bounds "<:" "&" in
             Printf.sprintf "[%s%s%s]" ty boundsL boundsR)
      |> String.concat " "
    in
    Printf.sprintf "%s\n  where %s" displayTy bounds

(** Pretty-prints a [compact_ty_scheme], the simplification type IR. *)
let string_of_cty { ty; rec_vars } =
  let string_of_sty = string_of_sty ~showBounds:false in
  let rec show = function
    | { vars; prims; rcd; fn } ->
        let vars =
          VarSet.elements vars
          |> List.map (fun vs -> STyVar vs |> string_of_sty)
        in
        let prims = StringSet.elements prims in
        let rcd =
          Option.to_list rcd
          |> List.map (fun fs ->
                 List.map (fun (f, t) -> Printf.sprintf "%s: %s" f (show t)) fs
                 |> String.concat ", " |> Printf.sprintf "{%s}")
        in
        let fn =
          Option.to_list fn
          |> List.map (fun (p, r) ->
                 Printf.sprintf "(%s -> %s)" (show p) (show r))
        in
        Printf.sprintf "⟨%s⟩" (String.concat "; " (vars @ prims @ rcd @ fn))
  in
  let displayTy = show ty in
  let recVars =
    rec_vars
    |> List.map (fun (vs, cty) ->
           Printf.sprintf "[%s :: %s]" (string_of_sty (STyVar vs)) (show cty))
    |> String.concat " "
  in
  Printf.sprintf "%s\n  recs %s" displayTy recVars

(** Pretty-prints a [ty], the final type IR. *)
let string_of_ty ty =
  let uniq_typevars ty =
    let rec go = function
      | TyTop | TyBottom | TyPrim _ -> StringSet.empty
      | TyVar v -> StringSet.singleton v
      | TyRecursive (l, r) | TyUnion (l, r) | TyIntersection (l, r) | TyFn (l, r)
        ->
          StringSet.union (go l) (go r)
      | TyRecord fields ->
          List.concat_map (fun (_, t) -> go t |> StringSet.elements) fields
          |> StringSet.of_list
    in
    go ty
  in
  let tyVars = uniq_typevars ty |> StringSet.elements in
  let base = Char.code 'a' in
  let ctx =
    List.mapi
      (fun i ty ->
        let displayTy = Printf.sprintf "'%c" (Char.chr (base + i)) in
        (ty, displayTy))
      tyVars
  in
  let parenIf cond what = if cond then Printf.sprintf "(%s)" what else what in
  let rec show prec = function
    | TyTop -> "any"
    | TyBottom -> "never"
    | TyPrim n -> n
    | TyVar v -> List.assoc v ctx
    | TyRecursive (TyVar v, ty) ->
        Printf.sprintf "μ %s. %s" (List.assoc v ctx) (show 30 ty)
    | TyRecursive (ty, _) ->
        failwith (show 0 ty ^ " is not a valid recursive type name")
    | TyUnion (l, r) ->
        Printf.sprintf "%s|%s" (show 20 l) (show 20 r) |> parenIf (prec > 20)
    | TyIntersection (l, r) ->
        Printf.sprintf "%s&%s" (show 25 l) (show 25 r) |> parenIf (prec > 25)
    | TyFn (p, r) ->
        Printf.sprintf "%s -> %s" (show 11 p) (show 10 r) |> parenIf (prec > 10)
    | TyRecord fields ->
        List.map (fun (f, t) -> Printf.sprintf "%s: %s" f (show 0 t)) fields
        |> String.concat ", " |> Printf.sprintf "{%s}"
  in
  show 0 ty

let rec string_of_term = function
  | Num n -> string_of_int n
  | Var v -> v
  | Abs (n, b) -> Printf.sprintf "(fn %s -> %s)" n (string_of_term b)
  | App (a, b) -> Printf.sprintf "(%s %s)" (string_of_term a) (string_of_term b)
  | Record fields ->
      List.map
        (fun (f, t) -> Printf.sprintf "%s: %s" f (string_of_term t))
        fields
      |> String.concat ", " |> Printf.sprintf "{%s}"
  | RecordProject (t, f) -> Printf.sprintf "%s.%s" (string_of_term t) f
  | Let { is_rec; name; rhs; body } ->
      Printf.sprintf "let%s %s = %s in %s"
        (if is_rec then " rec" else "")
        name (string_of_term rhs) (string_of_term body)
