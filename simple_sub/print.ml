open Language

let uniq_typevars ty =
  let rec go = function
    | TyTop | TyBottom | TyPrim _ -> []
    | TyVar v -> [ v ]
    | TyRecursive (n, ty) -> n :: go ty
    | TyUnion (l, r) | TyIntersection (l, r) | TyFn (l, r) -> go l @ go r
    | TyRecord fields -> List.concat_map (fun (_, t) -> go t) fields
  in
  List.sort_uniq String.compare (go ty)

let string_of_ty ty =
  let tyVars = uniq_typevars ty in
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
    | TyRecursive (n, ty) ->
        Printf.sprintf "μ%s. %s" (List.assoc n ctx) (show 30 ty)
    | TyUnion (l, r) ->
        Printf.sprintf "%s | %s" (show 20 l) (show 20 r) |> parenIf (prec > 20)
    | TyIntersection (l, r) ->
        Printf.sprintf "%s & %s" (show 25 l) (show 25 r) |> parenIf (prec > 25)
    | TyFn (p, r) ->
        Printf.sprintf "%s -> %s" (show 11 p) (show 10 r) |> parenIf (prec > 10)
    | TyRecord fields ->
        List.map (fun (f, t) -> Printf.sprintf "%s: %s" f (show 0 t)) fields
        |> String.concat ", " |> Printf.sprintf "{%s}"
  in
  show 0 ty
