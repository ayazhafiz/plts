open Language
open Language.Ast

let flatten_ty ty =
  let rec flatten_inters ty =
    match ty with
    | Any | Never | Int -> ty
    | Tuple tys -> Tuple (List.map flatten_inters tys)
    | Not ty -> Not (flatten_inters ty)
    | Union tys -> Union (TySet.map flatten_inters tys)
    | Inter tys ->
        Inter
          (TySet.fold
             (function Inter intys -> TySet.union intys | ty -> TySet.add ty)
             (TySet.map flatten_inters tys)
             TySet.empty)
  in
  let rec flatten_unions ty =
    match ty with
    | Any | Never | Int -> ty
    | Tuple tys -> Tuple (List.map flatten_unions tys)
    | Not ty -> Not (flatten_unions ty)
    | Inter tys -> Inter (TySet.map flatten_unions tys)
    | Union tys ->
        Union
          (TySet.fold
             (function Union intys -> TySet.union intys | ty -> TySet.add ty)
             (TySet.map flatten_unions tys)
             TySet.empty)
  in
  let rec unwrap_trivial ty =
    let go builder tys =
      if TySet.cardinal tys = 1 then TySet.choose tys else builder tys
    in
    match ty with
    | Any | Never | Int -> ty
    | Tuple tys -> Tuple (List.map unwrap_trivial tys)
    | Not ty -> Not (unwrap_trivial ty)
    | Inter tys -> go (fun tys -> Inter tys) (TySet.map unwrap_trivial tys)
    | Union tys -> go (fun tys -> Union tys) (TySet.map unwrap_trivial tys)
  in
  ty |> flatten_inters |> flatten_unions |> unwrap_trivial

let is_union = function Union _ -> true | _ -> false

let get_union = function Union tys -> tys | _ -> failwith "not union"

let is_inter = function Inter _ -> true | _ -> false

let get_inter = function Inter tys -> tys | _ -> failwith "not union"

let is_not = function Not _ -> true | _ -> false

let get_not = function Not ty -> ty | _ -> failwith "not union"

let split_at elt =
  let rec walk before = function
    | [] -> failwith "splitting element not found"
    | hd :: rest when hd = elt -> (List.rev before, rest)
    | hd :: rest -> walk (hd :: before) rest
  in
  walk []

(* See 3.2. *)
let dnf_step = function
  | Not (Not t) -> t
  | Not (Union tys) -> Inter (TySet.map (fun t -> Not t) tys)
  | Not (Inter tys) -> Union (TySet.map (fun t -> Not t) tys)
  | Inter inters when TySet.exists is_union inters ->
      (* Factor unions out of intersections:

            T & (U | V) & A & (B | C)
         =>   (T & U & A & (B | C))
            | (T & V & A & (B | C))

         (B | C) will be factored out in a later rewrite pass. *)
      let factor_out = TySet.find_first is_union inters in
      let rest_inters = TySet.remove factor_out inters in
      Union
        (TySet.map
           (fun s_i -> Inter (TySet.add s_i rest_inters))
           (get_union factor_out))
  | Tuple tys when List.exists is_union tys ->
      (* Factor unions out of tuples. *)
      let factor_out = List.find is_union tys in
      let before, after = split_at factor_out tys in
      Union
        (TySet.map
           (fun t_i -> Tuple (before @ [ t_i ] @ after))
           (get_union factor_out))
  | Tuple tys when List.exists is_inter tys ->
      (* Factor intersections out of tuples. *)
      let factor_out = List.find is_inter tys in
      let before, after = split_at factor_out tys in
      Inter
        (TySet.map
           (fun t_i -> Tuple (before @ [ t_i ] @ after))
           (get_inter factor_out))
  | Tuple tys when List.exists is_not tys ->
      (* Factor negations out of tuples, in the following way:
           (..., !T, ...) => (..., any, ...) & !(..., T, ...)
         E.g. (int, !int) => (int, any) & !(int, int), inhabited (f.x.) by (1, (1, 2)). *)
      let factor_out = List.find is_not tys in
      let before, after = split_at factor_out tys in
      let t = get_not factor_out in
      Inter
        (TySet.of_list
           [
             Tuple (before @ [ Any ] @ after);
             Not (Tuple (before @ [ t ] @ after));
           ])
  | ty -> ty

let dnf ty =
  let rec step ty =
    match ty with
    | Any | Int | Never -> ty
    | Tuple tys -> dnf_step (Tuple (List.map step tys))
    | Not t -> dnf_step (Not (step t))
    | Inter tys -> dnf_step (Inter (TySet.map step tys))
    | Union tys -> dnf_step (Union (TySet.map step tys))
  in
  let rec fix last_ty ty =
    match last_ty with
    | Some last_ty when last_ty = ty -> ty
    | _ -> fix (Some ty) (flatten_ty (step ty))
  in
  fix None (flatten_ty ty)
