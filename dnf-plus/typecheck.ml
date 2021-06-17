open Language
open Language.Ast

let inter tys = Inter (TySet.of_list tys)

let union tys = Union (TySet.of_list tys)

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
    match ty with
    | Any | Never | Int -> ty
    | Tuple tys -> Tuple (List.map unwrap_trivial tys)
    | Not ty -> Not (unwrap_trivial ty)
    | Inter tys ->
        TySet.map unwrap_trivial tys |> fun tys ->
        if TySet.cardinal tys = 1 then TySet.choose tys else Inter tys
    | Union tys -> (
        TySet.map unwrap_trivial tys |> TySet.remove Never |> function
        | tys -> (
            match TySet.to_list tys with
            | [] -> Never
            | [ t ] -> t
            | _ -> Union tys))
  in
  ty |> flatten_inters |> flatten_unions |> unwrap_trivial

(***************)
(* Type Atoms  *)
(* Section 3.1 *)
(***************)

type pos_atom = AAny | AInt | ATuple of pos_atom list

type neg_atom = ANot of pos_atom

type star_atom = AtomPos of pos_atom | AtomNeg of neg_atom

exception Not_positive of string

let atom_of_ty =
  let rec pos = function
    | Any -> AAny
    | Int -> AInt
    | Tuple tys -> ATuple (List.map pos tys)
    | ty -> raise (Not_positive (string_of_ty ty))
  in
  function
  | Never -> AtomNeg (ANot AAny)
  | Not Never -> AtomPos AAny
  | Not ty -> AtomNeg (ANot (pos ty))
  | ty -> AtomPos (pos ty)

let rec ty_of_pos = function
  | AAny -> Any
  | AInt -> Int
  | ATuple tys -> Tuple (List.map ty_of_pos tys)

let ty_of_neg (ANot p) = Not (ty_of_pos p)

let ty_of_atom = function AtomNeg n -> ty_of_neg n | AtomPos p -> ty_of_pos p

type atom_inter = AINever | AIPos of pos_atom

(** Atom subtyping s ≤ t: Figure 4. *)
let rec ( @<: ) s t =
  match (s, t) with
  | s, t when s = t -> true
  | _, AAny -> true
  | ATuple s, ATuple t ->
      List.length s = List.length t && List.for_all2 ( @<: ) s t
  | _ -> false

(** Computes s ≥ t. *)
let ( @:> ) s t = t @<: s

(** Computes s /≥ t. *)
let ( @:/> ) s t = not (s @:> t)

(** Atom intersection: Definition 4. *)
let rec ( @^^ ) t1 t2 =
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> AIPos t1
  | AAny, t | t, AAny -> AIPos t
  | AInt, ATuple _ | ATuple _, AInt -> AINever
  | ATuple t, ATuple s
    when List.length t <> List.length s
         || List.exists2 (fun ti si -> ti @^^ si = AINever) t s ->
      AINever
  | ATuple t, ATuple s ->
      AIPos
        (ATuple
           (List.map2
              (fun ti si ->
                match ti @^^ si with
                | AIPos p -> p
                | AINever -> failwith "impossible")
              t s))
  | _ -> failwith "impossible state"

(***************************)
(* Disjunctive Normal Form *)
(* Section 3.2             *)
(***************************)

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

exception Not_dnf of string

module StarAtomSet = Set.Make (struct
  type t = star_atom

  let compare = compare
end)

type dnf_inter = DnfInter of StarAtomSet.t

type dnf = DnfForm of dnf_inter list

(** Lemma 4: DNF(T) ~ |_i &_i T*_{i,j},
    where T* = T+ | T-,
          T- = !T+
          T+ = any | int | (T_1, ..., T_n) *)
let dnf_of_ty =
  let dnf_inter ty =
    match ty with
    | Inter tys ->
        DnfInter
          (TySet.to_list tys |> List.map atom_of_ty |> StarAtomSet.of_list)
    | Union _ -> raise (Not_dnf (string_of_ty ty))
    | Not _ | Tuple _ | Any | Never | Int ->
        DnfInter (StarAtomSet.singleton (atom_of_ty ty))
  in
  let rec dnf_form ty =
    match ty with
    | Union tys -> DnfForm (TySet.to_list tys |> List.map dnf_inter)
    | Inter _ -> dnf_form (Union (TySet.singleton ty))
    | Not _ | Tuple _ | Any | Never | Int ->
        dnf_form (Inter (TySet.singleton ty))
  in
  dnf_form

let ty_of_dnf (DnfForm inters) =
  Union
    (List.map
       (fun (DnfInter atoms) ->
         Inter
           (StarAtomSet.to_seq atoms |> List.of_seq |> List.map ty_of_atom
          |> TySet.of_list))
       inters
    |> TySet.of_list)
  |> flatten_ty

(* Definition 6 *)
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
  let dnf_ty = fix None (flatten_ty ty) in
  let dnf_ty = dnf_of_ty dnf_ty in
  dnf_ty

(****************************)
(* Conjuct Canonicalization *)
(* Section 4.3              *)
(****************************)

(* Definition 8 *)

module PosAtomSet = struct
  include Set.Make (struct
    type t = pos_atom

    let compare = compare
  end)

  let to_list t = to_seq t |> List.of_seq
end

(** [canonical_conjuct] describes the result of Conjuct Canonicalization, which
    results in the bottom type [Never] or the form described in Definition 7. *)
type canonical_conjuct =
  | ConjNever  (** Canonicalization determined this is a "never" type. *)
  | CanonicalConjuct of { add : pos_atom; sub : PosAtomSet.t }
      (** A Canonical Conjuct as described in Definition 7, of the form
            1. T+_1 & !T+_2 & ... & !T+_n
               ^^^^   ^^^^^^^^^^^^^^^^^^^
               atom           sub
            2. Forall !T+_k, T+_1 \ne T+_k and T+_k <: T+_1
            3. For any two !T+_k, !T+_m, T+_k :/> T+_m *)

let assert_canonical_conjuct = function
  | ConjNever -> ()
  | CanonicalConjuct { add; sub } ->
      PosAtomSet.iter (fun tk -> assert (add <> tk && (add @:> tk))) sub;
      PosAtomSet.iter
        (fun tk ->
          PosAtomSet.iter (fun tm -> if tk <> tm then assert (tk @:/> tm)) sub)
        sub

let and_then_none pred = function Some res -> Some res | None -> pred ()

let can (DnfInter atoms) =
  let pos, neg =
    StarAtomSet.fold
      (fun atom (pos, neg) ->
        match atom with
        | AtomPos p -> (p :: pos, neg)
        | AtomNeg (ANot n) -> (pos, n :: neg))
      atoms ([], [])
  in
  let neg = PosAtomSet.of_list neg in
  (* Rule 2: T+_i & T+_j & ... => (T+_i ^^ T+_j) & ... *)
  let rec rule2 total pos =
    match (total, pos) with
    | AINever, _ -> AINever
    | (AIPos _ as total), [] -> total
    | AIPos total, atom :: rest -> rule2 (total @^^ atom) rest
  in
  (* Rule 3: T+_x & !T+_y & ... => never                         if T+_x <: T+_y *)
  let rule3 add sub () =
    if PosAtomSet.exists (fun ty -> add @<: ty) sub then Some ConjNever
    else None
  in
  (* Rule 4: T+_x & !T+_y & ... => T+_x & ...                    if T+_x ^^ T+_y = never *)
  let rule4 add sub () =
    PosAtomSet.find_first_opt (fun ty -> add @^^ ty = AINever) sub
    |> Option.map (fun ty ->
           CanonicalConjuct { add; sub = PosAtomSet.remove ty sub })
  in
  (* Rule 5: T+_x & !T+_y & ... => T+_x & !(T+_x ^^ T+_y) ...    if T+_x :/> T+_y *)
  let rule5 add sub () =
    PosAtomSet.find_first_opt (fun ty -> add @:/> ty) sub
    |> Option.map (fun ty ->
           match add @^^ ty with
           | AIPos interxy ->
               CanonicalConjuct
                 { add; sub = PosAtomSet.(add interxy (remove ty sub)) }
           | AINever -> failwith "impossible")
  in
  (* Rule 6: !T+_x & !T+_y & ... => !T+_x & ...                  if T+_x :> T+_y *)
  let rule6 add sub () =
    let rec find_txy = function
      | [] -> None
      | tx :: rest -> (
          let rec find_ty = function
            | [] -> None
            | ty :: _ when tx @:> ty -> Some (tx, ty)
            | _ :: rest -> find_ty rest
          in
          match find_ty rest with Some txy -> Some txy | None -> find_txy rest)
    in
    find_txy (PosAtomSet.to_list sub)
    |> Option.map (fun (_tx, ty) ->
           CanonicalConjuct { add; sub = PosAtomSet.remove ty sub })
  in
  (* The rest of Definition 8 - rule 3-6 *)
  let rec rule3456 add sub =
    match
      None
      |> and_then_none (rule3 add sub)
      |> and_then_none (rule4 add sub)
      |> and_then_none (rule5 add sub)
      |> and_then_none (rule6 add sub)
    with
    | None -> CanonicalConjuct { add; sub }
    | Some ConjNever -> ConjNever
    | Some (CanonicalConjuct { add; sub }) -> rule3456 add sub
  in
  let any = AIPos AAny in
  let conj =
    match rule2 any pos with
    | AINever -> ConjNever
    | AIPos add -> rule3456 add neg
  in
  assert_canonical_conjuct conj;
  conj

(********************)
(* Subtyping        *)
(* Section 4.4, 4.5 *)
(********************)

(** Definition 11: DNF+ construction *)
let dnf_plus t =
  let (DnfForm union_of_inters) = dnf t in
  let union_of_inters =
    List.map
      (fun inters ->
        match can inters with
        | ConjNever -> Never
        | CanonicalConjuct { add; sub } ->
            let add = ty_of_pos add in
            let sub =
              PosAtomSet.to_list sub
              |> List.map (fun t_ij -> ty_of_neg (ANot t_ij))
            in
            Inter (TySet.of_list (add :: sub)))
      union_of_inters
  in
  let t = TySet.of_list union_of_inters in
  let t = TySet.remove Never t in
  flatten_ty (Union t)

(** Determines [s <: t]. *)
let ( <: ) s t = dnf_plus (inter [ s; Not t ]) = Never

let ( </: ) s t = not (s <: t)

(*********************)
(* Term Typechecking *)
(*********************)

type fnbind = { params : (string * ty) list; body : term }

exception TyErr of string

let tyerr what = raise (TyErr what)

let getvar v venv =
  match List.assoc_opt v venv with
  | Some ty -> ty
  | None -> tyerr ("variable " ^ v ^ " is unbound")

let update ty real =
  ty := Some real;
  real

let rec typeof ?(report_unhabited_branches = false) venv fenv = function
  | Num _ -> Int
  | Var (v, ty) -> update ty (getvar v venv)
  | Tup (ts, ty) -> update ty (Tuple (List.map (typeof venv fenv) ts))
  | App (fn, args, ty) when List.mem_assoc fn fenv ->
      let { params; body } = List.assoc fn fenv in
      if List.length args <> List.length params then
        tyerr ("wrong number of arguments to " ^ fn);
      let venv' =
        List.fold_left2
          (fun venv a (p, pty) ->
            let aty = typeof venv fenv a in
            if aty </: pty then
              tyerr
                (Printf.sprintf "argument %s is wrong type" (string_of_term a));
            (* Bind specific type of this argument in this application, rather
               than the "generic" formal parameter type; this allows us to
               retrieve a specific return type for this exact application. *)
            (p, aty) :: venv)
          venv args params
      in
      update ty (typeof venv' fenv body)
  | App (fn, _, _) -> tyerr ("function " ^ fn ^ " is unbound")
  | Dec (fn, params, body, cont, ty) ->
      let venv' = params @ venv in
      (* ensure body is sound with declared args *)
      ignore
        (update ty (typeof ~report_unhabited_branches:true venv' fenv body));
      (* NB: we are binding the body of the function rather than its universal
         return type for the declared parameters. This permits us to return the
         type of _specific_ applications of the function by inlining arguments,
         which may be subtypes of the formal params, at those applications.
         See the [App] case. *)
      let fenv' = (fn, { params; body }) :: fenv in
      typeof venv fenv' cont
  | If (var, isty, then', else', ty) ->
      let varty = getvar var venv in
      let vthenty = inter [ varty; isty ] in
      let velsety = inter [ varty; Not isty ] in
      let thenty =
        if vthenty </: Never then typeof ((var, vthenty) :: venv) fenv then'
        else (
          if report_unhabited_branches then tyerr "then branch is never taken";
          Never)
      in
      let elsety =
        if velsety </: Never then typeof ((var, velsety) :: venv) fenv else'
        else (
          if report_unhabited_branches then tyerr "else branch is never taken";
          Never)
      in
      update ty (union [ thenty; elsety ])

let typecheck body =
  try Ok (typeof [] [] body |> dnf_plus) with TyErr err -> Error err
