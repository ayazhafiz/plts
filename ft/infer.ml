open Language
open Language.Ast
module T = Typecheck

(** Describes a constraint on a infer type variable. *)
type constr =
  | LB of int * ty
      (** [LB(infer, ty)] describes a lower bound for a infer variable, namely infer >= ty. *)
  | UB of int * ty
      (** [UB(infer, ty)] describes an upper bound for a infer variable, namely infer <= ty. *)

let ( >= ) infer ty = LB (infer, ty)

let ( <= ) infer ty = UB (infer, ty)

let reify_ty get_solution =
  let rec dotype = function
    | (Any as ty) | (Int as ty) | (Never as ty) -> ty
    | Tuple tys -> Tuple (List.map dotype tys)
    | Not ty -> Not (dotype ty)
    | Inter tys -> Inter (TySet.map dotype tys)
    | Union tys -> Union (TySet.map dotype tys)
    | Infer t -> get_solution t
  in
  dotype

(** Inference variable solving for FT:
      Given a infer variable f1 with constraints
        f1 >= T
        f1 >= U
        f1 <= V
        f1 <= W
      We find that
                   f1 := never & |_S ( f1 >= S ) = never|T|U = T|U
        subject to f1 <: any & &_S ( f1 <= S ) = any&V&W = V&W
      Clearly, if the subtyping condition for f1 is unsatisfiable, f1 has no
      solution (by analysis of lower and upper bounds introduced by the
      constraints, and the fact that the FT type system is a complete lattice).

      TODO: handle cycles between inference variables (see examples/page5.ft).

      Note that this solver is a form of liveness analysis - if a solution
      yields f1 := never, we know that variables of type f1 are never reached. *)
module ConstraintSolver = struct
  module G = Graph.Imperative.Digraph.Concrete (struct
    type t = int

    let compare = Stdlib.compare

    let hash i = i

    let equal = Stdlib.( = )
  end)

  module SCC = Graph.Components.Make (G)

  type bounds = { lb : ty list; ub : ty list }

  let trivial_bounds = { lb = [ Never ]; ub = [ Any ] }

  let string_of_boundset (var, { lb; ub }) =
    Printf.sprintf "%s\n\tlower: %s\n\tupper: %s" (string_of_ty (Infer var))
      (List.map string_of_ty lb |> String.concat "\n\t       ")
      (List.map string_of_ty ub |> String.concat "\n\t       ")

  let rec infervars_of_ty = function
    | Any | Int | Never -> []
    | Tuple tys -> List.concat_map infervars_of_ty tys
    | Not ty -> infervars_of_ty ty
    | Inter tys | Union tys ->
        TySet.to_list tys |> List.concat_map infervars_of_ty
    | Infer t -> [ t ]

  let rec sets_of_constrs = function
    | [] -> []
    | (LB (v, _) | UB (v, _)) :: _ as all ->
        let vconstrs, rest =
          List.partition (function LB (t, _) | UB (t, _) -> t = v) all
        in
        let vbounds =
          List.fold_left
            (fun { lb; ub } -> function
              | LB (_, ty) -> { lb = ty :: lb; ub }
              | UB (_, ty) -> { lb; ub = ty :: ub })
            trivial_bounds vconstrs
        in
        (v, vbounds) :: sets_of_constrs rest

  (** [solution_clusters_of_sets sets] yields a list [SO] of lists of inference
      variables with the following properties. Let [G] be a directed graph whose
      vertices are inference variables and an edge [i=>j] means that the lower
      bounds of [i] depend on the variable [j]. Then
        - each cluster [C\in SO] of inference variables is strongly connected in
          [G]
        - [SO] is topologically sorted, so that each [ivar\in SO(n)] depends
          only on inference variables in [SO(1)..SO(n-1)] or in its strongly
          connected component [SO(n)]. *)
  let solution_clusters_of_sets sets =
    let graph = G.create ~size:(List.length sets) () in
    List.iter (fun (ivar, _) -> G.add_vertex graph (G.V.create ivar)) sets;
    let ( => ) i j = G.add_edge graph (G.V.create i) (G.V.create j) in
    List.iter
      (fun (i, { lb; _ }) ->
        List.iter
          (fun lb -> List.iter (fun j -> i => j) (infervars_of_ty lb))
          lb)
      sets;
    let clusters = SCC.scc_list graph in
    List.map (List.map G.V.label) clusters

  let string_of_solution_clusters sc =
    List.map
      (fun cluster ->
        List.map string_of_int cluster
        |> String.concat "," |> Printf.sprintf "[%s]")
      sc
    |> String.concat " => "

  let solve constrs =
    let open Typecheck in
    let sets = sets_of_constrs constrs in
    let solution_clusters = solution_clusters_of_sets sets in
    let solved = Hashtbl.create (List.length sets) in
    (* Reify a type with inference vars we already solved for. *)
    let reify_solved =
      reify_ty (fun ivar ->
          match Hashtbl.find_opt solved ivar with
          | Some ty -> ty
          | None -> Infer ivar)
    in
    (* Check if a variable has a "deep" inference var, that is one that is not
       on the toplevel. *)
    let has_deep_ivar =
      let rec ck deep = function
        | Any | Int | Never -> false
        | Tuple tys -> List.exists (ck true) tys
        | Not ty -> ck true ty
        | Inter tys | Union tys -> TySet.exists (ck true) tys
        | Infer _ -> deep
      in
      ck false
    in
    (* Solve for potential solutions. *)
    let rec solve = function
      | [] -> ()
      | cluster_ivars :: rest ->
          let cluster =
            List.map
              (fun ivar ->
                let { lb; _ } = List.assoc ivar sets in
                let lb = List.map reify_solved lb in
                (ivar, lb))
              cluster_ivars
          in
          (* Check if this cluster is "deep", meaning that some ivar deeply
             depends on any other ivar in the cluster. *)
          let is_deep =
            List.exists (fun (_, lbs) -> List.exists has_deep_ivar lbs) cluster
          in
          (if is_deep then
           (* A deep cluster cannot be solved for an exact solution: some
              inference variable will have a recursive type, which we can't
              express. The closest we (or the user) can get is to mark
              everything as [Any]. *)
           List.iter (fun ivar -> Hashtbl.add solved ivar Any) cluster_ivars
          else
            (* Every ivar in a flat cluster has the same solution, if it exists.
               This solution is the union of the lower bounds of each ivar,
               excluding the ivars themselves. *)
            let sol_lb =
              List.concat_map snd cluster
              |> List.filter (fun t -> not (needs_inference t))
            in
            let sol = union sol_lb in
            List.iter (fun ivar -> Hashtbl.add solved ivar sol) cluster_ivars);
          solve rest
    in
    (* Verify solutions actually exist against upper bounds. *)
    let rec verify = function
      | [] -> ()
      | (ivar, { ub; _ }) :: rest ->
          let vty = Hashtbl.find solved ivar in
          let ub = List.map (reify_ty (Hashtbl.find solved)) ub in
          let ubty = inter ub in
          if vty </: ubty then
            tyerr
              (Printf.sprintf "Unsolvable constraint: %s <: %s"
                 (string_of_ty vty) (string_of_ty ubty));
          verify rest
    in
    (try
       solve solution_clusters;
       verify sets
     with e ->
       let bss = List.map string_of_boundset sets |> String.concat "\n" in
       let so = string_of_solution_clusters solution_clusters in
       Printf.eprintf
         "Bad constraints. Debug output:\n\
          Bounds sets:\n\
          %s\n\
          Solution clusters:\n\
          %s"
         bss so;
       raise e);
    solved

  let%expect_test "constraint solving" =
    let constrs =
      [
        1 >= Int;
        1 >= Tuple [ Int; Int ];
        1 <= Infer 2;
        2 >= Int;
        2 >= Tuple [ Int; Int ];
        2 >= Tuple [ Int; Int; Int ];
        3 >= Infer 1;
        3 >= Infer 2;
        3
        <= Union
             (TySet.of_list
                [ Int; Tuple [ Int; Int ]; Tuple [ Int; Int; Int ] ]);
      ]
    in
    let solved =
      solve constrs |> Hashtbl.to_seq |> List.of_seq
      |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.map (fun (v, ty) ->
             Printf.sprintf "%d: %s" v (Typecheck.dnf_plus ty |> string_of_ty))
      |> String.concat "\n"
    in
    print_string solved;
    [%expect
      {|
      1: int | (int, int)
      2: int | (int, int) | (int, int, int)
      3: int | (int, int) | (int, int, int) |}]
end

let collect_constraints tm =
  let constrs = ref [] in
  let ( >=+ ) a b =
    (match a with Infer a -> constrs := (a >= b) :: !constrs | _ -> ());
    match b with Infer b -> constrs := (b <= a) :: !constrs | _ -> ()
  in
  let rec typeof venv fenv = function
    | Num _ -> Int
    | Var (v, _) -> T.getvar v venv
    | Tup (ts, _) -> Tuple (List.map (typeof venv fenv) ts)
    | App (fn, args, _) ->
        let params, retty = T.getfn fn fenv in
        if List.length args <> List.length params then
          T.tyerr ("wrong number of arguments to " ^ fn);
        let argtys = List.map (typeof venv fenv) args in
        List.iter2 ( >=+ ) params argtys;
        retty
    | Dec (fn, params, body, cont, _) ->
        let params = List.map (fun (name, ty, _) -> (name, !ty)) params in
        let paramtys = List.map snd params in
        let venv' = params @ venv in
        let retty = typeof venv' fenv body in
        let fenv' = (fn, (paramtys, retty)) :: fenv in
        typeof venv fenv' cont
    | If (var, isty, then', else', _) ->
        let varty = T.getvar var venv in
        varty >=+ isty;
        let then_varty = T.inter [ varty; isty ] in
        let else_varty = T.inter [ varty; Not isty ] in
        let thenty = typeof ((var, then_varty) :: venv) fenv then' in
        let elsety = typeof ((var, else_varty) :: venv) fenv else' in
        T.union [ thenty; elsety ]
  in
  ignore (typeof [] [] tm);
  !constrs

let apply_solutions solutions =
  let get_solution ty =
    match Hashtbl.find_opt solutions ty with
    | None ->
        (* If there is no available solution, it means the type was never reached. *)
        Never
    | Some ty -> ty
  in
  let dotype = reify_ty get_solution in
  let doparam ((_, ty, _) as p) =
    ty := dotype !ty;
    p
  in
  let rec doterm = function
    | (Num _ as t) | (Var _ as t) -> t
    | Tup (ts, ckty) -> Tup (List.map doterm ts, ckty)
    | App (fn, args, ckty) -> App (fn, List.map doterm args, ckty)
    | Dec (fn, params, body, cont, ckty) ->
        Dec (fn, List.map doparam params, doterm body, doterm cont, ckty)
    | If (var, is, then', else', ckty) ->
        If (var, dotype is, doterm then', doterm else', ckty)
  in
  doterm

let infer_types term =
  try
    let reify =
      term |> collect_constraints |> ConstraintSolver.solve |> apply_solutions
    in
    Ok (reify term)
  with T.TyErr what -> Error what
