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
  type bounds = { lb : ty list; ub : ty list }

  let trivial_bounds = { lb = [ Never ]; ub = [ Any ] }

  let string_of_boundset (var, { lb; ub }) =
    Printf.sprintf "%s\n\tlower: %s\n\tupper: %s" (string_of_ty (Infer var))
      (List.map string_of_ty lb |> String.concat "\n\t       ")
      (List.map string_of_ty ub |> String.concat "\n\t       ")

  let infervars_of_bounds { lb; ub } =
    List.fold_left
      (fun all -> function Infer f -> f :: all | _ -> all)
      [] (lb @ ub)

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

  let toposort depgraph =
    let nodes = List.map fst depgraph in
    let rec toposort path n =
      if List.mem n path then path (* already accounted for *)
      else
        let nodedeps = List.assoc n depgraph in
        let dep_path' = List.fold_left toposort path nodedeps in
        n :: dep_path'
    in
    List.fold_left toposort [] nodes |> List.rev

  let%test "toposort" =
    let simple_graph =
      [
        (6, [ 1; 3 ]);
        (5, [ 3; 4 ]);
        (4, [ 3; 2 ]);
        (3, [ 1; 2 ]);
        (2, []);
        (1, []);
      ]
    in
    toposort simple_graph = [ 1; 2; 3; 6; 4; 5 ]

  let solve constrs =
    let open Typecheck in
    let sets = sets_of_constrs constrs in
    let infervar_depgraph =
      List.map (fun (v, bounds) -> (v, infervars_of_bounds bounds)) sets
    in
    let solve_order = toposort infervar_depgraph in
    let solved = Hashtbl.create (List.length solve_order) in
    let reify = List.map (reify_ty (Hashtbl.find solved)) in
    let rec go = function
      | [] -> ()
      | v :: rest ->
          let { lb; _ } = List.assoc v sets in
          let lb = reify lb in
          let vty = Union (TySet.of_list lb) in
          Hashtbl.add solved v vty;
          go rest
    in
    let rec verify = function
      | [] -> ()
      | v :: rest ->
          let vty = Hashtbl.find solved v in
          let { ub; _ } = List.assoc v sets in
          let ub = reify ub in
          let ubty = Inter (TySet.of_list ub) in
          if vty </: ubty then
            tyerr
              (Printf.sprintf "Unsolvable constraint: %s <: %s"
                 (string_of_ty vty) (string_of_ty ubty));
          verify rest
    in
    (try
       go solve_order;
       verify solve_order
     with _ ->
       let bss = List.map string_of_boundset sets |> String.concat "\n" in
       let so = List.map string_of_int solve_order |> String.concat ", " in
       failwith
         ("Bad constraints. Debug output:\nBounds sets:\n" ^ bss
        ^ "\nSolving order:\n" ^ so));
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
  let reify =
    term |> collect_constraints |> ConstraintSolver.solve |> apply_solutions
  in
  try Ok (reify term) with T.TyErr what -> Error what
