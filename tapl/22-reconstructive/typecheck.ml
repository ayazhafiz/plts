open Language
open Eval
open Util.Error
open Util

let mbComputeSingleTyStep ctx ty =
  match ty with
  | TyVar (idx, _) -> getBoundNameOfTypeVar dummyinfo ctx idx
  | _ -> None

let rec simplifyTy ctx ty =
  match mbComputeSingleTyStep ctx ty with
  | Some ty' -> simplifyTy ctx ty'
  | None -> ty

let rec tyeq ctx ty1 ty2 =
  let ty1 = simplifyTy ctx ty1 in
  let ty2 = simplifyTy ctx ty2 in
  match (ty1, ty2) with
  | TyUnknown, TyUnknown
  | TyNever, TyNever
  | TyNat, TyNat
  | TyFloat, TyFloat
  | TyString, TyString
  | TyUnit, TyUnit
  | TyBool, TyBool ->
      true
  | TyVar (i, _), TyVar (j, _) -> i = j
  | TyArrow (t11, t12), TyArrow (t21, t22) ->
      tyeq ctx t11 t21 && tyeq ctx t12 t22
  | TyId name1, TyId name2 -> name1 = name2
  | TyRecord fields1, TyRecord fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all
           (fun (name, ty1) ->
             try
               let ty2 = List.assoc name fields2 in
               tyeq ctx ty1 ty2
             with Not_found -> false)
           fields1
  | TyVariant fields1, TyVariant fields2 ->
      List.length fields1 = List.length fields2
      && List.for_all2
           (fun (_, ty1) (_, ty2) -> tyeq ctx ty1 ty2)
           fields1 fields2
  | TySource ty1, TySource ty2 -> tyeq ctx ty1 ty2
  | TyRef ty1, TyRef ty2 -> tyeq ctx ty1 ty2
  | _ -> false

(* Returns [ true ] iff tyS <: tyT
                            ^^ is a subtype *)
let rec subtype ctx tyS tyT =
  tyeq ctx tyS tyT
  ||
  let tyS = simplifyTy ctx tyS in
  let tyT = simplifyTy ctx tyT in
  match (tyS, tyT) with
  | _, TyUnknown -> true
  | TyNever, _ -> true
  (* The subtyping relationship between a function is contravariant on the
     paramter and covariant on the return type.
     For example, [ (Animal) -> Cat ] is a subtype of [ (Cat) -> Animal ],
     because anywhere a Cat will be taken as a parameter that can be treated as
     an animal, and anywhere we expect to be returned an Animal we can accept a
     Cat. *)
  | TyArrow (tyS1, tyS2), TyArrow (tyT1, tyT2) ->
      subtype ctx tyT1 tyS1 && subtype ctx tyS2 tyT2
  | TyRecord tyS, TyRecord tyT ->
      List.for_all
        (fun (name, tyT_i) ->
          try
            let tyS_i = List.assoc name tyS in
            subtype ctx tyS_i tyT_i
          with Not_found -> false)
        tyT
  (* <BigCat> <: <Cat|Dog>, but <Cat|Dog> <: <Cat> does not hold. *)
  | TyVariant tyS, TyVariant tyT ->
      List.for_all
        (fun (name, tyS_i) ->
          try
            let tyT_i = List.assoc name tyT in
            subtype ctx tyS_i tyT_i
          with Not_found -> false)
        tyS
  (* Source types are covariant. That is, I can read a Source BigCat wherever I
     can read a Source Cat (Source BigCat <: Source Cat)*)
  | TySource tyS, TySource tyT -> subtype ctx tyS tyT
  (* Ref BigCat <: Source Cat *)
  | TyRef tyS, TySource tyT -> subtype ctx tyS tyT
  (* Reference types are invariant; that is, their parameters must be subtypes
     of each other.
     For example, if I have [ c = ref {Cat} : Ref Cat ], [ !c ] implies Ref Cat
       <: Ref Animal (anywhere I read a Cat, I can read it as an Animal).
     But also, in places I expect to write to a reference, it suffices to pass a
     reference to a supertype of Cat, because anywhere I write a Cat, I can
     write to a supertype of it. That is, [ c := {Cat} ] can be substituted with
     [ a := {Cat} ] where [ a : Ref Animal ]. Thus Ref Animal <: Ref Cat. *)
  | TyRef tyS, TyRef tyT -> subtype ctx tyS tyT && subtype ctx tyT tyS
  | _, _ -> false

let findUnion t1 t2 cmp_fn =
  let labels1 = List.map (fun (l, _) -> l) t1 in
  let labels2 = List.map (fun (l, _) -> l) t2 in
  let common = List.filter (fun l -> List.mem l labels2) labels1 in
  List.map
    (fun l ->
      let ty = cmp_fn (List.assoc l t1) (List.assoc l t2) in
      (l, ty))
    common

let findIntersect t1 t2 cmp_fn =
  let labels1 = List.map (fun (l, _) -> l) t1 in
  let labels2 = List.map (fun (l, _) -> l) t2 in
  let all =
    List.append labels1
      (List.filter (fun l -> not (List.mem l labels1)) labels2)
  in
  List.map
    (fun l ->
      let ty =
        try
          let tl_1 = List.assoc l t1 in
          try
            let tl_2 = List.assoc l t2 in
            cmp_fn tl_1 tl_2
          with Not_found -> tl_1
        with Not_found -> List.assoc l t2
      in
      (l, ty))
    all

(* Finds the least upper bound (supertype) of two types. *)
let rec join ctx t1 t2 =
  if subtype ctx t1 t2 then t2
  else if subtype ctx t2 t1 then t1
  else
    let t1 = simplifyTy ctx t1 in
    let t2 = simplifyTy ctx t2 in
    match (t1, t2) with
    | TyArrow (ty11, ty12), TyArrow (ty21, ty22) ->
        (* [(BigCat)->Cat] join [(Cat)->Animal] yields [(BigCat)->Animal]. *)
        let tyIn = meet ctx ty11 ty21 in
        let tyOut = join ctx ty12 ty22 in
        TyArrow (tyIn, tyOut)
    | TyRecord f1, TyRecord f2 -> TyRecord (findUnion f1 f2 (join ctx))
    | TyVariant f1, TyVariant f2 -> TyVariant (findIntersect f1 f2 (join ctx))
    | _, _ -> TyUnknown

(* Finds the greatest lower bound (subtype) of two types. *)
and meet ctx t1 t2 =
  if subtype ctx t1 t2 then t1
  else if subtype ctx t2 t1 then t2
  else
    let t1 = simplifyTy ctx t1 in
    let t2 = simplifyTy ctx t2 in
    match (t1, t2) with
    | TyArrow (ty11, ty12), TyArrow (ty21, ty22) ->
        (* [(BigCat)->Cat] meet [(Cat)->Animal] yields [(Cat)->Cat]. *)
        let tyIn = join ctx ty11 ty21 in
        let tyOut = meet ctx ty12 ty22 in
        TyArrow (tyIn, tyOut)
    | TyRecord f1, TyRecord f2 -> TyRecord (findIntersect f1 f2 (meet ctx))
    | TyVariant f1, TyVariant f2 -> TyVariant (findUnion f1 f2 (meet ctx))
    | _, _ -> TyNever

exception No_typeof of string

let rec typeof ctx t =
  match t with
  | TmVar (info, i, _) -> getTermType info ctx i
  | TmAbs (_, _, None, _) ->
      raise
        (No_typeof
           "Cannot typecheck abstractions with no parameter type; use \
            inference.")
  | TmAbs (_, param, Some paramTy, def) ->
      (* The return type must be evaluated in the context including the
         parameter, but is valid in the context outside the parameter, so we
         shift it down after figuring out its type. *)
      let ctx' = addbinding ctx param (VarBinding paramTy) in
      let retTy = typeShift (-1) (typeof ctx' def) in
      TyArrow (paramTy, retTy)
  | TmApp (info, t1, t2) -> (
      let tyAbs = typeof ctx t1 in
      let tyParam = typeof ctx t2 in
      match simplifyTy ctx tyAbs with
      | TyArrow (tyIn, tyOut) ->
          if subtype ctx tyParam tyIn then tyOut
          else
            errAt info (fun _ ->
                pr "parameter type mismatch: found ";
                printty ctx tyIn;
                pr "; expected a subtype of ";
                printty ctx tyParam)
      | ty ->
          errAt info (fun _ ->
              pr "arrow type expected; got ";
              printty ctx ty) )
  | TmIf (info, cond, thn, els) ->
      if subtype ctx (typeof ctx cond) TyBool then
        join ctx (typeof ctx thn) (typeof ctx els)
      else error info "\"if\" condition not a boolean!"
  | TmCase (info, condTerm, cases) -> (
      match simplifyTy ctx (typeof ctx condTerm) with
      | TyVariant fieldTys ->
          if List.length cases <> List.length fieldTys then
            error info "number of cases does not match variants!"
          else ();

          let caseBodyTys =
            List.map
              (fun (variantName, (nameInBody, body)) ->
                let variantTy =
                  try List.assoc variantName fieldTys
                  with Not_found ->
                    error info
                      ("case " ^ variantName ^ " not found among variants!")
                in
                (* We evaluate the type of the body in the context of the
                   name introduced by the variant case, but that name is not
                   present in the context of the entire case expression, for
                   which the type of case body is for. Thus we shift the
                   resulting type back by one to account for the destruction of
                   the variant name. *)
                let ctx' = addbinding ctx nameInBody (VarBinding variantTy) in
                let bodyTy = typeShift (-1) (typeof ctx' body) in
                bodyTy)
              cases
          in
          let firstBodyTy = List.hd caseBodyTys in
          List.fold_left
            (fun commonTy curBodyTy -> join ctx commonTy curBodyTy)
            firstBodyTy caseBodyTys
      | ty ->
          errAt info (fun _ ->
              pr "expected variant type in case condition, got ";
              printty ctx ty;
              pr "\n") )
  | TmAscribe (info, term, asTy) ->
      let termTy = typeof ctx term in
      if subtype ctx termTy asTy then asTy
      else
        errAt info (fun _ ->
            pr "term type ";
            printty ctx termTy;
            pr " is not a subtype of ";
            printty ctx asTy;
            pr "\n")
  | TmLet (_, name, boundTerm, inTerm) ->
      let boundTy = typeof ctx boundTerm in
      let ctx' = addbinding ctx name (VarBinding boundTy) in
      (* "let name = boundTerm in inTerm" is effectively just [name->boundTerm](inTerm),
         so while "inTerm" should be checked with the context including "name",
         its resulting type is in the context outside that including "name"
         since "name" gets destroyed after the application. *)
      typeShift (-1) (typeof ctx' inTerm)
  | TmVariant (_, variantName, term) ->
      TyVariant [ (variantName, typeof ctx term) ]
  | TmFix (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyArrow (tyFrom, tyTo) ->
          if tyeq ctx tyFrom tyTo then tyTo
          else
            error info
              "\"fix\"ed term accepts and returns different values!\n\
               A fixed recursive term must be of the form T -> T."
      | _ -> error info "A fixed recursive term must be of the form T -> T." )
  | TmRecord (_, fields) ->
      let fieldTys =
        List.map (fun (name, term) -> (name, typeof ctx term)) fields
      in
      TyRecord fieldTys
  | TmProj (info, term, field) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyRecord fields -> (
          try List.assoc field fields
          with Not_found -> error info (field ^ " not found in field types!") )
      | _ -> error info "Projection is not on a record type!" )
  | TmIsZero (info, term) ->
      if subtype ctx (typeof ctx term) TyNat then TyBool
      else error info "term of iszero is not a natural number!"
  | TmSucc (info, term) | TmPred (info, term) ->
      if subtype ctx (typeof ctx term) TyNat then TyNat
      else error info "term of natural function is not a natural number!"
  | TmTimesfloat (info, t1, t2) ->
      if subtype ctx (typeof ctx t1) TyFloat then
        if subtype ctx (typeof ctx t2) TyFloat then TyFloat
        else error info "second term to timesfloat is not a float!"
      else error info "first term to timesfloat is not a float!"
  | TmPlusfloat (info, t1, t2) ->
      if subtype ctx (typeof ctx t1) TyFloat then
        if subtype ctx (typeof ctx t2) TyFloat then TyFloat
        else error info "second term to plusfloat is not a float!"
      else error info "first term to plusfloat is not a float!"
  | TmRef (_, term) -> TyRef (typeof ctx term)
  | TmDeref (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyRef ty -> ty
      | TySource ty -> ty
      | ty ->
          errAt info (fun _ ->
              pr "non-reference types cannot be dereferenced; found ";
              printty ctx ty) )
  | TmRefAssign (info, t1, t2) -> (
      match simplifyTy ctx (typeof ctx t1) with
      | TyRef ty ->
          if subtype ctx (typeof ctx t2) ty then TyUnit
          else
            error info
              "value assigned to reference is not a subtype of the inner type!"
      | ty ->
          errAt info (fun _ ->
              pr "non-reference types cannot be assigned via := ; found ";
              printty ctx ty) )
  | TmLoc (info, _) -> error info "locations are evaluation-only!"
  | TmInert (_, ty) -> ty
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmUnit _ -> TyUnit
  | TmString _ -> TyString
  | TmFloat _ -> TyFloat
  | TmZero _ -> TyNat
  | TmWith (info, t1, t2) -> (
      let tyBase = typeof ctx t1 in
      let tyExtra = typeof ctx t2 in
      match (simplifyTy ctx tyBase, simplifyTy ctx tyExtra) with
      | TyRecord fBase, TyRecord fExtra ->
          let fDerived =
            List.append fExtra
              (List.filter
                 (fun (fName, _) -> not (List.mem_assoc fName fExtra))
                 fBase)
          in
          TyRecord fDerived
      | _ -> error info "\"with\" terms must both be records!" )

(*** Type Reconstruction ***)
(* Type constraints *)
type constr = (ty * ty) list

let emptyconstr = []

let combineConstr = List.append

type nextuvar = NextUniqTyVar of string * uvargenerator

and uvargenerator = unit -> nextuvar

let uvargen =
  let rec f n () = NextUniqTyVar ("??T" ^ string_of_int n, f (n + 1)) in
  f 0

(* Reconstructs the type of a term, yielding a tuple (ty, nextTyGenerator, constraints).
   `ty` is only valid under the `constraints`. *)
let rec tyRecon ctx nextuvar t =
  match t with
  | TmVar (info, i, _) ->
      let ty = getTermType info ctx i in
      (ty, nextuvar, [])
  | TmAbs (_, param, paramTy, body) ->
      let paramTy, nextuvar =
        match paramTy with
        | Some paramTy -> (paramTy, nextuvar)
        | None ->
            let (NextUniqTyVar (tyParamName, nextuvar)) = nextuvar () in
            (TyId tyParamName, nextuvar)
      in
      (* FIXME: return type must be shifted back down after reconstruction,
         because we evaluated in a context larger (with the parameter name)
         than we are returning it from (without the parameter name). *)
      let ctx' = addbinding ctx param (VarBinding paramTy) in
      let retTy, nextuvar, constrBody = tyRecon ctx' nextuvar body in
      (TyArrow (paramTy, retTy), nextuvar, constrBody)
  | TmApp (_, fn, body) ->
      let tyFn, nextuvar, constrFn = tyRecon ctx nextuvar fn in
      let tyBody, nextuvar, constrBody = tyRecon ctx nextuvar body in
      let (NextUniqTyVar (tyAppName, nextuvar)) = nextuvar () in
      let tyApp = TyId tyAppName in
      let constrApp = [ (tyFn, TyArrow (tyBody, tyApp)) ] in
      (tyApp, nextuvar, List.concat [ constrApp; constrFn; constrBody ])
  | TmTrue _ -> (TyBool, nextuvar, [])
  | TmFalse _ -> (TyBool, nextuvar, [])
  | TmIf (_, cond, thn, els) ->
      let tyCond, nextuvar, constrCond = tyRecon ctx nextuvar cond in
      let tyThn, nextuvar, constrThn = tyRecon ctx nextuvar thn in
      let tyEls, nextuvar, constrEls = tyRecon ctx nextuvar els in
      let constrIf = [ (tyCond, TyBool); (tyThn, tyEls) ] in
      ( tyThn,
        nextuvar,
        List.concat [ constrIf; constrCond; constrThn; constrEls ] )
  | TmZero _ -> (TyNat, nextuvar, [])
  | TmSucc (_, t1) | TmPred (_, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let constrSucc = (tyT1, TyNat) in
      (TyNat, nextuvar, constrSucc :: constrT1)
  | TmIsZero (_, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let constrSucc = (tyT1, TyNat) in
      (TyBool, nextuvar, constrSucc :: constrT1)
  | TmFix (_, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let (NextUniqTyVar (tyFixedName, nextuvar)) = nextuvar () in
      let tyFixed = TyId tyFixedName in
      let constrFix = (tyT1, TyArrow (tyFixed, tyFixed)) in
      (tyFixed, nextuvar, constrFix :: constrT1)
  | TmLet (_, name, def, inBody) ->
      (* Let polymorphism:
         If the RHS of the let binding is a value itself, it is safe to just treat
         the body of the let binding as being entirely substituted everywhere it
         is used. This is because we don't care what the type of the binding is,
         except exactly where it is used.

         So, for something like

           let twice = lambda f. lambda a. f(f(a)) in
           let three = twice succ 1 in
           let fls = twice (lambda n. n) false in
           ...

         would typecheck, because `f` and `a` are assigned fresh constraints
         (and unified to fresh types) directly at their usage sites (namely in
         the definition of `three` and `fls`).

         However, we only permit this polymorphism in the case the definition of
         the let binding is a value. If the definition is *not* a value, this
         approach may still typecheck but be unsound, for example in the case of

           let r = ref (lambda x. x) in
           (r := (lambda x. succ x); (!r)true);
            ^-- r substituted as usage site, typechecks to Nat -> Nat
                                       ^-- r substituted as usage site, typechecks to Bool -> Bool

         In this case, we cannot treat `r` polymorphically, and instead create a
         fresh type for `r` used in building the checking constraints.
      *)
      if isval ctx def then tyRecon ctx nextuvar (termSubstTop def inBody)
      else
        let tyDef, nextuvar, constrDef = tyRecon ctx nextuvar def in
        let ctx' = addbinding ctx name (VarBinding tyDef) in
        let tyInBody, nextuvar, constrInBody = tyRecon ctx' nextuvar inBody in
        (tyInBody, nextuvar, List.concat [ constrDef; constrInBody ])
  | TmAscribe (_, t1, ty) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let constrAscribe = (tyT1, ty) in
      (ty, nextuvar, constrAscribe :: constrT1)
  | TmInert (_, ty) -> (ty, nextuvar, [])
  | TmString _ -> (TyString, nextuvar, [])
  | TmFloat _ -> (TyFloat, nextuvar, [])
  | TmUnit _ -> (TyUnit, nextuvar, [])
  | TmTimesfloat (_, t1, t2) | TmPlusfloat (_, t1, t2) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let tyT2, nextuvar, constrT2 = tyRecon ctx nextuvar t2 in
      let constrFloat = [ (tyT1, TyFloat); (tyT2, TyFloat) ] in
      (TyFloat, nextuvar, List.concat [ constrFloat; constrT1; constrT2 ])
  | TmRef (_, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let (NextUniqTyVar (tyRefName, nextuvar)) = nextuvar () in
      let tyRef = TyId tyRefName in
      let constrRef = [ (tyRef, TyRef tyT1) ] in
      (tyRef, nextuvar, List.concat [ constrRef; constrT1 ])
  | TmDeref (_, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let (NextUniqTyVar (tyDerefName, nextuvar)) = nextuvar () in
      let tyDeref = TyId tyDerefName in
      let constrDeref = [ (tyT1, TyRef tyDeref) ] in
      (tyDeref, nextuvar, List.concat [ constrDeref; constrT1 ])
  | TmRefAssign (_, t1, t2) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let tyT2, nextuvar, constrT2 = tyRecon ctx nextuvar t2 in
      let constrAssign = [ (tyT1, TyRef tyT2) ] in
      (TyUnit, nextuvar, List.concat [ constrAssign; constrT1; constrT2 ])
  | TmLoc (info, _) -> error info "locations are evaluation-only!"
  (* Note: record implementation is not entirely correct!
     We do not (correctly) account for the resolution of records with fields
     extending those required by the projection, nor of records with fields not
     included in the projection (!).
     To fix this, row variables or some form of subtyping must be employed in
     `unify`.
  *)
  | TmProj (_, t1, field) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      let (NextUniqTyVar (tyProjName, nextuvar)) = nextuvar () in
      let tyProj = TyId tyProjName in
      let constrProj = [ (tyT1, TyRecord [ (field, tyProj) ]) ] in
      (tyProj, nextuvar, List.concat [ constrProj; constrT1 ])
  | TmRecord (_, fields) ->
      let fieldTys, nextuvar, constrFields =
        List.fold_right
          (fun (name, term) (fieldTys, nextuvar, constrAll) ->
            let tyField, nextuvar, constrField = tyRecon ctx nextuvar term in
            let fieldTys = (name, tyField) :: fieldTys in
            let constrAll = List.concat [ constrField; constrAll ] in
            (fieldTys, nextuvar, constrAll))
          fields ([], nextuvar, [])
      in
      (TyRecord fieldTys, nextuvar, constrFields)
  | TmVariant (_, name, t1) ->
      let tyT1, nextuvar, constrT1 = tyRecon ctx nextuvar t1 in
      (TyVariant [ (name, tyT1) ], nextuvar, constrT1)
  | TmCase (_, cond, cases) ->
      let tyCond, nextuvar, constrCond = tyRecon ctx nextuvar cond in
      let caseBodyTys, variantFields, nextuvar, constrAllBodiesInner =
        List.fold_right
          (fun (variantName, (nameInBody, body))
               (bodyTys, variantFields, nextuvar, constrAll) ->
            let (NextUniqTyVar (variantTyName, nextuvar)) = nextuvar () in
            let variantTy = TyId variantTyName in
            let variantFields = (variantName, variantTy) :: variantFields in
            let ctx' = addbinding ctx nameInBody (VarBinding variantTy) in
            let bodyTy, nextuvar, constrBody = tyRecon ctx' nextuvar body in
            let constrAll = List.concat [ constrBody; constrAll ] in
            (bodyTy :: bodyTys, variantFields, nextuvar, constrAll))
          cases ([], [], nextuvar, [])
      in
      let (NextUniqTyVar (caseRetTyName, nextuvar)) = nextuvar () in
      let caseRetTy = TyId caseRetTyName in
      let caseBodyConstrs =
        List.map (fun bodyTy -> (bodyTy, caseRetTy)) caseBodyTys
      in
      let caseCondConstrs = [ (tyCond, TyVariant variantFields) ] in
      ( caseRetTy,
        nextuvar,
        List.concat
          [ caseCondConstrs; caseBodyConstrs; constrCond; constrAllBodiesInner ]
      )
  (* TODO: implement other forms! (Not concerned with implementing `TmWith` though tbh.) *)
  | TmWith _ -> error dummyinfo "Not implemented"

(* Represents type substitutions (from unknown IDs to real types) discovered during type unification. *)
type tysubst = (string * ty) list

let substTy tyId withTy inTy =
  let rec update ty =
    match ty with
    | TyId id1 as orig -> if id1 = tyId then withTy else orig
    | TyArrow (ty1, ty2) -> TyArrow (update ty1, update ty2)
    | TyRecord fields ->
        TyRecord (List.map (fun (n, t) -> (n, update t)) fields)
    | TyVariant fields ->
        TyVariant (List.map (fun (n, t) -> (n, update t)) fields)
    | TySource t -> TySource (update t)
    | TyRef t -> TyRef (update t)
    | TyUnknown | TyNat | TyBool | TyFloat | TyString | TyUnit | TyNever
    | TyVar _ ->
        ty
  in
  update inTy

let substConstraints tyId withTy constr =
  List.map
    (fun (ty1, ty2) -> (substTy tyId withTy ty1, substTy tyId withTy ty2))
    constr

let tyNameOccursIn tyId inTy =
  let rec check ty =
    match ty with
    | TyId id1 -> id1 = tyId
    | TyArrow (ty1, ty2) -> check ty1 || check ty2
    | TyRecord fields | TyVariant fields ->
        List.exists (fun (_, t) -> check t) fields
    | TySource t | TyRef t -> check t
    | TyUnknown | TyNat | TyBool | TyFloat | TyString | TyUnit | TyNever
    | TyVar _ ->
        false
  in
  check inTy

let tryTyEq ctx s t = try tyeq ctx s t with No_typeof _ -> false

let unify info ctx msg constr =
  let rec update constr =
    match constr with
    | [] -> []
    | (s, t) :: rest when tryTyEq ctx s t -> update rest
    | (TyArrow (s1, s2), TyArrow (t1, t2)) :: rest ->
        update ((s1, t1) :: (s2, t2) :: rest)
    | (TyRef t1, TyRef t2) :: rest -> update ((t1, t2) :: rest)
    | ((TyRecord f1 as t1), (TyRecord f2 as t2)) :: rest ->
        (* The second element in the tuple is the one we identify the field
           constraints on (see case TmProj in tyRecon).

           For example, Unify (rcd, {a:Nat,b:String}) means rcd needs to contain at
           least the fields a, b.

           NOTE: this is not complete (?!), see tyRecon.
        *)
        let toUnify =
          List.map
            (fun (name, ty2) ->
              try
                let ty1 = List.assoc name f1 in
                (ty1, ty2)
              with Not_found ->
                errAt info (fun _ ->
                    print_string (msg ^ ": record ");
                    printty ctx t1;
                    print_string " missing fields from ";
                    printty ctx t2))
            f2
        in
        update (List.concat [ toUnify; rest ])
    | (TyVariant f1, TyVariant f2) :: rest ->
        (* The second element in the tuple is the one we identify the variant field
           constraints on (see case TmCase in tyRecon).

           For example, Unify (var, cmp@<a:Nat,b:String>) means the variant fields
           in var must be in cmp as well; however, clearly not all variants in cmp need
           to be in var.
        *)
        let toUnify =
          List.map
            (fun (name, ty1) ->
              try
                let ty2 = List.assoc name f2 in
                (ty1, ty2)
              with Not_found ->
                error info (msg ^ ": variant type missing variants"))
            f1
        in
        update (List.concat [ toUnify; rest ])
    (* Substitute a dummy type ID for a "real" type. *)
    | ((tyOther as t1), (TyId tyX as t2)) :: rest
    | ((TyId tyX as t1), (tyOther as t2)) :: rest ->
        if tyNameOccursIn tyX tyOther then
          errAt info (fun _ ->
              print_string (msg ^ ": circular constraints for ");
              printty ctx t1;
              print_string " and ";
              printty ctx t2)
        else
          let subst = (tyX, tyOther) in
          let updatedConstraints = substConstraints tyX tyOther rest in
          List.append (update updatedConstraints) [ subst ]
    | (t1, t2) :: _ ->
        errAt info (fun _ ->
            print_string "Cannot solve for ";
            printty ctx t1;
            print_string " and ";
            printty ctx t2)
  in
  update constr

let applyTySubsts tySubsts ty =
  List.fold_left
    (fun inTy (id, toTy) -> substTy id toTy inTy)
    ty (List.rev tySubsts)

let subst2Constr substs = List.map (fun (id, toT) -> (TyId id, toT)) substs
