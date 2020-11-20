open Language
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
  | TyList ty1, TyList ty2 -> tyeq ctx ty1 ty2
  | TyRef ty1, TyRef ty2 -> tyeq ctx ty1 ty2
  | _ -> false

(* Returns [ true ] iff tyS <: tyT
                            ^^ is a subtype *)
let rec subtype ctx tyS tyT =
  tyeq ctx tyS tyT
  ||
  match (tyS, tyT) with
  | _, TyTop -> true
  (* The subtyping relationship between a function is contravariant on the
     paramter and covariant on the return type.
     For example, [ (Animal) -> Cat ] is a subtype of [ (Cat) -> Animal ],
     because anywhere a Cat will be taken as a parameter that can be treated as
     an animal, and anywhere we expect to be returned an Animal we can accept a
     Cat. *)
  | TyArrow (tyS1, tyS2), TyArrow (tyT1, tyT2) ->
      subtype ctx tyT1 tyS1 && subtype tyS2 tyT2
  | TyList tyS, TyList tyT -> subtype ctx tyS tyT
  | TyRecord tyS, TyRecord tyT ->
      List.for_all
        (fun (name, tyT_i) ->
          try
            let tyS_i = List.assoc name tyS in
            subtype ctx tyS_i tyT_i
          with Not_found -> false)
        tyT
  (* <BigCat> <: <Cat|Dog>, but <Cat|Dog> <: <Cat> does not hold. *)
  | TyVariant tyS, TyRecord tyT ->
      List.for_all
        (fun (name, tyS_i) ->
          try
            let tyT_i = List.assoc name tyT in
            subtype ctx tyS_i tyT_i
          with Not_found -> false)
        tyS
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

let rec typeof ctx t =
  match t with
  | TmVar (info, i, _) -> getTermType info ctx i
  | TmAbs (_, param, paramTy, def) ->
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
          if tyeq ctx tyIn tyParam then tyOut
          else
            errAt info (fun _ ->
                pr "parameter type mismatch: found ";
                printty ctx tyIn;
                pr " expected ";
                printty ctx tyParam)
      | ty ->
          errAt info (fun _ ->
              pr "arrow type expected; got ";
              printty ctx ty) )
  | TmIf (info, cond, thn, els) ->
      if tyeq ctx (typeof ctx cond) TyBool then
        let tyThn = typeof ctx thn in
        let tyEls = typeof ctx els in
        if tyeq ctx tyThn tyEls then tyThn
        else error info "\"if\" branches have different types!"
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
                (variantName, bodyTy))
              cases
          in
          let firstCase, expectedBodyTy = List.hd caseBodyTys in
          List.iter
            (fun (name, ty) ->
              if not (tyeq ctx ty expectedBodyTy) then
                error info
                  ( name ^ " case does not match type introduced by "
                  ^ firstCase ^ "!" ))
            caseBodyTys;
          expectedBodyTy
      | ty ->
          errAt info (fun _ ->
              pr "expected variant type in case condition, got ";
              printty ctx ty;
              pr "\n") )
  | TmAscribe (info, term, ty) ->
      if tyeq ctx (typeof ctx term) ty then ty
      else error info "term and ascription type differ!"
  | TmLet (_, name, boundTerm, inTerm) ->
      let boundTy = typeof ctx boundTerm in
      let ctx' = addbinding ctx name (VarBinding boundTy) in
      (* "let name = boundTerm in inTerm" is effectively just [name->boundTerm](inTerm),
         so while "inTerm" should be checked with the context including "name",
         its resulting type is in the context outside that including "name"
         since "name" gets destroyed after the application. *)
      typeShift (-1) (typeof ctx' inTerm)
  | TmTag (info, variantName, term, variantTy) -> (
      match simplifyTy ctx variantTy with
      | TyVariant fields -> (
          try
            let expectedTy = List.assoc variantName fields in
            if tyeq ctx expectedTy (typeof ctx term) then variantTy
            else error info "variant does not have type expected by tag!"
          with Not_found ->
            error info (variantName ^ "does not exist in tag type!") )
      | _ -> error info "tag is not a variant type!" )
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
      if tyeq ctx (typeof ctx term) TyNat then TyBool
      else error info "term of iszero is not a natural number!"
  | TmSucc (info, term) | TmPred (info, term) ->
      if tyeq ctx (typeof ctx term) TyNat then TyNat
      else error info "term of natural function is not a natural number!"
  | TmTimesfloat (info, t1, t2) ->
      if tyeq ctx (typeof ctx t1) TyFloat then
        if tyeq ctx (typeof ctx t2) TyFloat then TyFloat
        else error info "second term to timesfloat is not a float!"
      else error info "first term to timesfloat is not a float!"
  | TmPlusfloat (info, t1, t2) ->
      if tyeq ctx (typeof ctx t1) TyFloat then
        if tyeq ctx (typeof ctx t2) TyFloat then TyFloat
        else error info "second term to plusfloat is not a float!"
      else error info "first term to plusfloat is not a float!"
  | TmCons (info, head, tail) -> (
      match simplifyTy ctx (typeof ctx tail) with
      | TyList elemTy as listTy ->
          if tyeq ctx elemTy (typeof ctx head) then listTy
          else error info "list type and element type differ!"
      | _ -> error info "expected second argument to cons to be a list!" )
  | TmIsNil (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyList _ -> TyBool
      | _ -> error info "expected first argument to isnil to be a list!" )
  | TmHead (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyList elemTy -> elemTy
      | _ -> error info "argument to head must be a list!" )
  | TmTail (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyList _ as listTy -> listTy
      | _ -> error info "argument to tail must be a list!" )
  | TmRef (_, term) -> TyRef (typeof ctx term)
  | TmDeref (info, term) -> (
      match simplifyTy ctx (typeof ctx term) with
      | TyRef ty -> ty
      | ty ->
          errAt info (fun _ ->
              pr "non-reference types cannot be dereferenced; found ";
              printty ctx ty) )
  | TmRefAssign (info, t1, t2) -> (
      match simplifyTy ctx (typeof ctx t1) with
      | TyRef ty ->
          if tyeq ctx ty (typeof ctx t2) then TyUnit
          else
            error info
              "value assigned to reference differs from reference type!"
      | ty ->
          errAt info (fun _ ->
              pr "non-reference types cannot be assigned via := ; found ";
              printty ctx ty) )
  | TmLoc (info, _) -> error info "locations are evaluation-only!"
  | TmNil (_, ty) -> ty
  | TmInert (_, ty) -> ty
  | TmTrue _ -> TyBool
  | TmFalse _ -> TyBool
  | TmUnit _ -> TyUnit
  | TmString _ -> TyString
  | TmFloat _ -> TyFloat
  | TmZero _ -> TyNat
