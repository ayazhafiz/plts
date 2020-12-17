open Language
open Util.Error
open Util

let mbComputeSingleTyStep ctx ty =
  match ty with
  | TyRec (_, tyS1) as tyS -> Some (typeSubstTop tyS tyS1)
  | TyVar (idx, _) -> getBoundNameOfTypeVar dummyinfo ctx idx
  | _ -> None

let rec simplifyTy ctx ty =
  match mbComputeSingleTyStep ctx ty with
  | Some ty' -> simplifyTy ctx ty'
  | None -> ty

let tyeq ctx ty1 ty2 =
  let rec tyeq seen ctx ty1 ty2 =
    (* If we have already seen a pair of recursive types in lowering them, they are
       surely equal. For example, in typechecking
         Rec X. <nil:Unit, cons:{Nat,X}> ?eq <nil:Unit, const:{Nat, Rec X.  <nil:Unit, cons:{Nat,X}>}>
       we will eventually directly compare the two recursive types, and then "go
       in deeper", but all that matters is that we saw their recursive form once.
       If everything else matches during the lowering process, we already know
       their recursive form is the same.
    *)
    List.mem (ty1, ty2) seen
    ||
    match (ty1, ty2) with
    | TyRec (_, ty1'), _ ->
        tyeq ((ty1, ty2) :: seen) ctx (typeSubstTop ty1 ty1') ty2
    | _, TyRec (_, ty2') ->
        tyeq ((ty1, ty2) :: seen) ctx ty1 (typeSubstTop ty2 ty2')
    | _ -> (
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
            tyeq seen ctx t11 t21 && tyeq seen ctx t12 t22
        | TyId name1, TyId name2 -> name1 = name2
        | TyRecord fields1, TyRecord fields2 ->
            List.length fields1 = List.length fields2
            && List.for_all
                 (fun (name, ty1) ->
                   try
                     let ty2 = List.assoc name fields2 in
                     tyeq seen ctx ty1 ty2
                   with Not_found -> false)
                 fields1
        | TyVariant fields1, TyVariant fields2 ->
            List.length fields1 = List.length fields2
            && List.for_all2
                 (fun (_, ty1) (_, ty2) -> tyeq seen ctx ty1 ty2)
                 fields1 fields2
        | TySource ty1, TySource ty2 -> tyeq seen ctx ty1 ty2
        | TyRef ty1, TyRef ty2 -> tyeq seen ctx ty1 ty2
        | _ -> false )
  in
  tyeq [] ctx ty1 ty2

(* Returns [ true ] iff tyS <: tyT
                            ^^ is a subtype *)
let subtype ctx tyS tyT =
  let rec subtype seen ctx tyS tyT =
    List.mem (tyS, tyT) seen
    || tyeq ctx tyS tyT
    ||
    match (tyS, tyT) with
    | TyRec (_, tyS'), _ ->
        subtype ((tyS, tyT) :: seen) ctx (typeSubstTop tyS tyS') tyT
    | _, TyRec (_, tyT') ->
        subtype ((tyS, tyT) :: seen) ctx tyS (typeSubstTop tyT tyT')
    | _ -> (
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
            subtype seen ctx tyT1 tyS1 && subtype seen ctx tyS2 tyT2
        | TyRecord tyS, TyRecord tyT ->
            List.for_all
              (fun (name, tyT_i) ->
                try
                  let tyS_i = List.assoc name tyS in
                  subtype seen ctx tyS_i tyT_i
                with Not_found -> false)
              tyT
        (* <BigCat> <: <Cat|Dog>, but <Cat|Dog> <: <Cat> does not hold. *)
        | TyVariant tyS, TyVariant tyT ->
            List.for_all
              (fun (name, tyS_i) ->
                try
                  let tyT_i = List.assoc name tyT in
                  subtype seen ctx tyS_i tyT_i
                with Not_found -> false)
              tyS
        (* Source types are covariant. That is, I can read a Source BigCat wherever I
           can read a Source Cat (Source BigCat <: Source Cat)*)
        | TySource tyS, TySource tyT -> subtype seen ctx tyS tyT
        (* Ref BigCat <: Source Cat *)
        | TyRef tyS, TySource tyT -> subtype seen ctx tyS tyT
        (* Reference types are invariant; that is, their parameters must be subtypes
           of each other.
           For example, if I have [ c = ref {Cat} : Ref Cat ], [ !c ] implies Ref Cat
             <: Ref Animal (anywhere I read a Cat, I can read it as an Animal).
           But also, in places I expect to write to a reference, it suffices to pass a
           reference to a supertype of Cat, because anywhere I write a Cat, I can
           write to a supertype of it. That is, [ c := {Cat} ] can be substituted with
           [ a := {Cat} ] where [ a : Ref Animal ]. Thus Ref Animal <: Ref Cat. *)
        | TyRef tyS, TyRef tyT ->
            subtype seen ctx tyS tyT && subtype seen ctx tyT tyS
        | TyRec (_, tyS'), _ ->
            subtype ((tyS, tyT) :: seen) ctx (typeSubstTop tyS tyS') tyT
        | _, TyRec (_, tyT') ->
            subtype ((tyS, tyT) :: seen) ctx tyS (typeSubstTop tyT tyT')
        | _, _ -> false )
  in
  subtype [] ctx tyS tyT

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
