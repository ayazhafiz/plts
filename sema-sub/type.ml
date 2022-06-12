type type_atom = Int | String
type polarity = Pos | Neg

module TypeAtomSet = Set.Make (struct
  type t = type_atom

  let compare = compare
end)

type base_type = polarity * TypeAtomSet.t

let union_base b1 b2 =
  let open TypeAtomSet in
  match (b1, b2) with
  (* {t1} ∨ {t2} = {t1 ∨ t2} *)
  | (Pos, b1), (Pos, b2) -> (Pos, union b1 b2)
  (* ¬{t1} ∨ ¬{t2} = ¬{t1 ∧ t2} (DeMorgan) *)
  | (Neg, b1), (Neg, b2) -> (Neg, inter b1 b2)
  (* ¬{t1} ∨ {t2} = ¬{t1 \ t2} *)
  | (Neg, b1), (Pos, b2) | (Pos, b2), (Neg, b1) -> (Neg, diff b1 b2)

let inter_base b1 b2 =
  let open TypeAtomSet in
  match (b1, b2) with
  (* {t1} ∧ {t2} = {t1 ∧ t2} *)
  | (Pos, b1), (Pos, b2) -> (Pos, inter b1 b2)
  (* ¬{t1} ∧ ¬{t2} = ¬{t1 ∨ t2} (DeMorgan) *)
  | (Neg, b1), (Neg, b2) -> (Neg, union b1 b2)
  (* {t1} ∧ ¬{t2} = {t1 \ t2} *)
  | (Pos, b1), (Neg, b2) | (Neg, b2), (Pos, b1) -> (Pos, diff b1 b2)

let diff_base b1 b2 =
  let open TypeAtomSet in
  match (b1, b2) with
  (* {t1} \ {t2} = {t1 \ t2} *)
  | (Pos, b1), (Pos, b2) -> (Pos, diff b1 b2)
  (* ¬{t1} \ ¬{t2} = {t2 \ t1} *)
  | (Neg, b1), (Neg, b2) -> (Pos, diff b2 b1)
  (* {t1} \ ¬{t2} = {t1 ∧ t2} *)
  | (Pos, b1), (Neg, b2) -> (Pos, inter b1 b2)
  (* ¬{t1} \ {t2} = ¬{t1 ∨ t2} *)
  | (Neg, b1), (Pos, b2) -> (Neg, union b1 b2)

type ty = base_type * prod_bdd * arrow_bdd
(** DNF of a type, expressed as disjunction of
    (base type DNF
    ,product type BDD
    ,arrow type BDD) *)

and prod_bdd = [ `Prod of ty * ty ] bdd
and arrow_bdd = [ `Arrow of ty * ty ] bdd

(** Binary Decision Diagram.
    Encode DNF product or function types, that is a disjunction of
    conjunctions of polar type atoms, where the atoms are either functions or
    products.
    A node in the BDD is a type atom.
    A leaf in the BDD is either True ([Btop]) or False ([BBot]), representing
    whether the conjuction of atoms along the path to that leaf are a disjunctive
    clause in the encoded DNF. Said another way, if a bdd represents type [T] and
    [b] is the leaf a path through type atoms [a1, ..., an] in the bdd, [b]
    answers [a1 ∧ ... ∧ an <: T].
    When a path goes left from a node [t], [t] is included positively in the
    conjunction; when it goes right it is included negatively. When it goes
    through the middle, [t] is not included at all.

    If we have BDD

    {[
                        t
        b =          __/|\__
             b_left /   |   \ b_right
                      b_mid
    ]}

    then [b = (t ∧ b_left) ∨ b_mid ∨ (¬t ∧ b_right)]

    BDDs are canonical. Hence, ordering of atoms matters, we just use OCaml's
    {!compare}.

    The middle is used to encode unions efficiently (lazily). When {union}ing
    two bdds b1 b2 where b1.atom < b2.atom, we avoid materializing the union
    fully to the left and right subtrees and instead put b2 in the middle. Only
    when it comes time to look at unions or differences, which may impact the
    disjunction, do we materialize the union.
*)
and 'a bdd =
  | BTop  (** True *)
  | BBot  (** False *)
  | BNode of 'a node  (** A node *)

and 'a node = { atom : 'a; left : 'a bdd; middle : 'a bdd; right : 'a bdd }

let rec create_bdd atom left middle right =
  if middle == BTop then BTop
  else if left == right then
    if left == middle then (* all are the same, no need for a new node *)
      left
    else
      (* left = right, middle is different, union them *)
      union_bdd left middle
  else BNode { atom; left; middle; right }

and union_bdd b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> b1
  | BTop, _ | _, BTop -> BTop
  | BBot, b | b, BBot -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } ) ->
      if a1 < a2 then create_bdd a1 l1 (m1 |. b2) r1
      else if a2 < a1 then create_bdd a2 l2 (b1 |. m2) r2
      else create_bdd a1 (l1 |. l2) (m1 |. m2) (r1 |. r2)

and ( |. ) a b = union_bdd a b

let rec inter_bdd b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> b1
  | BBot, _ | _, BBot -> BBot
  | BTop, b | b, BTop -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } ) ->
      if a1 < a2 then create_bdd a1 (l1 &. b2) (m1 &. b2) (r1 &. b2)
      else if a2 < a1 then create_bdd a2 (b1 &. l2) (b1 &. m2) (b1 &. r2)
      else
        (* must materialize the middle unions in the respective types at this point *)
        let l1, r1, l2, r2 = (l1 |. m1, r1 |. m1, l2 |. m2, r2 |. m2) in
        create_bdd a1 (l1 &. l2) BBot (r1 &. r2)

and ( &. ) a b = inter_bdd a b

let rec neg_bdd = function
  | BTop -> BBot
  | BBot -> BTop
  | BNode { atom; left; middle; right = BBot } ->
      create_bdd atom BBot ~.(left |. middle) ~.middle
  | BNode { atom; left = BBot; middle; right } ->
      create_bdd atom ~.middle ~.(right |. middle) BBot
  | BNode { atom; left; middle = BBot; right } ->
      create_bdd atom ~.left ~.(left |. right) ~.right
  | BNode { atom; left; middle; right } ->
      create_bdd atom ~.(left |. middle) BBot ~.(right |. middle)

and ( ~. ) b = neg_bdd b

let rec diff_bdd b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> BBot
  | _, BTop -> BBot
  | BBot, _ -> BBot
  | BTop, b -> ~.b
  | b, BBot -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } ) ->
      if a1 < a2 then create_bdd a1 ((l1 |. m1) /. b2) BBot ((r1 |. m1) /. b2)
      else if a2 < a1 then
        create_bdd a2 (b1 /. (l2 |. m2)) BBot (b1 /. (r2 |. m2))
      else
        create_bdd a1 ((l1 |. m1) /. (l2 |. m2)) BBot ((r1 |. m1) /. (r2 |. m2))

and ( /. ) a b = diff_bdd a b

let union (b1, bp1, ba1) (b2, bp2, ba2) =
  (union_base b1 b2, bp1 |. bp2, ba1 |. ba2)

let ( || ) = union

let inter (b1, bp1, ba1) (b2, bp2, ba2) =
  (inter_base b1 b2, bp1 &. bp2, ba1 &. ba2)

let ( && ) = inter

let diff (b1, bp1, ba1) (b2, bp2, ba2) =
  (diff_base b1 b2, bp1 /. bp2, ba1 /. ba2)

let ( // ) = diff

(** ⊤ *)
let top : ty = ((Neg, TypeAtomSet.empty), BTop, BTop)

let neg t = top // t
let ( ~~ ) = neg

(** ⊥ *)
let bot : ty = ((Pos, TypeAtomSet.empty), BBot, BBot)

let ty_of_syn : Syntax.loc_ty -> ty =
  let atomize t : ty = ((Pos, TypeAtomSet.singleton t), BBot, BBot) in
  let base_bot : base_type = (Pos, TypeAtomSet.empty) in
  let open Syntax in
  let rec go (_, t) =
    match t with
    | TInt -> atomize Int
    | TString -> atomize String
    | TAny -> top
    | TNever -> bot
    | TArrow (t1, t2) ->
        let t1, t2 = (go t1, go t2) in
        let arrow_bdd = create_bdd (`Arrow (t1, t2)) BTop BBot BBot in
        (base_bot, BBot, arrow_bdd)
    | TProd (t1, t2) ->
        let t1, t2 = (go t1, go t2) in
        let prod_bdd = create_bdd (`Prod (t1, t2)) BTop BBot BBot in
        (base_bot, prod_bdd, BBot)
    | TOr (t1, t2) -> go t1 || go t2
    | TAnd (t1, t2) -> go t1 && go t2
    | TNot t -> ~~(go t)
  in
  go

type 'a bdd_ctx = { trans_atom : 'a -> Syntax.loc_ty; any : Syntax.loc_ty }

let rec syn_of_ty : ty -> Syntax.loc_ty =
  let open Syntax in
  let trans_base_atom = function Int -> zero TInt | String -> zero TString in
  let trans_base_type (pol, atoms) =
    let atoms =
      List.map trans_base_atom @@ List.of_seq @@ TypeAtomSet.to_seq atoms
    in
    match (pol, atoms) with
    | Pos, [] -> zero TNever
    | Neg, [] -> zero TAny
    | pol, atoms -> (
        let atoms =
          List.fold_left
            (fun t u -> zero @@ TOr (t, u))
            (List.hd atoms) (List.tl atoms)
        in
        match pol with Pos -> atoms | Neg -> zero @@ TNot atoms)
  in
  let rec trans_bdd bdd_ctx d = function
    | BBot -> zero TNever
    | BTop ->
        (* at the toplevel, we must render the top type as the corresponding bdd top type.
           that is, we want to show (any -> any) rather than any to the user, for
           correctness.
           but deeper, BTop/BBot hold a different meaning, and truly do correspond
           to any/never. *)
        if d = 0 then bdd_ctx.any else zero TAny
    | BNode { atom; left; middle; right } ->
        (* t = (atom ∧ left) ∨ mid ∨ (¬atom ∧ right) *)
        let atom = bdd_ctx.trans_atom atom in
        let d' = d + 1 in
        let left, mid, right =
          ( trans_bdd bdd_ctx d' left,
            trans_bdd bdd_ctx d' middle,
            trans_bdd bdd_ctx d' right )
        in
        let left_part = TAnd (atom, left) in
        let right_part = TAnd (zero (TNot atom), right) in
        zero @@ TOr (zero left_part, zero @@ TOr (mid, zero right_part))
  in
  let prod_ctx =
    {
      trans_atom =
        (fun (`Prod (t1, t2)) -> zero @@ TProd (syn_of_ty t1, syn_of_ty t2));
      any = zero @@ TProd (zero TAny, zero TAny);
    }
  in
  let arrow_ctx =
    {
      trans_atom =
        (fun (`Arrow (t1, t2)) -> zero @@ TArrow (syn_of_ty t1, syn_of_ty t2));
      any = zero @@ TArrow (zero TAny, zero TAny);
    }
  in
  function
  | (Neg, atoms), BTop, BTop when TypeAtomSet.is_empty atoms -> zero @@ TAny
  | (Pos, atoms), BBot, BBot when TypeAtomSet.is_empty atoms -> zero @@ TNever
  | base, bdd_prod, bdd_arrow ->
      let base = trans_base_type base in
      let prods, arrows =
        (trans_bdd prod_ctx 0 bdd_prod, trans_bdd arrow_ctx 0 bdd_arrow)
      in
      let t = zero @@ TOr (base, zero @@ TOr (prods, arrows)) in
      simpl_ty t

let%expect_test "dnf_norm" =
  let can_ty s =
    let t = Result.get_ok @@ Syntax_help.parse s in
    let t = syn_of_ty @@ ty_of_syn t in
    Syntax.string_of_ty t
  in
  let check_reparse s =
    let can_s = can_ty s in
    if can_s <> s then (* failwith ("Differs: can " ^ can_s ^ " vs " ^ s) *) ()
  in
  let cases =
    [
      "int";
      "int | string";
      "(int, int)";
      "string -> int";
      "(int | string, string)";
      "(int, string) | (string, string)";
      "int | any";
      "!int|any";
      "!any";
      "!(int|any)";
      "!(int&any)";
      "int & (any | (int, int))";
      "(int | (int, int), any)";
      "(any & !int, any)";
      "(int, int)&int | int&any";
      "(((int, int) & !int, (int, int) & !int), ((int, int) & !int, (int, int) \
       & !int))";
    ]
  in
  let results = List.map can_ty cases in
  List.iter check_reparse results;
  print_string @@ String.concat "\n"
  @@ List.map2 (fun c r -> if c = r then c else c ^ " => " ^ r) cases results;
  [%expect
    {|
      int
      int | string
      (int, int)
      string -> int
      (int | string, string)
      (int, string) | (string, string)
      int | any => any
      !int|any => any
      !any => never
      !(int|any) => never
      !(int&any) => !int | (any, any) | (any -> any)
      int & (any | (int, int)) => int
      (int | (int, int), any)
      (any & !int, any) => (!int | (any, any) | (any -> any), any)
      (int, int)&int | int&any => int
      (((int, int) & !int, (int, int) & !int), ((int, int) & !int, (int, int) & !int)) => (((int, int), (int, int)), ((int, int), (int, int))) |}]
