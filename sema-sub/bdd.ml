open Type_atom

type 'a node = { atom : 'a; left : 'a bdd; middle : 'a bdd; right : 'a bdd }

(** Binary Decision Diagram.
    Encode DNF product or function types, that is a disjunction of
    conjunctions of polar type atoms, where the atoms are either functions or
    products.
    A node in the BDD is a type atom.
    A leaf in the BDD is either True ([Btop]) or False ([BBot])<++>, representing
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

type prod_bdd = [ `Prod of type_atom * type_atom ] bdd
type arrow_bdd = [ `Arrow of type_atom * type_atom ] bdd

let rec union b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> b1
  | BTop, _ | _, BTop -> BTop
  | BBot, b | b, BBot -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } )
    when a1 = a2 ->
      BNode { atom = a1; left = l1 |. l2; middle = m1 |. m2; right = r1 |. r2 }
  | BNode n1, BNode n2 ->
      let { atom; left; middle; right }, b' =
        if n1.atom < n2.atom then (n1, b2) else (n2, b1)
      in
      BNode { atom; left; middle = middle |. b'; right }

and ( |. ) a b = union a b

let create atom left middle right =
  if middle == BTop then BTop
  else if left == right then
    if left == middle then (* all are the same, no need for a new node *)
      left
    else (* left = right, middle is different, union them *)
      union left middle
  else BNode { atom; left; middle; right }

let rec inter b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> b1
  | BBot, _ | _, BBot -> BBot
  | BTop, b | b, BTop -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } )
    when a1 = a2 ->
      (* must materialize the middle unions in the respective types at this point *)
      let l1, r1, l2, r2 = (l1 |. m1, r1 |. m1, l2 |. m2, r2 |. m2) in
      BNode { atom = a1; left = l1 &. l2; middle = BBot; right = r1 &. r2 }
  | BNode n1, BNode n2 ->
      let { atom; left; middle; right }, b' =
        if n1.atom < n2.atom then (n1, b2) else (n2, b1)
      in
      BNode
        { atom; left = left &. b'; middle = middle &. b'; right = right &. b' }

and ( &. ) a b = union a b

let rec not = function
  | BTop -> BBot
  | BBot -> BTop
  | BNode { atom; left; middle; right = BBot } ->
      BNode { atom; left = BBot; middle = ~.(middle |. left); right = ~.middle }
  | BNode { atom; left = BBot; middle; right } ->
      BNode
        { atom; left = ~.middle; middle = ~.(middle |. right); right = BBot }
  | BNode { atom; left; middle = BBot; right } ->
      BNode { atom; left = ~.left; middle = ~.(left |. right); right = ~.right }
  | BNode { atom; left; middle; right } ->
      BNode
        {
          atom;
          left = ~.(left |. middle);
          middle = BBot;
          right = ~.(right |. middle);
        }

and ( ~. ) b = not b

let rec diff b1 b2 =
  match (b1, b2) with
  | b1, b2 when b1 = b2 -> BBot
  | _, BTop -> BBot
  | BBot, _ -> BBot
  | BTop, b -> ~.b
  | b, BBot -> b
  | ( BNode { atom = a1; left = l1; middle = m1; right = r1 },
      BNode { atom = a2; left = l2; middle = m2; right = r2 } ) ->
      if a1 < a2 then
        BNode
          {
            atom = a1;
            left = (l1 |. m1) /. b2;
            middle = BBot;
            right = (r1 |. m1) /. b2;
          }
      else if a2 < a1 then
        BNode
          {
            atom = a2;
            left = b1 /. (l2 |. m2);
            middle = BBot;
            right = b1 /. (r2 |. m2);
          }
      else
        BNode
          {
            atom = a1;
            left = (l1 |. m1) /. (l2 |. m2);
            middle = BBot;
            right = (r1 |. m1) /. (r2 |. m2);
          }

and ( /. ) a b = diff a b
