open Type_atom
open Bdd

type polarity = Pos | Neg

module TypeAtomSet = Set.Make (struct
    type t = type_atom

    let compare = compare
  end)

type base_type = polarity * TypeAtomSet.t

type t = base_type * prod_bdd * arrow_bdd
(** DNF of a type, expressed as disjunction of
    (base type DNF
    ,product type BDD
    ,arrow type BDD) *)

(** ⊤ *)
let top : t = ((Neg, TypeAtomSet.empty), BTop, BTop)

(** ⊥ *)
let bot : t = ((Pos, TypeAtomSet.empty), BBot, BBot)

let ty_to_bdd: Syntax.ty -> t = let open Syntax in
  let atomize t: t = ((Pos, TypeAtomSet.singleton t), BBot, BBot) in
  let base_bot: base_type = (Pos, TypeAtomSet.empty) in
  let rec go (_, t) = match t with
    | TInt -> atomize Int
    | TString -> atomize (Type_atom.String) 
    | TAny -> top
    | TNever -> bot
    | TProd (t1, t2) -> (base_bot, )
