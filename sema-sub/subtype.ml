open Type

let ()

(** [uninhab t] is true iff [t] is uninhabited, that is t = ⊥.
    Note that the [bdd] representation does not directly reduce to ⊥ (for example
    [int -> string \ string -> int = !(int -> string) & (string -> int)],
    which is uninhabited but not ⊥), so this method performs that calculation instead. *)
let uninhab : ty -> bool = function
  | (Pos, atoms), bdd_prod, bdd_arrow ->
      TypeAtomSet.is_empty atoms && empty_prod bdd_prod && empty_arrow bdd_arrow
  | _ -> false

(** s <: t iff s \ t = ⊥ *)
let ( <: ) s t = uninhab @@ (s // t)
