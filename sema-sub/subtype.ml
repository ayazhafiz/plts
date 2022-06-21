open Type

(** Decides if a product bdd is uninhabited.
    See section 4.2 and exercrise EX6 of Cas22. *)
let rec empty_prod ty =
  (* Φ of EX6 Cas22 *)
  let rec phi s1 s2 n =
    match n with
    | [] -> empty s1 || empty s2
    | (t1, t2) :: n ->
        (s1 <: t1 || phi (s1 /~ t1) s2 n) && (s2 <: t2 || phi s1 (s2 /~ t2) n)
  in
  (* https://pnwamk.github.io/sst-tutorial/#%28part._sec~3aprod-inhabitation%29 *)
  let rec walk ty s1 s2 n =
    match ty with
    | BBot -> true
    | BTop ->
        (* This type (s1, s2) is included, so call phi with the negated types *)
        phi s1 s2 n
    | BNode { atom = `Prod (t1, t2); left; middle; right } ->
        (* Walking down the left, we include the atom in the intersection *)
        walk left (s1 &~ t1) (s2 &~ t2) n
        (* The middle ignores the atom, it's not realized yet *)
        && walk middle s1 s2 n
        (* Walking down the right, we negate the atom in the intersection,
           so add it to the neg set *)
        && walk right s1 s2 ((t1, t2) :: n)
  in
  walk ty top top []

(** Decides if an arrow bdd is uninhabited.
    See section 4.2, and especially page 31-33, of Cas22. *)
and empty_arrow ty =
  (* Φ' of Cas22 page 33 *)
  let rec phi' t1 t2 p =
    match p with
    | [] -> empty t1 || empty t2
    | (s1, s2) :: p ->
        (* TODO: perhaps try the optimization of https://pnwamk.github.io/sst-tutorial/#%28part._sec~3aarrow-inhabitation%29 instead? *)
        (* s2_p =      ∧      S2
                  S1->S2 ∈ P
        *)
        let s2_p () = List.fold_left (fun t (_, s2) -> t &~ s2) top p in
        (t1 <: s1 || s2_p () <: ~~t2)
        && phi' t1 (t2 &~ s2) p
        && phi' (t1 /~ s1) t2 p
  in
  (* https://pnwamk.github.io/sst-tutorial/#%28part._sec~3aarrow-inhabitation%29 *)
  let rec walk ty dom p n =
    match (ty, dom, p, n) with
    | BBot, _, _, _ -> true
    | BTop, _, _, [] -> false
    | BTop, dom, p, (t1, t2) :: n ->
        (t1 <: dom && phi' t1 ~~t2 p) || walk BTop dom p n
    | BNode { atom = `Arrow (s_dom, s_cod); left; middle; right }, dom, p, n ->
        walk left (dom |~ s_dom) ((s_dom, s_cod) :: p) n
        && walk middle dom p n
        && walk right dom p ((s_dom, s_cod) :: n)
  in
  walk ty bot [] []

(** [uninhab t] is true iff [t] is uninhabited, that is t = ⊥.
        Note that the [bdd] representation does not directly reduce to ⊥ (for example
        [int -> string \ string -> int = !(int -> string) & (string -> int)],
        which is uninhabited but not ⊥), so this method performs that calculation instead. *)
and empty : ty -> bool = function
  | (Pos, atoms), bdd_prod, bdd_arrow ->
      TypeAtomSet.is_empty atoms && empty_prod bdd_prod && empty_arrow bdd_arrow
  | _ -> false

(** s <: t iff s \ t = ⊥ *)
and ( <: ) (s : ty) (t : ty) : bool = empty @@ (s /~ t)

(** s </: t iff s \ t != ⊥ *)
let ( </: ) s t = not (s <: t)

let%test_module "subtype" =
  (module struct
    let ( <: ) s t = Result.get_ok @@ parse_ty s <: Result.get_ok @@ parse_ty t
    let ( </: ) s t = not @@ (s <: t)

    (* Tests taken from
       https://github.com/pnwamk/sst-tutorial/blob/c7ce1a35e3ba688ee275144941e729ff8b3b0c40/model/testing.rkt#L137-L287,
       licensed as Apache 2.0, copyright pnwamk.
       Thank you pnwank! *)
    let%test _ = "never" <: "any"
    let%test _ = "any" <: "!never"
    let%test _ = "!never" <: "any"
    let%test _ = "any" </: "never"
    let%test _ = "!never" </: "never"
    let%test _ = "int" <: "int"
    let%test _ = "int" <: "any"
    let%test _ = "string" <: "any"
    let%test _ = "any" </: "int"
    let%test _ = "any" </: "string"
    let%test _ = "int" </: "string"
    let%test _ = "int" <: "int|string"
    let%test _ = "int|string" <: "int|string"
    let%test _ = "int|string" </: "int"
    let%test _ = "!(int|string)" <: "!int"
    let%test _ = "!int" </: "!(int|string)"
    let%test _ = "int" </: "int&string"
    let%test _ = "int&string" <: "never"
    let%test _ = "int&string" <: "int"
    let%test _ = "int&string" <: "string"
    let%test _ = "(int, int)" <: "(int, int)"
    let%test _ = "(int, int)" </: "(int, string)"
    let%test _ = "(int, any)" </: "(int, int)"
    let%test _ = "(int, any)" </: "(never, int)"
    let%test _ = "(int, int)" <: "(int, any)"
    let%test _ = "(int, int)" <: "(any, int)"
    let%test _ = "(never, string)" <: "(int, int)"
    let%test _ = "(string, never)" <: "(int, int)"
    let%test _ = "(string, never)" <: "(int, never)"
    let%test _ = "(string, never)" <: "(never, int)"
    let%test _ = "(int, any)|(any, int)" </: "(int, int)"
    let%test _ = "(int, int)" <: "(int, any)|(any, int)"
    let%test _ = "(int, any)&(any, int)" <: "(int, int)"
    let%test _ = "(int, int)" <: "(int, any)&(any, int)"

    let%test _ =
      "(true|int, false|int)&(true|false, false|true)" <: "(true, false)"

    let%test _ =
      "(true, false)" <: "(true|int, false|int)&(true|false, false|true)"

    let%test _ =
      "(true|int, true|false)&(true|false, false|true)" </: "(true, false)"

    let%test _ =
      "(true|false, false|int)&(true|false, false|true)" </: "(true, false)"

    let%test _ =
      "(true, false)" <: "(true|int, true|false)&(true|false, false|true)"

    let%test _ =
      "(true, false)" <: "(true|false, false|int)&(true|false, false|true)"

    let%test _ =
      "(true|false, true|false)&(!(true, true) & !(false, false))"
      <: "(true, false) | (false, true)"

    let%test _ =
      "(true, false) | (false, true)"
      <: "(true|false, true|false) & ( !(true, true) & !(false, false) )"

    let%test _ = "int -> int" <: "int -> int"
    let%test _ = "any -> any" <: "never -> any"
    let%test _ = "any -> any" <: "never -> never"
    let%test _ = "never -> any" </: "any -> any"
    let%test _ = "int -> int" <: "int -> any"
    let%test _ = "any -> int" <: "int -> int"
    let%test _ = "int -> int" </: "any -> int"
    let%test _ = "int -> any" </: "int -> int"
    let%test _ = "(int -> int) & (true -> false)" <: "int -> int"
    let%test _ = "(int -> int) & (true -> false)" <: "true -> false"
    let%test _ = "(int -> int)" </: "(int -> int) & (true -> false)"
    let%test _ = "(true -> false)" </: "(int -> int) & (true -> false)"
    let%test _ = "(int -> int) & (true -> false)" <: "(true|int) -> (false|int)"
    let%test _ = "(int -> true) & (!int -> false)" <: "any -> true|false"
    let%test _ = "true|int -> false|int" </: "(int -> int) & (true -> false)"
    let%test _ = "(true|int -> int) | (false|int -> int)" <: "int -> int"

    let%test _ =
      "(true|int -> int) | (false|int -> false)" <: "int -> false|int"

    let%test _ = "(true|false) & !true" <: "false"
    let%test _ = "false" <: "!true & (true|false)"
    let%test _ = "false" <: "(true|false) & !true"

    let%test _ =
      {|((true, true) | (true, false) | (false, true) | (false, false)) & !(true, any)|}
      <: "(false, true) | (false, false)"

    let%test _ =
      {|((true, true) | (true, false) | (false, true) | (false, false)) & (!true, int|true|false)|}
      <: "(false, true)|(false, false)"

    let%test _ =
      "(false, true) | (false, false)"
      <: {|(true, true) | (true, false) | (false, true) | (false, false)|}

    let%test _ =
      "(false, true) | (false, false)"
      <: {|((true, true) | (true, false) | (false, true) | (false, false)) & (!true, int|true|false)|}
  end)
