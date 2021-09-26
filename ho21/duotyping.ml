open Language

(** The terminal type relative to the current mode.
    Top for the Sub mode, as A <: * is always true.
    Bot for the Sup mode, as A :> ! is always true. *)
let terminal = function Sub -> TTop | Sup -> TBot

let flipmode = function Sub -> Sup | Sup -> Sub

let rec split m t =
  match (m, t) with
  (* Sp-and *)
  | m, TOp (m', a, b) when m = m' -> Some (a, b, SpAnd (a, t, b), 1)
  (* Sp-orL *)
  | m, TOp (m', a, b) when splittable m a ->
      let a1, a2, subsplit, steps = split m a |> Option.get in
      let a1b, a2b = (TOp (m', a1, b), TOp (m', a2, b)) in
      Some (a1b, a2b, SpOrL (a1b, t, a2b, subsplit), steps + 1)
  (* Sp-orR *)
  | m, TOp (m', a, b) when splittable m b ->
      let b1, b2, subsplit, steps = split m b |> Option.get in
      let ab1, ab2 = (TOp (m', a, b1), TOp (m', a, b2)) in
      Some (ab1, ab2, SpOrR (ab1, t, ab2, subsplit), steps + 1)
  (* Sp-arrowR *)
  | Sub, TArrow (a, b) when splittable Sub b ->
      let c, d, subsplit, steps = split Sub b |> Option.get in
      let a_c, a_d = (TArrow (a, c), TArrow (a, d)) in
      Some (a_c, a_d, SpArrowR (a_c, t, a_d, subsplit), steps + 1)
  (* Sp-arrowL *)
  | Sub, TArrow (a, d) when splittable Sup a ->
      let b, c, subsplit, steps = split Sup a |> Option.get in
      let b_d, c_d = (TArrow (b, d), TArrow (c, d)) in
      Some (b_d, c_d, SpArrowR (b_d, t, c_d, subsplit), steps + 1)
  | _ -> None

and splittable m t = Option.is_some (split m t)

(* Okay, yes, the construction of debug data is ugly... I might clean it up
   eventually... but probably not. *)

let ( +> ) (result, steps) addt = (result, steps + addt)

let ( &&> ) (r1, s1, d1) (r2, s2, d2) =
  if not r1 then (false, s1, []) else (r1 && r2, s1 + s2, [ d1; d2 ])

(** Duotypechecks two types, depending on the mode (subtype or supertype). *)
let rec check mode t1 t2 isdual =
  match (mode, t1, t2) with
  (* AD-int, generalized for all primitives *)
  | _, TPrim s1, TPrim s2 -> (s1 = s2, 1, ADPrim (mode, t1, t2))
  (* AD-bound *)
  | m, a, t when terminal m == t -> (true, 1, ADBound (m, a, t))
  (* AD-and *)
  | m, a, b when splittable m b ->
      let b1, b2, split, steps = split m b |> Option.get in
      let ok, steps', derivs = check m a b1 false &&> check m a b2 false in
      (ok, steps + steps' + 1, ADAnd (m, a, b, split, derivs))
  (* AD-andL, AD-andR *)
  | m, a, b when splittable m a ->
      let a1, a2, split, steps = split m a |> Option.get in
      let okL, stepsL, derivL = check m a1 b false in
      if okL then (true, steps + stepsL + 1, ADAndL (m, a, b, split, derivL))
      else
        let okR, stepsR, derivR = check m a2 b false in
        (okR, steps + stepsL + stepsR + 1, ADAndR (m, a, b, split, derivR))
  (* AD-arrow *)
  | m, TArrow (a1, a2), TArrow (b1, b2) ->
      let ok, steps, derivs =
        check (flipmode m) a1 b1 false &&> check m a2 b2 false
      in
      (ok, steps + 1, ADArrow (m, t1, t2, derivs))
  (* AD-dual *)
  | m, a, b when not isdual ->
      let ok, steps, deriv = check (flipmode m) b a true in
      (ok, steps + 1, ADDual (m, a, b, deriv))
  | _ -> (false, 1, ADBound (Sub, TTop, TBot))

let listify (r, steps, derivation) = (r, steps, [ derivation ])

let try_into judgement t1 t2 (r, steps, derivation_trees) =
  if r then Some (Judgement (judgement, t1, t2, { steps; derivation_trees }))
  else None

(** Judges if s <: t *)
let is_sub s t = check Sub s t false |> listify |> try_into `Sub s t

(** Judges if s :> t *)
let is_sup s t = check Sup s t false |> listify |> try_into `Sup s t

(** Judges if s ~= t *)
let is_iso s t =
  check Sub s t false &&> check Sup s t false |> try_into `Iso s t

(** Judges if s # t; that is, s and t are incomparable. *)
let is_incomp s t =
  let okSub, stepsSub, _ = check Sub s t false in
  if okSub then None
  else
    let okSup, stepsSup, _ = check Sup s t false in
    if okSup then None else try_into `Incomp s t (true, stepsSub + stepsSup, [])

let or_else opt f = match opt with Some r -> Some r | None -> f ()

let ( |= ) = or_else

let judge t1 t2 =
  is_iso t1 t2
  |= (fun () -> is_sub t1 t2)
  |= (fun () -> is_sup t1 t2)
  |= (fun () -> is_incomp t1 t2)
  |> Option.get
