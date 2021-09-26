type mode = Sub | Sup

type ty =
  | TPrim of string
  | TTop
  | TBot
  | TArrow of ty * ty
  | TOp of mode * ty * ty  (** Sub -> A & B; Sup -> A | B *)

type split =
  | SpAnd of ty * ty * ty
  | SpArrowR of ty * ty * ty * split (* C <| B |> D *)
  | SpArrowL of ty * ty * ty * split (* B <| A |> C *)
  | SpOrL of ty * ty * ty * split
  | SpOrR of ty * ty * ty * split

type derivation =
  | ADPrim of mode * ty * ty
  | ADBound of mode * ty * ty
  | ADArrow of mode * ty * ty (* a1 -> a2 <> b1 -> b2 *) * derivation list
  | ADDual of mode * ty * ty * derivation
  | ADAnd of mode * ty * ty (* a <> b *) * split * derivation list
  | ADAndL of mode * ty * ty (* a <> b *) * split * derivation
  | ADAndR of mode * ty * ty (* a <> b *) * split * derivation

type debug_data = { steps : int; derivation_trees : derivation list }

type judgement =
  | Judgement of [ `Sub | `Sup | `Iso | `Incomp ] * ty * ty * debug_data

let should_wrap_ty (child, parent) =
  let prec = function
    | TArrow _ -> 1
    | TOp (Sup, _, _) -> 2
    | TOp (Sub, _, _) -> 3
    | _ -> failwith "unreachable"
  in
  match (child, parent) with
  | (TPrim _ | TTop | TBot), _ | _, (TPrim _ | TTop | TBot) -> false
  | TOp (m1, _, _), TOp (m2, _, _) ->
      if m1 <> m2 then true else prec child < prec parent
  | _ -> prec child < prec parent
