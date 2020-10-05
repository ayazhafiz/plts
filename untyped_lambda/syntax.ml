(** A lambda term, using de Bruijn indeces. *)
type term = TmVar of int | TmAbs of term | TmApp of term * term
