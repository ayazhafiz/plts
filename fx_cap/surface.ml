type lineco = int * int
(** line * column *)

type loc = lineco * lineco
(** start * end *)

type hover_info = {
  range : loc;
  md_docs : string list;
      (** Regions of markdown documentation.
          Each region should roughly correspond to a paragraph. *)
}
