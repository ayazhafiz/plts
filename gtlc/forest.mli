module type Elt = sig
  type t
end

module type F = sig
  type elt

  type t

  val eq : t -> t -> bool

  val create_node : elt -> t

  val value : t -> elt

  val find : t -> t

  val union : t -> t -> unit

  val union_ordered : t -> t -> bool -> unit
  (** [union_ordered u v f] is like [union u v], but if [f = true] [u] becomes
      the representative of the merged class. *)
end

module Make (E : Elt) : F with type elt = E.t
