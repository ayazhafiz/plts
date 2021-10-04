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
end

module Make (E : Elt) = struct
  type elt = E.t

  type t = { mutable rank : int; mutable parent : t; mutable elt : elt }

  let eq = ( == )

  let create_node elt =
    let rec node = { rank = 0; parent = node; elt } in
    node

  let value { elt; _ } = elt

  let rec find node =
    if node.parent != node then node.parent <- find node.parent;
    node.parent

  let union u v =
    if u != v then
      let root_m = find u in
      let root_n = find v in
      if root_m.rank > root_n.rank then root_n.parent <- root_m
      else if root_n.rank > root_m.rank then root_m.parent <- root_n
      else (
        root_m.parent <- root_n;
        root_n.rank <- root_n.rank + 1)

  let union_ordered u v f =
    union u v;
    if f then (
      let root = find u in
      let new_u_elt = root.elt in
      root.elt <- u.elt;
      u.elt <- new_u_elt)
end
