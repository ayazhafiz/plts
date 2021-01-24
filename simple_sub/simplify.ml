open Language

type compact_ty = {
  vars : ty list;
  prims : ty list;
  rcd : (string, compact_ty) list option;
  fn : (compact_ty, compact_ty) option;
}
(** Describes a union or intersection with different type components. *)

let isEmpty { vars; prims; rcd; fn } =
  List.length vars = 0
  && List.length prims = 0
  && Option.is_none rcd && Option.is_none fn

let empty_compact_ty = { vars = []; prims = []; rcd = None; fn = None }

let merge_opts f = function
  | Some l, Some r -> Some (f l r)
  | (Some _ as l), _ -> l
  | (_, Some _) as r -> r
  | _ -> None

let sort_s s =
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq
  |> String.of_seq

(*
val rec = mergeOptions(lhs.rec, rhs.rec) { (lhs, rhs) =>
  if (pol) lhs.flatMap { case (k, v) => rhs.get(k).map(k -> merge(pol)(v, _)) }
  else mergeSortedMap(lhs, rhs)(merge(pol)) }
val fun = mergeOptions(lhs.fun, rhs.fun){
  case ((l0,r0), (l1,r1)) => (merge(!pol)(l0, l1), merge(pol)(r0, r1)) }
CompactType(lhs.vars ++ rhs.vars, lhs.prims ++ rhs.prims, rec, fun)
*)
let rec merge isPos lhs rhs =
  let rcd =
    merge_opts
      (fun (fL, fR) ->
        if isPos then
          List.filter_map (fun (f, tyL) ->
              Option.map (fun tyR -> merge isPos tyL tyR) (List.assoc_opt f fR))
        else
          List.sort
            (fun ((f1, _), (f2, _)) -> String.compare f1 f2)
            (List.concat fL fR))
      (lhs.rcd, rhs.rcd)
  in
  rcd
